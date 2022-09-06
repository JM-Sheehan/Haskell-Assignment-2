{-# LANGUAGE OverloadedStrings #-}

module HtmlReport where

import Data.Foldable (traverse_)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (src)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Colonnade
import Text.Blaze.Colonnade
import Fmt (pretty, Buildable)

import CovidData
import StatReport
    ( StatEntry(qfield, meanVal, minVal, maxVal, daysBetweenMinMax), AverageValue (tenDayMax, sevenDayAverage, AverageValue) )

viaFmt :: Buildable a => a -> Html
viaFmt = text . pretty


---- Columns for the necesarry average values ----
colAverage ::Colonnade Headed AverageValue Html
colAverage = mconcat
      [ headed "Ten Day Max" (viaFmt . tenDayMax)
      , headed "Seven Day Average" (viaFmt . sevenDayAverage)
      ]

---- Column For the statistics reports of all fields ----
colStats :: Colonnade Headed StatEntry Html
colStats = mconcat
      [ headed "Quote Field" (i . string . show . qfield)
      , headed "Mean" (viaFmt . meanVal)
      , headed "Min" (viaFmt . minVal)
      , headed "Max" (viaFmt . maxVal)
      , headed "Days between Min/Max" (viaFmt . daysBetweenMinMax)
      ]

---- Column data which contains columns for all fields from the Covid data class ----
colData :: Colonnade Headed CovidData Html
colData = mconcat
      [ headed "Day" (viaFmt . date)
      , headed "Iso Code" (viaFmt . iso_code)
      , headed "Continent" (viaFmt . continent )
      , headed "Location" (viaFmt . location)
      , headed "Total Cases" (viaFmt . total_cases)
      , headed "New Cases" (viaFmt . new_cases)
      , headed "Total Deaths" (viaFmt . total_deaths)
      , headed "New Deaths" (viaFmt . new_deaths)
      , headed "Reproduction Rate" (viaFmt . reproduction_rate)
      , headed "ICU Patients" (viaFmt  . icu_patients)
      , headed "Total Vaccinations" (viaFmt  . total_vaccinations)
      , headed "People Vaccinated" (viaFmt . people_vaccinated )
      , headed "People Fully Vaccinated" (viaFmt . people_fully_vaccinated)
      , headed "New Vaccinations" (viaFmt . new_vaccinations)
      ]


---- Builds the HTML Report for writing to a file ----
htmlReport :: (Functor t, Foldable t) =>
             t CovidData -> [StatEntry]  -> [StatEntry]-> [AverageValue] ->ByteString
htmlReport quotes statEntries traversedStatEntries averages  = renderHtml $ docTypeHtml $ do
     H.head $ do
       style tableStyle

     body $ do

       h1 "Averages Report"
       encodeHtmlTable mempty colAverage averages ---- Table of Top 10 Day Mean and Average 7 Day Mean ----

       h1 "Traversed Statistic"
       encodeHtmlTable mempty colStats traversedStatEntries ---- Table of the selected stat only ----

       h1 "Statistics Report"
       encodeHtmlTable mempty colStats statEntries ---- Table of statistics of all fields ----

       h1 "Covid Data"
       encodeHtmlTable mempty colData quotes ---- Table of all values ----
  where
    tableStyle = "table {border-collapse: collapse}" <>
            "td, th {border: 1px solid black; padding: 5px}"