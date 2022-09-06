{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HandleData where

-- import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.List.Split(chunksOf)
import Data.Foldable (toList)
import Data.Text (unpack)
import qualified Data.Vector as V
import Data.Time (Day, parseTimeM, defaultTimeLocale, parseTimeOrError)
import CovidData
import StatReport ( AverageValue (AverageValue, sevenDayAverage, tenDayMax), binning, statInfo, textReport, topMean, averageMean, getNewList, traversedStatInfo, StatValue (value), getNewValues, clipCovidData, regMean, fromMaybe, regMax, regMin, fromMaybe, averageReport)
import HtmlReport ( htmlReport )
import Text.Blaze.Html4.FrameSet (input)

-- Menu for choosing category, then getting min and max between given days
printMenu :: IO Int
printMenu = do
  putStrLn "Choose category to query"
  putStrLn "---------------------------------------"
  putStrLn "Enter 0 for : Total Cases"
  putStrLn "Enter 1 for : New Cases"
  putStrLn "Enter 2 for : Total Deaths"
  putStrLn "Enter 3 for : New Deaths"
  putStrLn "Enter 4 for : Reproduction Rate"
  putStrLn "Enter 5 for : ICU Patients"
  putStrLn "Enter 6 for : Total Vaccinations"
  putStrLn "Enter 7 for : People Vaccinated"
  putStrLn "Enter 8 for : People Fully Vaccinated"
  putStrLn "Enter 9 for : New Vaccinations"
  putStrLn "---------------------------------------"
  inp <- getLine
  let val = read inp::Int
  if val >= 0 && val <= 9 then return val else printMenu ---- Loops until a valid number is inputted ----

generateReports :: (Functor t, Foldable t) =>
                 String ->  t CovidData -> IO ()
generateReports htmlFl covidData = do

  ---- Gets category from input in printMenu ----
  ---- Then breaks this down into tables for both text report and the html report ----
  ---- Both provide two tables one which includes statistical report for all categories ----
  ---- and one just for the choosen category. ----

  category <- printMenu ---- Stores players input ----
  let traversedStatInfo' = traversedStatInfo category covidData ---- get the StatEntry for the chosen field ----
  let statInfo' = statInfo covidData --- Get all StatEntry values ----
  let textRpt = textReport statInfo' ---- Build text report for console ----

  let traverseTextRpt = textReport traversedStatInfo' --- Text Report for the chosen field only ----
  let newCaseInfo = traversedStatInfo 1 covidData ---- Gives new Case field only ----

  let rows = getNewList covidData --- All Rows of the dataset in a list format ----

  chooseDates rows ---- Calls function where user will input dates and data is returned within these dates

  let casesValues = getNewValues 1 rows ---- 1 is the index of the cases field


  ---- Gives the top Top 10 day mean value ----
  putStrLn "\nTOP 10 DAY"
  let rollingTen = topMean 10 casesValues
  print rollingTen

  --- Gives the average 7 day mean value ----
  putStrLn "\nAVERAGE 7 DAY"
  let rolling7 = averageMean 7 casesValues
  print rolling7


  ---- Gives the average 10 day mean value ----
  putStrLn "\nAVERAGE 10 DAY\n"
  let rolling7 = averageMean 10 casesValues
  print rolling7

  ---- Construct AvarageValue data type for later use with html reporting ----
  let averages = AverageValue{tenDayMax = rollingTen, sevenDayAverage = rolling7}

  ---- Converted to list for using within html reporting ----
  let input = [averages]

  ---- Converts to string which is a table ----
  let value = averageReport input

  ---- Generates html file
  let htmlRpt = htmlReport  covidData statInfo' traversedStatInfo' input
  BL.writeFile htmlFl htmlRpt
  putStrLn value

  ---- Outputs text report ----
  putStr textRpt
  putStr traverseTextRpt



chooseDates :: [CovidData] -> IO()
chooseDates cases = do
    -- Letting user choose the day for which we well calculate
  -- -- mean, average and min between
  --
  putStrLn "Choose Two Dates Between 2020-02-02 and 2021-05-17"
  putStrLn "Start Day: Format(YYYY-MM-DD)"
  dayOne <- getLine
  putStrLn "End Day: Format(YYYY-MM-DD)"
  dayTwo <- getLine
  let startDate = parseDay dayOne
  let endDate = parseDay dayTwo

  ---- Gets only the wanted dates ----
  let clipped = clipCovidData startDate endDate cases

  let clippedTotalCases = getNewValues 0 clipped
  let clippedNewCases = getNewValues 1 clipped
  let clippedNewVaccinations = getNewValues 9 clipped

  newVaccineInfo clippedNewVaccinations
  newCasesInfo clippedNewCases
  totalCasesInfo clippedTotalCases




---- Gives the wanted info for number of new vaccinations ----
newVaccineInfo :: [Maybe Double]-> IO ()
newVaccineInfo vaccineInfo = do
  let meanNewVaccinations = regMean vaccineInfo
  let minNewVaccinations = regMin vaccineInfo
  let maxNewVaccinations = regMax vaccineInfo


  putStrLn "\nAVERAGE NEW VACCINATIONS FOR PERIOD"
  print meanNewVaccinations
  putStrLn "\nMIN NEW VACCINATIONS FOR PERIOD"
  print (fromMaybe minNewVaccinations)
  putStrLn "\nMax NEW VACCINATIONS FOR PERIOD"
  print (fromMaybe maxNewVaccinations)
  putStrLn "\n\n\n"



---- Gives the wanted info for number of new cases ----
newCasesInfo :: [Maybe Double]-> IO ()
newCasesInfo casesInfo = do
  let meanNewCases = regMean casesInfo
  let minNewCases = regMin casesInfo
  let maxNewCases = regMax casesInfo


  putStrLn "\nAVERAGE NEW CASES FOR PERIOD"
  print meanNewCases
  putStrLn "\nMIN NEW CASES FOR PERIOD"
  print (fromMaybe minNewCases)
  putStrLn "\nMax NEW CASES FOR PERIOD"
  print (fromMaybe maxNewCases)
  putStrLn "\n\n\n"


---- Gives the wanted info for number of total cases ----
totalCasesInfo :: [Maybe Double] -> IO()
totalCasesInfo totalCases = do
  let meanTotalCases = regMean totalCases
  let minTotalCases = regMin totalCases
  let maxTotalCases = regMax totalCases


  putStrLn "\nAVERAGE TOTAL CASES FOR PERIOD"
  print meanTotalCases
  putStrLn "\nMIN TOTAL CASES FOR PERIOD"
  print (fromMaybe minTotalCases)
  putStrLn "\nMax TOTAL CASES FOR PERIOD"
  print (fromMaybe maxTotalCases)
  putStrLn "\n\n\n"

---- used to parse a string to the Date data type ----
parseDay :: String -> Day
parseDay = parseTimeOrError True defaultTimeLocale timeFormat
  where
    timeFormat = "%Y-%m-%d"
