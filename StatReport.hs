{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module StatReport where

import Data.List.Split(chunksOf)
import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy, Foldable (toList))
import Data.Time (diffDays, Day)
import Fmt      -- used for formating text
    ( Buildable(..),
      Builder,
      (+|),
      (+||),
      pretty,
      (|+),
      (||+),
      fixedF, yearF )
import Colonnade ( ascii, headed )

import CovidData ( field2fun,  QField(ReproductionRate, NewCases, NewVaccinations, TotalCases), CovidData(date, continent, CovidData, new_cases), getDayField )
import Data.ByteString (ByteString, getLine, interact, length, elemIndex)
import Text.Blaze.Html5.Attributes (xmlns, list)
import Data.Text (chunksOf)
import qualified GHC.Read as Data

decimalPlacesFloating :: Int
decimalPlacesFloating = 2

data StatValue = StatValue {     --this is used to format the calculated fields
    decimalPlaces :: Int,
    value :: Double
  }

data StatEntry = StatEntry {     -- where we store our stat results
    qfield :: QField,
    meanVal :: StatValue,
    minVal :: StatValue,
    maxVal :: StatValue,
    daysBetweenMinMax :: Int
  }

data AverageValue = AverageValue{ --- Data type to store ten day Max mean and average seven day mean
  tenDayMax :: Double,
  sevenDayAverage :: Double
}


----------------The Following were adapted from the in class example-----------------------

---- Fold based summing function for maybe values ----
sumMaybe:: (Fractional a, Foldable t) => t (Maybe a) -> a
sumMaybe = foldr ((+) . fromMaybe) 0

---- Can get the mean of any list of maybe values ----
mean :: (Fractional a, Foldable t) => t (Maybe a) -> a
mean xs =  sumMaybe xs / fromIntegral (Prelude.length xs)

---- Converted from the stock quotes example ----
computeMinMaxDays :: (Ord a, Foldable t) =>
                     (CovidData -> a) -> t CovidData -> (a, a, Int)
computeMinMaxDays get quotes = (get minQ, get maxQ, days)
  where
    cmp = comparing get
    minQ = minimumBy cmp quotes
    maxQ = maximumBy cmp quotes
    days = fromIntegral $ abs $ diffDays (date  minQ) (date maxQ)


---- Converted from the stock quotes example ----
statInfo :: (Functor t, Foldable t) =>   t CovidData -> [StatEntry]
statInfo  quotes = fmap qFieldStatInfo [minBound .. maxBound]
  where
    decimalPlacesByQField ReproductionRate = decimalPlacesFloating
    decimalPlacesByQField _ = 0

    qFieldStatInfo qfield =
      let
        get = field2fun qfield
        (mn, mx, daysBetweenMinMax) =
              computeMinMaxDays get quotes
        decPlaces = decimalPlacesByQField qfield
        meanVal = StatValue decimalPlacesFloating (mean $ fmap get quotes)
        minVal = StatValue decPlaces (fromMaybe mn)
        maxVal = StatValue decPlaces (fromMaybe mx)
      in StatEntry {..}

-----------------------------------------------------------------------------

--------------------Mathematic functions for maybe lists----------------------

---- Sum a list of maybe doubles converted to a double value ----
recSum :: [Maybe Double] -> Double
recSum [] = 0
recSum (Nothing:xs) = 0 +recSum xs
recSum (Just x:xs)= x + recSum xs

---- Get the mean of a list of maybe doubles converted to a double ----
regMean :: [Maybe Double] -> Double
regMean xs = (recSum xs)/fromIntegral (Prelude.length xs)

---- Get the lowest value from a list of maybe doubles
regMin :: [Maybe Double] -> Maybe Double
regMin [] = Nothing
regMin [Just x] = Just x
regMin (x:xs) = minValue x (regMin xs)

---- Comparison function for two maybe doubles to get the lowest value ----
minValue :: Maybe Double -> Maybe Double-> Maybe Double
minValue x y
  | x > y = y
  | x < y =  x
  | x == y  = x


--- Applies the Max value recursively across list to ultimately get the max of the list ----
regMax :: [Maybe Double] -> Maybe Double
regMax [] = Nothing
regMax [Just x] = Just x
regMax (x:xs) = maxValue x (regMax xs)


---- Comparison function for two maybe doubles to get the highest value ----
maxValue :: Maybe Double -> Maybe Double-> Maybe Double
maxValue x y
  | x < y = y
  | x > y =  x
  | x == y  = x
-------------------------------------------------------------------------------

---- Simple function for mean of lists of doubles ----
nonMaybeMean:: [Double] -> Double
nonMaybeMean xs = (sum xs)/fromIntegral (Prelude.length xs)

---- Get field by index ----
getField :: Int ->  QField
getField a =  toEnum a::QField

---- Simple function for converting maybe value to a numerical value ----
fromMaybe :: Num a => Maybe a -> a
fromMaybe Nothing = 0
fromMaybe (Just n) = n




---------Uses Library Function to split up list into list of lists of given length-------
binning :: Int -> [Maybe Double] -> [[Maybe Double]]
binning = Data.List.Split.chunksOf


maximum' :: Ord a => [a] -> a ---- Single quote used to differentiate from the Prelude.maximum function
maximum' [x]       = x ---- if there is only one element in the list this is the max
maximum' (x:y:xs) = maximum' ((if x >= y then x else y):xs) --- compares the first two elements in the list eliminating the lower


---------- Used to get average mean from a list of means -----------------------

averageMean :: Int -> [Maybe Double] -> Double
averageMean size list= average
  where
    ranges = binning size list ---- Splits the list into lists of the given size variable
    means = fmap regMean ranges ---- maps the mean function across each of these lists
    average = nonMaybeMean means ----- gets the average value of all means

------------ Used to get highest mean from a list of means ---------------------
topMean :: Int -> [Maybe Double] -> Double
topMean size list= top
  where
    ranges = binning size list ---- Splits the lists into lists of the given size variable
    means = fmap regMean ranges ---- maps the mean function across each of these lists
    top = maximum' means ---- gets the highest mean across the lists



---- Same as the given statInfo function except it varies by the Int input
---- this allows us the only access the statInfo of a choosen field by index
traversedStatInfo :: (Functor t, Foldable t) => Int ->  t CovidData -> [StatEntry]
traversedStatInfo a  quotes = fmap qFieldStatInfo [getField a]   ---- getField is used to specify the chosen field     

  where
    decimalPlacesByQField ReproductionRate = decimalPlacesFloating
    decimalPlacesByQField _ = 0

    qFieldStatInfo qfield =
      let
        get = field2fun qfield
        (mn, mx, daysBetweenMinMax) =
              computeMinMaxDays get quotes
        decPlaces = decimalPlacesByQField qfield
        meanVal = StatValue decimalPlacesFloating (mean $ fmap get quotes)
        minVal = StatValue decPlaces (fromMaybe mn)
        maxVal = StatValue decPlaces (fromMaybe mx)
      in StatEntry {..}


---- Gets List of values for all fields ----
getNewList :: Foldable t => t a -> [a]
getNewList quotes = val
  where
    val = fromFoldable quotes

---- Gets the Value field of the given statValue ----
getValue :: StatValue -> Double
getValue = value

---- Get the value of given field from CovidData type ----
getNewValue:: Int -> (CovidData -> Maybe Double)
getNewValue a = field2fun (getField a)

---- Maps the above function across the whole dataset ----
getNewValues:: Int -> [CovidData] -> [Maybe Double]
getNewValues a = fmap (getNewValue a)

---- Gets the Value field of all StatValues ----
fromStatValue :: [StatValue] -> [Double]
fromStatValue = fmap getValue

---- Just renaming of toList function to fromFoldable ----
fromFoldable :: Foldable t => t a -> [a]
fromFoldable = toList

---- Comparison used to check if a row of the datasets dates are within the given period
checkDate :: Day->Day-> CovidData -> Bool
checkDate start end daysData = currentDay >= start  && currentDay <= end ---- checks to see if this date is within the given start and end dates
  where
    currentDay = getDayField daysData ---- Gets the date for the given row of the data set ----


---- Used to get a dataset with only the rows within the given dates ----
clipCovidData :: Day -> Day -> [CovidData] -> [CovidData]
clipCovidData _ _ [] = [] --- just returns the empty list
clipCovidData start end (x:xs) =
  if ((checkDate start end x)) then x:clipCovidData start end xs ---- date is within the given period keep it in the list and recursively run the function on the rest of the list
  else clipCovidData start end xs ---- date is outside the given period drop the value and recursively run the function on the rest of the list



--------------- The following are adapted from the in class example --------------
instance Buildable StatValue where                                   
  build sv = fixedF (decimalPlaces sv)  (value sv)              

instance Buildable StatEntry where
  build StatEntry {..} =
           ""+||qfield||+": "
             +|meanVal|+" (mean), "
             +|minVal|+" (min), "
             +|maxVal|+" (max), "
             +|daysBetweenMinMax|+" (days)"

textReport :: [StatEntry] -> String                                    
textReport = ascii colStats                                           
  where
    colStats = mconcat
      [ headed "Quote Field" (show . qfield)                           
      , headed "Mean" (pretty . meanVal)                               
      , headed "Min" (pretty . minVal)
      , headed "Max" (pretty . maxVal)
      , headed "Days between Min/Max" (pretty . daysBetweenMinMax)
      ]

showPrice :: Double -> Builder                                            
showPrice = fixedF decimalPlacesFloating
------------------------------------------------------------------------------------


---- Own buildable instance for table of ten day and seven day rolling averages ----
instance Buildable AverageValue where
  build AverageValue {..} =
           ""+|tenDayMax|+" (Ten Day Max), "
             +|sevenDayAverage|+" (Seven Day Average), "



---- Report Function for both values ----
averageReport :: [AverageValue] -> String                                    
averageReport = ascii colStats                                           
  where
    colStats = mconcat
      [ headed "Ten Day Max" (show . tenDayMax)                          
      , headed "Seven Day Average" (pretty . sevenDayAverage)                               
      ]