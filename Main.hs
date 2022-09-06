{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import CovidData
import HandleData(generateReports)
import qualified Data.Vector as V

---- Functions taken from in class example ----
---- Uses functions from the src folder ----
main ::  IO ()
main  = do
  putStrLn "Please enter the filepath/name of the csv file: (stats.csv)"
  fname <- getLine
  putStrLn "Please enter the name of the html target file"
  htmlFile <- getLine
  csvData <- BL.readFile fname
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, covidData) -> generateReports htmlFile covidData


readData :: FilePath -> IO  [CovidData]
readData fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, readData) -> pure (V.toList readData)

