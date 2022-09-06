{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CovidData where
import qualified Data.Text as T
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.ByteString.Char8 (unpack)
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord, FromField (..))
import qualified Data.ByteString as BL
import GHC.ExecutionStack (Location(Location))

---- CovidData class and all the necessary fields ----
data CovidData = CovidData {
                   date :: Day,
                   iso_code  :: String,
                   continent :: String,
                   location :: String,
                   total_cases :: Maybe Double,
                   new_cases :: Maybe Double,
                   total_deaths :: Maybe Double,
                   new_deaths ::  Maybe Double,
                   reproduction_rate :: Maybe Double,
                   icu_patients :: Maybe Double,
                   total_vaccinations :: Maybe Double,
                   people_vaccinated :: Maybe Double,
                   people_fully_vaccinated :: Maybe Double,
                   new_vaccinations :: Maybe Double
                 }
  deriving (Generic, FromNamedRecord, Show)

---- Parses Date Data Type ----
instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack


---- Gets the date of the given row of the dataset ----
getDayField :: CovidData -> Day
getDayField = date


data QField =   TotalCases | NewCases |
                TotalDeaths | NewDeaths | ReproductionRate | IcuPatients | 
                TotalVaccinations | PeopleVaccinated | PeopleFullyVaccinated | NewVaccinations
  deriving (Eq, Ord, Show, Enum, Bounded)



field2fun :: QField -> CovidData -> Maybe Double
field2fun TotalCases =  total_cases
field2fun NewCases = new_cases
field2fun TotalDeaths = total_deaths
field2fun NewDeaths = new_deaths
field2fun ReproductionRate = reproduction_rate
field2fun IcuPatients = icu_patients
field2fun TotalVaccinations = total_vaccinations
field2fun PeopleVaccinated = people_vaccinated
field2fun PeopleFullyVaccinated = people_fully_vaccinated
field2fun NewVaccinations = new_vaccinations