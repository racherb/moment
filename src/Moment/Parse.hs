{-
module      :  Helpers.Momentum.Parse
Description :  Parseo y operaciones con fecha
Copyright   :  (c) Raciel Hernández Barroso
License     :  Privative
Date     :  2015.01.06

Maintainer  :  racielhb@gmail.com
Stability   :  experimental [unstable | experimental | provisional | stable | frozen]
Portability :  portable | non-portable (<reason>)

Helpers.Momentum.Parse
-}
module Moment.Parse(currentTime,
  currentTimeStrIO,
  currentTimeStr,
  format,
  FormatTime(..),
  stringToDate,
  extractYear,
  extractMonth,
  extractDay,
  makeUtcTime) where

  import Moment.Prelude

  import Data.Time
  import Data.Fixed

  import Moment.Types(Date)

  -- | Current system date and time
  currentTime :: IO UTCTime
  currentTime = getCurrentTime

  -- | Current system date and time in IO String format
  currentTimeStrIO :: IO String
  currentTimeStrIO = fmap show currentTime

  --Fecha y hora del sistema en formato String
  currentTimeStr :: IO String
  currentTimeStr = currentTimeStrIO

  -- | Date Formatting
  --TODO: Generalizar expresión usando patrones (regexp)
  --"%Y-%m-%d"
  format :: FormatTime t => [Char] -> t -> String
  format ofmt udt
    | ofmt == "YYYYMMDD"      = applyFormat "%Y%m%d"
    | ofmt == "YYMMDD"        = applyFormat "%y%m%d"
    | ofmt == "YYYY-MM-DD"    = applyFormat "%Y-%m-%d"
    | ofmt == "YY-MM-DD"      = applyFormat "%y-%m-%d"
    | ofmt == "YYYY/MM/DD"    = applyFormat "%Y/%m/%d"
    | ofmt == "YY/MM/DD"      = applyFormat "%y/%m/%d"
    | ofmt == "HHMI"          = applyFormat "%H%M"
    | ofmt == "WDI"           = applyFormat "%u"
    | ofmt == "ISO-P"         = applyFormat "%Y%m%dT%H%M%S"
    | otherwise                   = applyFormat ofmt
    where
      applyFormat fmt = formatTime defaultTimeLocale fmt udt


  -- | Converts a string to a set format fmt date type
  --Formato YYYYMMDD = "%Y%m%d"
  --Prj: stringToDate "20150511" "%Y%m%d" -> 2015-05-11 00:00:00 UTC
  stringToDate :: String -> String -> UTCTime
  stringToDate input fmt = parseTimeOrError True defaultTimeLocale fmt input :: UTCTime

  makeUtcTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> UTCTime
  makeUtcTime yyyy mm dd h24 mi ps = UTCTime { utctDay = fromGregorian yyyy mm dd,
                          utctDayTime = timeOfDayToTime (TimeOfDay h24 mi ps)
                        }

  -- | Extracts the year from a given date in YYYYMMDD format
  extractYear :: Date -> Maybe Int
  extractYear date
    | length date >= 4      = Just (read (take 4 date)::Int)
    | otherwise             = Nothing

  --  | Extracts the month from a given date in YYYYMMDD format
  extractMonth :: Date -> Maybe Int
  extractMonth date
    | length date >= 6      = Just month
    | otherwise             = Nothing
    where
        md = drop 4 date
        month = read (take 2 md)::Int

  -- | Extracts the day of a given date in YYYYMMDD format
  extractDay :: Date -> Maybe Int
  extractDay date
    | length date == 8      = Just (read(drop 6 date)::Int)
    | otherwise             = Nothing


  {-
  now :: String -> Maybe (IO String)
  now format = do
    let tnow = currentTime
    let (Just ans) = format tnow format
    return $ ans

  moment :: IO ()
  moment = return ()

  --seconds
  --miliseconds
  --minute
  --hours

  --get
  -}
