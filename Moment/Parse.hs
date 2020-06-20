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

  import Data.Time
  import Data.Time.Format
  import Data.Time.Calendar
  import Data.Fixed
  --import System.Locale (defaultTimeLocale)
  import System.Posix.Unistd
  import Moment.Types(Date)

  --Fecha y hora actual del sistema
  currentTime :: IO UTCTime
  currentTime = getCurrentTime

  --Fecha y hora actual del sistema en formato IO String
  currentTimeStrIO :: IO String
  currentTimeStrIO = fmap show currentTime

  --Fecha y hora del sistema en formato String
  --currentTimeStr :: String
  currentTimeStr = do
    strVal <- currentTimeStrIO
    return strVal

  --Formateo de fechas
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


  --Convierte un string a un tipo de fecha de formato establecido
  --Formato YYYYMMDD = "%Y%m%d"
  --Prj: stringToDate "20150511" "%Y%m%d" -> 2015-05-11 00:00:00 UTC
  --TODO: readTime está obsoleto: Buscar alternativa
  stringToDate :: String -> String -> UTCTime
  stringToDate input format = readTime defaultTimeLocale format input :: UTCTime

  makeUtcTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> UTCTime
  makeUtcTime yyyy mm dd h24 mi ps = UTCTime { utctDay = fromGregorian yyyy mm dd,
                          utctDayTime = timeOfDayToTime (TimeOfDay h24 mi ps)
                        }

  --Extrae el año de una fecha dada en formato YYYYMMDD
  extractYear :: Date -> Maybe Int
  extractYear date
    | (length date >= 4)    = Just (read (take 4 date)::Int)
    | otherwise             = Nothing

  --Extrae el mes de una fecha dada en formato YYYYMMDD
  extractMonth :: Date -> Maybe Int
  extractMonth date
    | length date >= 6    = Just month
    | otherwise             = Nothing
    where
        md = drop 4 date
        month = read (take 2 md)::Int

  --Extrae el dia de una fecha dada en dormato YYYYMMDD
  extractDay :: Date -> Maybe Int
  extractDay date
    | (length date == 8)        = Just (read(drop 6 date)::Int)
    | otherwise                 = Nothing


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
