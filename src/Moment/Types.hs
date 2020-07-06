{-
module      :  Moment.Types
Description :  Definición y Operaciones de Calendarios
Copyright   :  (c) Raciel Hernández Barroso
License     :  Privative
Date     :  2015.01.06

Maintainer  :  racielhb@gmail.com
Stability   :  experimental [unstable | experimental | provisional | stable | frozen]
Portability :  portable | non-portable (<reason>)

Moment.Types
-}

{-# LANGUAGE GADTs #-}

module Moment.Types where

  import Moment.Prelude

  type YearCalendar = Integer
  type MonthCalendar = Int
  type DayCalendar = Int
  type Index = Int

  --Definición del tipo Date, se espera una fecha de tipo YYYYMMDD
  type Date = String
  type DateCalendar = [Maybe Int] --Fecha calendario en modo lista

  --Define el tipo de calendario
  data KindCalendar = REGULAR | PERIODIC | RULEBASED | FIXDAYS deriving (Show, Read, Eq)
