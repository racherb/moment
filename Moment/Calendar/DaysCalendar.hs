{-|
Module      :  Moment.Calendar.DaysCalendar
Description :  Definición y Operaciones de Calendarios
Copyright   :  (c) Raciel Hernández Barroso
License     :  MIT
Stability   :  experimental
Portability :  non-portable
-}

{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# OPTIONS_GHC -fwarn-unused-binds #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}

module Moment.Calendar.DaysCalendar where

  import Moment.Calendar.Internals (sort, nub)

  class DaysCalendarFactory a where
    make :: DaysCalendar a
    del :: DaysCalendar a
    get :: DaysCalendar a
