{-
module      :  Moment.Configuration
Description :  Configuration de Calendarios
Copyright   :  (c) Raciel Hernández Barroso
License     :  Privative
Date     :  2015.01.06

Maintainer  :  racielhb@gmail.com
Stability   :  experimental [unstable | experimental | provisional | stable | frozen]
Portability :  portable | non-portable (<reason>)

Moment.Configuration
-}

{-# OPTIONS_GHC -fwarn-missing-signatures #-}

module Moment.Configuration where

  import Moment.Types (RuleGroup, YearCalendar)

  weekdays = [1..7]
  --Monday Tuesday Wednesday Thursday Friday Saturday Sunday
  weekdays'         = [(1, "Mon", "Monday"),
                       (2, "Tue", "Tuesday"),
                       (3, "Wed", "Wednesday"),
                       (4, "Thu", "Thursday"),
                       (5, "Fri", "Friday"),
                       (6, "Sat", "Saturday"),
                       (7, "Sun", "Sunday")]

  firstDayOfTheWeek = 1              --Primer día de la semana -Monday
  workdays          = [1..5]         --Días de trabajo -Monday to Friday
  weekend           = [6,7]          --Días de fin de semana -Saturday & Sunday
  newDayTime        = "00:00:00"     --Hora en que inicia el nuevo día
  midTime           = "00:00:00"     --Hora de media noche

  weeksOnYear       = 52             -- 52 semanas en un año normal
  weeksOnYear'      = 53             -- 53 semanas tiene un año biciesto
  monthsOnYear      = 12             -- 12 meses tiene un año
  daysOnWeek        = 7              -- 7 días tiene una semana
  daysOnYear        = (weeksOnYear * daysOnWeek) + 1
  daysOnYear'       = (weeksOnYear' * daysOnWeek) + 1
  hoursOnDay        = 24             -- 24 horas tiene un día
  minOnHour         = 60             -- 60 minutos tiene una hora
  secOnMinute       = 60             -- 60 segundos tiene un minuto

  --Secuencia de meses en un año Calendario
  seqMonthsYearCalendar :: [Int]
  seqMonthsYearCalendar = [1..12]

  --Secuencia de días Calendarios
  seqDaysMonthCalendar :: [Int]
  seqDaysMonthCalendar = [1..31]


    {-
    domingo   01/01/2017  Año Nuevo (irrenunciable de tercera categoría)
    viernes   14/04/2017  Viernes Santo
    sábado    15/04/2017  Sábado Santo
    miércoles 19/04/2017  censo abreviado de población y vivienda
    lunes     01/05/2017  Día Nacional del Trabajo (irrenunciable de tercera categoría)
    domingo   21/05/2017  Día de las Glorias Navales
    miércoles 07/06/2017  Aniversario del Asalto y Toma del Morro de Arica (válido solamente en la XV región de Arica y Parinacota)
    lunes     26/06/2017  San Pedro y San Pablo
    domingo   02/07/2017  elecciones primarias (presidenciales y congresistas) (irrenunciable de segunda categoría) (realización de la elección aún indeterminada)
    domingo   16/07/2017  Virgen del Carmen
    martes    15/08/2017  Asunción de la Virgen
    domingo   20/08/2017  Nacimiento del Prócer de la Independencia (válido solamente en las comunas de Chillán y Chillán Viejo)
    lunes     18/09/2017  Día de la Independencia Nacional (irrenunciable de tercera categoría)
    martes    19/09/2017  Día de las Glorias del Ejército (irrenunciable de tercera categoría)
    lunes     09/10/2017  Encuentro de Dos Mundos
    viernes   27/10/2017  Día Nacional de las Iglesias Evangélicas y Protestantes
    miércoles 01/11/2017  Día de Todos los Santos
    domingo   19/11/2017  elecciones presidencial (¿primera vuelta?), congresistas y regionales (irrenunciable de segunda categoría)
    viernes   08/12/2017  Inmaculada Concepción de la Virgen
    domingo   17/12/2017  elección presidencial (segunda vuelta) (irrenunciable de segunda categoría) (este feriado solamente tendrá lugar si se efectúa una segunda vuelta en la elección presidencial)
    lunes     25/12/2017  Natividad del Señor (irrenunciable de tercera categoría)
    -}
