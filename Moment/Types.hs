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

  import Data.Monoid

  type YearCalendar = Integer
  type MonthCalendar = Int
  type DayCalendar = Int
  type Index = Int

  --Definición del tipo Date, se espera una fecha de tipo YYYYMMDD
  type Date = String
  type DateCalendar = [Maybe Int] --Fecha calendario en modo lista

  --Define el tipo de calendario
  data KindCalendar = REGULAR | PERIODIC | RULEBASED | FIXDAYS deriving (Show, Read, Eq)

  --Comportamiento del calendario
  --SELF_CONTAINED: Autónomo e independiente.
  --                Una vez creado su agendamiento no depende de los cambios del calendario parent
  --DYNAMIC_CONTENT: Su contenido varía dependiendo del calendario parent,
  --                 y de sus reglas de agendamiento si es que fuera el caso
  data CalendarBehavior = SELF_CONTAINED | DYNAMIC_CONTENT deriving (Show, Read, Eq)

  --Declaración de tipo Días Calendario
  --type DaysCalendar = [(YearCalendar, [(MonthCalendar, [DayCalendar])])]

  {-|
    Estructura típica de una Regla
    * t es una función general de transformación
    * b es el tipo base de referencia a la cual se aplicarán las reglas y la transformación
    * r es la lista que contiene las reglas a aplicar
    >>> let rc = Rule $ NoTransform (Inverse [1], Just [0])
  -}
  --newtype Rule t b r = Rule {unRule :: t (t b, Maybe [r])} --deriving (Show, Read, Eq)

  data Transformada a where -- = NoTrans t | Trans a
    Trans :: (a -> b) -> b -> Transformada a
    Notrans :: a -> Transformada a



  --Tipo Regla de Agendamiento, por ej: "EVERYDAY", "+D1", "-10", ">5"
  -- "ANYDAY",
  --type Rule = String
  data Rule = Nd                              --Ningún día
            | Ed                                --Todos los días
            | Sd (RulePrefix [String])         --Fehas específicos
            | Yc (RulePrefix [YearCalendar])   --Años calendario
            | Mc (RulePrefix [MonthCalendar])  --Meses calendario
            | Dm (RulePrefix [DayCalendar])    --Días del mes calendario
            | Wd (RulePrefix [Int])            --Días de la semana, pj: Lunes, -Lunes
            | Wk (RulePrefix [Int])            --Semanas del mes
             deriving (Show, Eq, Read)


  {-
  data Rule a = Nd                              --Ningún día
             | Ed                              --Todos los días
             | Sd (a)         --Fehas específicos
             | Yc (a)   --Años calendario
             | Mc (a)  --Meses calendario
             | Dm (a)    --Días del mes calendario
             | Wd (a)            --Días de la semana, pj: Lunes, -Lunes
             | Wk (a)            --Semanas del mes
              deriving (Show, Eq, Read)
              -}

  data RulePrefix a = NoRulePrefix
                    | NotPrefix a           -- Sin prefijo
                    | ForceIncl a           -- +D1, +L1
                    | ForceExcl a           -- -D1, -L1
                    | OrNextSch a           -- >1
                    | OrPrevSch a           -- <
                    deriving (Show, Read, Eq)

  instance (Monoid a) => Monoid (RulePrefix a) where
    mempty = NoRulePrefix
    mappend NoRulePrefix m = m
    mappend m NoRulePrefix = m
    mappend (NotPrefix m1) (NotPrefix m2) = NotPrefix (m1 <> m2)
    mappend (ForceIncl m1) (ForceIncl m2) = ForceIncl (m1 <> m2)
    mappend (ForceExcl m1) (ForceExcl m2) = ForceExcl (m1 <> m2)
    mappend (OrNextSch m1) (OrNextSch m2) = OrNextSch (m1 <> m2)
    mappend (OrPrevSch m1) (OrPrevSch m2) = OrPrevSch (m1 <> m2)
    mappend m1 m2 = mempty

  instance Monoid (Rule) where
    mempty = Nd
    mappend (Yc m1) (Yc m2) = Yc (m1 <> m2)



  --Tipo Lista de Reglas, grupo de reglas o Definiciones de reglas
  --P.ej.: let r1 = ["EVERYDAY", "-25", "+D5"]::RuleGroup
  --       Dias feriados de Chile let fd = ["W6", "W7", "24/12", "10/10", ..]
  type RuleGroup = ([Rule]) --deriving (Show, Read, Eq)
  --newtype RuleGroup = RuleGroup { ruleGroup :: [Rule] } deriving (Show, Eq, Read)

  --Tipo reglas anidadas o lógica de aplicación de las reglas
  --De momento las NestedRules estarán conformadas únicamente por dos grupos
  data NestedRules = Nr (RuleGroup) | And (RuleGroup) | Or (RuleGroup) deriving (Show, Read, Eq)

  --Definición del tipo reglas de agendamiento
  data SchedulingRules = NoneSchR | SchedulingRules { schedulingRules :: [NestedRules]} deriving (Read, Eq, Show)

  --Typo información de un Calendario
  data CalendarInfo = CalendarInfo {name :: String,
                                    description :: String,
                                    kind :: KindCalendar,
                                    parent :: String,
                                    behaivor :: CalendarBehavior,
                                    --rules :: SchedulingRules,
                                    autoextend :: Bool} deriving (Show, Eq, Read)

  --Declaración de tipo Definición de Calendar
  data Calendar a = Calendar {info :: CalendarInfo,
                              calDays :: a
                            } deriving (Show, Read, Eq)

  --Tipo de operadores de calendarios
  data CalendarOperator = OrMethod
                        | AndMethod
                        | InvertMethod
                        deriving (Show, Eq, Read)
