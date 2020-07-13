{-|
Module      :  Moment.Calendar.Internals
Description :  Internal Module of Definition and Operations of Calendars
Copyright   :  (c) Raciel Hernández Barroso
License     :  MIT
Stability   :  experimental
Portability :  non-portable

/Definition/.

'DaysCalendar' is a type that is composed of a list of values containing the month, day and year tuples.
For example:

@
  oc = zeros $ singleton (2019, 12, V.fromList []) 
  jc = ones $ singleton (2020, 1, V.fromList [])
  fc = step 1 1 30 $ singleton (2020, 2, V.fromList [])
  
  dc = oc <> jc <> fc

  ac = make 2020 03 (V.fromList [1,1,1,0,1])
  
  de = DaysCalendar $ V.fromList [(2016,12, V.fromList [1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0])]
  
@

-}

-- | The 'DaysCalendar' module allows advanced operations on calendar days in a simple and readable way, facilitating the scheduling and the elaboration of new calendar specifications.
-- ! This module is internal and it is not recommended to use it directly. Use DaysCalendar instead.

{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MonoLocalBinds          #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE Unsafe                  #-}


--{-# OPTIONS_GHC -fwarn-missing-signatures #-}

module Moment.Calendar.Internals (
-- * Types
  Day
, BiDay
, YearCalendar
, MonthCalendar
, DaysCalendar(..)
-- * Vectors
, nub
, sort
, (!)
-- * Constructors
, empty
, singleton
, make
, ones
, zeros
, step
, pulse
, section
, replica
-- * Queries
, qyearc
, qmonthc
, eyc
, eymc
, emyc
, edmc
-- * DaysCalendar functions
, or
, and
, invert
, add
, sustract
, normalize
, holes
, match
, move
, reverse
, oddd
, evend
, dropydc
, dropmdc
, dropymdc
-- * Conversion
, toDay
, toDates
, fromDates

-- * Helpers


) where
  
  import Moment.Prelude
  import Data.Vector ((!))
  import qualified Data.Vector as V
  import qualified Data.Vector.Unboxed as U
  import qualified Data.Vector.Algorithms.Merge as V (sort)
  import Data.Time (formatTime, defaultTimeLocale, Day, toGregorian, addDays, utctDay, gregorianMonthLength)
  import qualified Control.Monad.ST as ST

  import Moment.Parse --(makeUtcTime, extractYear, extractMonth, extractDay)
  
  -- * Important information
  -- ! Deprecated function, do not use
  -- ? Expose in API?
  -- TODO: Refactor this method

  {-@ type Nat = {v:Int | 0 <= v} @-}
  type YearCalendar = Integer    -- ^ Calendar year, 2020
  type MonthCalendar = Int       -- ^ Month Calendar 1..12
  type WeekDay = Int             -- ^ Week day index 1..7
  type WeekMonth = Int           -- ^ Week month
  type BiDay = Int               -- ^ Binary Day 0..1
  type IdDay = Int               -- ^ Index Day
  
  -- | newtype DaysCalendar a = DaysCalendar {unDaysCalendar :: V.Vector (YearCalendar, MonthCalendar, V.Vector a)} deriving (Show, Eq, Read)
  newtype DaysCalendar a = DaysCalendar {
    unDaysCalendar :: V.Vector (YearCalendar, MonthCalendar, V.Vector a)
    } deriving (Show, Eq, Read)
  
  -- | Return the empty 'DaysCalendar' type element
  empty :: DaysCalendar a
  {-# INLINE empty #-}
  empty = DaysCalendar V.empty

  -- | Returns the unique elements of a vector
  nub :: Eq a => V.Vector a -> V.Vector a
  {-# INLINE nub #-}
  nub !v
    | V.null v = V.empty
    | V.any (== V.head v) (V.tail v) = nub $ V.tail v
    | otherwise = V.cons (V.head v) (nub $ V.tail v)

  -- | Arrange the elements of a vector
  sort :: Ord a => V.Vector a -> V.Vector a
  {-# INLINE sort #-}
  sort !v = ST.runST $ do
    mv <- V.thaw v
    V.sort mv
    V.freeze mv

  -- | Create a 'DaysCalendar' type from an arbitrary minimum expression
  singleton :: (YearCalendar, MonthCalendar, V.Vector a) -> DaysCalendar a
  {-# INLINE singleton #-}
  singleton !x = DaysCalendar $ V.singleton x
  
#if MIN_VERSION_base(4,9,0)
  -- | Semigroup instance of type 'DaysCalendar'
  -- Since: base 4.9.0.0
  instance Semigroup (DaysCalendar a) where
    {-# INLINE (<>) #-}
    (DaysCalendar !a) <> (DaysCalendar !b) = DaysCalendar (a V.++ b)

  -- | Monoid instance of type 'DaysCalendar'
  instance Monoid (DaysCalendar a) where
    {-# INLINE mempty #-}
    mempty = empty
#else
  instance Monoid (DaysCalendar a) where
    {-# INLINE mempty #-}
    {-# INLINE (<>) #-}
    mempty = empty
    (DaysCalendar !a) <> (DaysCalendar !b) = DaysCalendar (a V.++ b)
#endif
  -- | Functor instance of type 'DaysCalendar'
  instance Functor DaysCalendar where
    fmap f !dc = DaysCalendar fm
      where
        fm = fmap (\(x, y, z) -> (x,y, fmap (\d -> f d) z)) v
        v = unDaysCalendar dc

  {-|
    'qyearc' Get all the DaysCalendar elements that belong to the given year
    >>> qyearc 2020 dc
    DaysCalendar {unDaysCalendar = [(2020,1,[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]),(2020,2,[1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1])]}  -}
  --qyearc :: YearCalendar -> DaysCalendar a
  qyearc :: YearCalendar ->  DaysCalendar a -> DaysCalendar a
  qyearc !year !dayscal = case lenyc of
                             0  -> empty
                             1  -> qyearc' (V.head v) year
                             _  -> (qyearc' (V.head v) year) <> (qyearc year (DaysCalendar $ V.tail v))
      where
          v = unDaysCalendar dayscal
          lenyc = V.length v
          qyearc' :: (YearCalendar, MonthCalendar, V.Vector a) -> YearCalendar-> DaysCalendar a
          qyearc' tdc _ = ans
              where
                  ans = if (year==fst3 tdc) then singleton tdc else empty

  -- | Get all DaysCalendar items belonging to the given year and month
    --
    -- @
    --    qmonthc 2017 2 dc
    --    DaysCalendar []
    -- @
    --
    -- @
    --    qmonthc 2020 1 dc
    --    DaysCalendar {unDaysCalendar = [(2020,2,[1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1])]}
    -- @
  qmonthc :: YearCalendar -> MonthCalendar -> DaysCalendar a -> DaysCalendar a
  qmonthc !year !month !dayscal = case lenyc of
                                    0  -> empty
                                    1  -> qmonthc' (V.head v) year month
                                    _  -> (qmonthc' (V.head v) year month) <> (qmonthc year month (DaysCalendar $ V.tail v))
      where
          v = unDaysCalendar dayscal
          lenyc = V.length v
          qmonthc' :: (YearCalendar, MonthCalendar, V.Vector a) -> YearCalendar -> MonthCalendar -> DaysCalendar a
          qmonthc' tdc yyyy mm = ans
              where
                  ans = if yyyy==fst3 tdc && mm==snd3 tdc then singleton tdc else empty

  -- | Extract every year you present at a given DaysCalendar
  -- Returns an ordered list of unique items
  -- eyc: extractYearsCalendar
  eyc :: DaysCalendar a -> V.Vector YearCalendar
  eyc (DaysCalendar !d) = sort . nub $ fmap fst3 d
  --sort . nub $ fmap (tft3)

 -- | Extract every single year and month from a 'DaysCalendar'
 -- eymc: extractYearsMonthCalendar
  eymc :: DaysCalendar a -> V.Vector (YearCalendar, MonthCalendar)
  {-# INLINE eymc #-}
  eymc (DaysCalendar d) = sort . nub $ fmap tft3 d

  -- | Extract all the months contained in a calendar year of 'DaysCalendar'
  -- emyc: extractMonthsOfYearCalendar
  emyc :: YearCalendar -> DaysCalendar a -> V.Vector MonthCalendar
  emyc !year !dayscal = sort . nub $ sndpart
    where
        (DaysCalendar v) = qyearc year dayscal
        sndpart = fmap snd3 v

  -- | It extracts all the defined calendar days for a given calendar year and month.
  -- edmc: extractDaysOfMonthCalendar
  edmc :: (Eq a) => YearCalendar -> MonthCalendar -> DaysCalendar a ->  V.Vector (V.Vector a)
  edmc !year !month !dayscal = nub $ fmap thr3 mcal
    where
        mcal = unDaysCalendar $ qmonthc year month dayscal

  dropydc :: YearCalendar -> DaysCalendar a -> DaysCalendar a
  dropydc y dc = DaysCalendar $ (V.filter (\x -> ((fst3 x)/=y)) (unDaysCalendar dc))

  dropmdc :: MonthCalendar -> DaysCalendar a -> DaysCalendar a
  dropmdc !m !dc = DaysCalendar $ (V.filter (\x -> ((snd3 x)/=m)) (unDaysCalendar dc))

  dropymdc :: YearCalendar -> MonthCalendar -> DaysCalendar a -> DaysCalendar a
  dropymdc !y !m !dc = DaysCalendar $ (V.filter (\x -> not ((fst3 x)==y && (snd3 x)==m)) (unDaysCalendar dc))

  resume :: DaysCalendarOp -> DaysCalendar BiDay -> DaysCalendar BiDay
  resume !mth !dc = case lenuy of
                               0 -> empty
                               1 -> resumeYm (V.head uniqyl)
                               _ -> (resumeYm (V.head uniqyl)) <> resumeNext (V.tail uniqyl)
      where
          uniqyl = eymc dc
          lenuy = V.length uniqyl

          resumeNext :: V.Vector (YearCalendar, MonthCalendar) -> DaysCalendar BiDay
          resumeNext vny = case V.length vny of
                                 0 -> empty
                                 1 -> resumeYm (V.head vny)
                                 _ -> resumeYm (V.head vny) <> resumeNext (V.tail vny)

          resumeYm :: (YearCalendar, MonthCalendar) -> DaysCalendar BiDay
          resumeYm tym = case lend of
                              0 -> empty
                              1 -> singleton (y, m, fullyd_ nd 0 $ V.head qymd)
                              _ -> singleton (y, m, resumeItself V.empty nd qymd mth)
            where
              qymd = edmc y m dc
              lend = V.length qymd
              y = fst tym
              m = snd tym
              nd = nDays y m

  -- | Resume or normalize the elements of a given DaysCalendar type.
  -- It is an application of the function 'resume' with 'DcOr' method.
  normalize :: DaysCalendar BiDay -> DaysCalendar BiDay
  {-# INLINE normalize #-}
  normalize = resume DcOr

  -- | Merges the elements of DaysCalendar based on the AND operator.
  -- Join two DaysCalendar based on 'DcAnd' operator
  and :: DaysCalendar BiDay -> DaysCalendar BiDay -> DaysCalendar BiDay
  {-# INLINE and #-}
  and !dc1 !dc2 = resume DcAnd $ dc1 <> dc2

  -- | Find any match between the elements of two 'DaysCalendar' types
  match :: DaysCalendar BiDay -> DaysCalendar BiDay -> DaysCalendar BiDay
  {-# INLINE match #-}
  match !dc1 !dc2 = resume DcMatchAny $ dc1 <> dc2

  -- | Operación de agrega (suma) BiDay
  match_ ::  BiDay -> BiDay -> BiDay
  match_ bd1 bd2
    | bd1==1 && bd2==1  = 1
    | bd1==0 && bd2==0  = 1
    | otherwise         = 0

  matchc ::  V.Vector BiDay -> V.Vector BiDay -> V.Vector BiDay
  matchc = applyOperator match_

  -- | Reverse DaysCalendar by year and month
  reverse :: DaysCalendar a  -> DaysCalendar a --V.Vector (YearCalendar, MonthCalendar, V.Vector a)
  reverse dc = DaysCalendar $ V.reverse (unDaysCalendar dc)

  -- | Complete a BiDay vector by incorporating the BiDay value until the n elements after the last value are completed
  -- 'n' is the number of elements
  -- 'k' is the value of the new elements
  -- 'v' is the vector of BiDay
  fullyd_ :: Int -> BiDay -> V.Vector BiDay -> V.Vector BiDay
  {-# INLINE fullyd_ #-}
  fullyd_ n k !v = case compare ni 0 of
                        EQ -> v
                        LT -> V.take n v
                        GT -> v V.++ V.replicate ni k
    where
      ni = n - (V.length v)

  -- |  Mark (or replace) all DaysCalendar elements with values 1
  ones :: DaysCalendar a -> DaysCalendar BiDay
  {-# INLINE ones #-}
  ones !dc = DaysCalendar $ fmap (\(x,y) -> (x,y, ones_ $ nDays x y)) uqy
    where
      uqy = eymc dc

  ones_ :: Int -> V.Vector BiDay
  ones_ n = V.replicate n 1

  -- | Uncheck (or replace) all DaysCalendar items with 0 values
  zeros :: DaysCalendar a -> DaysCalendar BiDay
  {-# INLINE zeros #-}
  zeros !dc = DaysCalendar $ fmap (\(x,y) -> (x,y, zeros_ $ nDays x y)) uqy
    where
      uqy = eymc dc

  zeros_ :: Int -> V.Vector BiDay
  zeros_ n = V.replicate n 0

  -- | Función paso escalón
  -- p.ej.: step_ 2 10 1 genera -> [1,1,0,0,1,1,0,0,1,1]
  -- s es el paso
  -- v es el valor inicial del escalón (0 ó 1)
  -- n es la cantidad de valores a generar
  step_ ::  BiDay -> Int -> Int -> V.Vector BiDay
  step_ _ 0 _ = V.empty
  step_ _ _ 0 = V.empty
  step_ v s n = replica_ n pattrn
    where
      !pattrn = replicate s v <> replicate s (invert_ v)

  -- | Generates a step with 'v' 's' 'n' features throughout 'DaysCalendar'
  -- 'v' is the initial value of the step (0 or 1)
  -- 's' is the step
  -- 'n' is the number of values to be generated
  step :: BiDay -> Int -> Int -> DaysCalendar BiDay -> DaysCalendar BiDay
  {-# INLINE step #-}
  step !v !s !n !dc = normalize $ DaysCalendar (fmap (\(x,y,_) -> (x,y, step_ v s n)) (unDaysCalendar dc))

  -- | Step step starting with values 1
  --stepones_ ::  Int -> Int -> V.Vector BiDay
  --stepones_ = step_ 1

  -- | Step step starting with values 0
  --stepzeros_ :: Int -> Int -> V.Vector BiDay
  --stepzeros_ = step_ 0

  pulse :: BiDay -> Int -> Int -> Int -> DaysCalendar a -> DaysCalendar BiDay
  {-# INLINE pulse #-}
  pulse s f p n !dc = normalize $ DaysCalendar (fmap (\(x,y,_) -> (x,y, pulse_ s f p n)) (unDaysCalendar dc))

  -- | Pulse length 'n' value 's' with frequency 'f' from position 'p
  -- 's' is the value of the signal or pulse
  -- 'f' is the frequency of the pulse along the resulting vector
  -- 'p' is the initial position in which the pulse will start to appear
  -- 'n' is the final size of the pulse vector
  pulse_ :: (Num Int) => BiDay -> Int -> Int -> Int -> V.Vector BiDay
  pulse_ s f p n = case compare (n - p1) 0 of
                         EQ -> ans
                         LT -> V.empty
                         GT -> ans
    where
      p1 = if p == 0 then 1 else p --si la posición entrada es 0 se establece en 0
      vb = replica_ n [invert_ s]
      frc = if f/=0 then mkDfrec p1 f n else [p1]
      ans = update_ frc s vb

  -- | Unitary pulse, the signal 's' appears only once along the resulting vector
  --pulse1_ :: (Num Int) => BiDay -> Int -> Int -> V.Vector BiDay
  --pulse1_ s p n = pulse_ s 0 p n

  section :: BiDay -> (IdDay, IdDay) -> Int -> DaysCalendar a -> DaysCalendar BiDay
  {-# INLINE section #-}
  section v (ti, te) n dc = normalize $ DaysCalendar (fmap (\(x,y,_) -> (x,y, section_ v (ti, te) n)) (unDaysCalendar dc))

  -- | Function defined by sections
  -- 'v' is the value of the leg
  -- 't' is the tuple with the stretch (from, to)
  -- 'n' is the final size of the leg vector
  --section_ :: (Num idDay2BiDay) => BiDay -> (IdDay, IdDay) -> Int -> V.Vector BiDay
  section_ :: BiDay -> (IdDay, IdDay) -> Int -> V.Vector BiDay
  section_ _ _ 0 = V.empty
  section_ v (ti, tf) n = case compare (tf - ti) 0 of
                                EQ -> pulse_ v 0 tf n
                                LT -> V.empty
                                GT -> gta
    where
      vb = replica_ n [invert_ v]
      gta = update_ [ti..tf] v vb

  -- | Build a vector with the 'IdDay' frequency distribution
  mkDfrec :: BiDay -> Int -> Int -> [IdDay]
  mkDfrec p 0 _ = [p] --If the frequency is 0 then return the position
  mkDfrec _ _ 0 = []  --If the resulting sample size is 0
  mkDfrec p f n = takeWhile (<=n) $ scanl (\acc _ -> acc+f) p stl
    where
      !stl = replicate ((div n f)+1) r
      r :: IdDay
      !r = 0

  -- | Return 'DaysCalendar' with the odd days of the 'DaysCalendar' entry
  -- 9999 12 31 as infinite
  oddd :: DaysCalendar BiDay -> DaysCalendar BiDay
  {-# INLINE oddd #-}
  oddd !dc = fromDates $ V.filter (/= toDay 9999 12 31) odds
    where
      dt = toDates dc
      odds = fmap (\x -> if odd $ thr3 (toGregorian x) then x else toDay 9999 12 31) dt

  -- | Return 'DaysCalendar' with even numbered days from the 'DaysCalendar' entry
  evend :: DaysCalendar BiDay -> DaysCalendar BiDay
  {-# INLINE evend #-}
  evend !dc = fromDates $ V.filter (/= toDay 9999 12 31) odds
    where
      dt = toDates dc
      odds = fmap (\x -> if even $ thr3 (toGregorian x) then x else toDay 9999 12 31) dt

  -- | Replicates 'n' times the pattern 'p' within the 'DaysCalendar
  -- The pattern is a 'BiDay' type list
  replica :: Int -> [BiDay] -> DaysCalendar BiDay -> DaysCalendar BiDay
  {-# INLINE replica #-}
  replica n p !dc = normalize $ DaysCalendar (fmap (\(x,y,_) -> (x,y, replica_ n p)) (unDaysCalendar dc))

  -- | Replica el patrón p hasta obtener un vector de BiDay de longitud n
  replica_ :: Int -> [BiDay] -> V.Vector BiDay
  replica_ n p = if null p || n == 0 then V.empty else ans
    where
      ans = V.fromList . take n $ concat (replicate m p)
      m' :: Num Int => Int
      m' = ceiling . fromIntegral $ quot n (length p)
      m = if m' == (0::Int) then 1 else m' + 1

  -- | Projects a finite vector on all DaysCalendar
  --TODO: Implementar
  --projection :: V.Vector BiDay -> DaysCalendar BiDay -> DaysCalendar BiDay
  --projection vd dc = empty

  -- | Build a DaysCalendar type
  -- @
  --    make 2017 4 (step_ 1 2 9)
  --
  -- @
  make :: YearCalendar -> MonthCalendar -> V.Vector BiDay -> DaysCalendar BiDay
  {-# INLINE make #-}
  make !y !m !k = normalize . singleton $ (y, m, k)

  -- | DaysCalendar Operator Type
  data DaysCalendarOp = DcOr
                      | DcAnd
                      | DcAdd
                      | DcSustract
                      | DcMatchAny
                      deriving (Show, Eq, Read)

  -- | Folds the items on a list according to the calendar operator to be applied  --En este caso ajoinc
  resumeItself ::  V.Vector BiDay -> Int -> V.Vector (V.Vector BiDay) -> DaysCalendarOp -> V.Vector BiDay
  {-# INLINE resumeItself #-}
  resumeItself !acc !nd !vv !mth = case lwe of
                                  0 -> acc
                                  1 -> rsi (V.head vv)
                                  _ -> resumeItself (rsi (V.head vv)) nd (V.tail vv) mth
    where
      !lwe = V.length (V.takeWhile (/= V.empty) vv)
      rsi !x = case mth of
                   DcOr -> fullyd_ nd 0 $ ordc acc x
                   DcAnd -> fullyd_ nd 0 $ anddc acc x
                   DcSustract -> fullyd_ nd 0 $ sustractc acc x
                   DcAdd -> fullyd_ nd 0 $ addc acc x
                   DcMatchAny -> fullyd_ nd 0 $ matchc acc x

  -- | Applies the operator function 'h' between two types 'DaysCalendar 
  --{-# INLINE applyOperator #-}
  applyOperator :: Eq b => (b -> b -> b) -> V.Vector b -> V.Vector b -> V.Vector b
  {-# INLINE applyOperator #-}
  applyOperator !h !v1 !v2
    | (v1==V.empty) && (v2==V.empty)        = V.empty
    | (v1/=V.empty) && (v2==V.empty)        = v1
    | (v1==V.empty) && (v2/=V.empty)        = v2
    | otherwise                             = V.zipWith h v1 v2

  -- | Join de DaysCalendar basado en el operador DcOr
  or :: DaysCalendar BiDay -> DaysCalendar BiDay -> DaysCalendar BiDay
  {-# INLINE or #-}
  or !dc1 !dc2 = resume DcOr $ dc1 <> dc2

  -- | Atomic join of calendars
  -- Lists can have different sizes, result is returned with size of the smallest  ordc ::  V.Vector BiDay -> V.Vector BiDay -> V.Vector BiDay
  ordc :: V.Vector BiDay -> V.Vector BiDay -> V.Vector BiDay
  ordc = applyOperator ordc_

  -- | Joint operator for Calendar Day
  -- (Old ojoinc)
  ordc_ ::  BiDay -> BiDay -> BiDay
  {-# INLINE ordc #-}
  ordc_ dc1 dc2
    | dc1' == 1 || dc2' == 1        = 1     --Si alguno de los dos es 1 entonces 1
    | dc1' == 0 && dc2' == 0        = 0     --Si ambos son 0 entonces 0
    | dc1' < 0 || dc2' < 0          = 0     --Si alguno es negativo, entonces 0
    | dc1' > 1 || dc2' > 1          = 1     --Si cualquiera de los dos es mayor a 1 entonces 1
    | otherwise                     = 0     --Cualquier otro caso considerar 0
    where
      dc1' = if dc1 >=1 then 1::BiDay else 0::BiDay
      dc2' = if dc2 >=1 then 1::BiDay else 0::BiDay

  -- | Invert DaysCalendar
  invert :: DaysCalendar BiDay -> DaysCalendar BiDay
  invert = fmap invert_

  -- | Inversion values of a BiDay vector
  --invertd ::  V.Vector BiDay -> V.Vector BiDay
  --invertd dc = if dc==V.empty then V.empty else fmap invert_ dc

  -- | BiDay investment operator
  invert_ ::  BiDay -> BiDay
  invert_ dc
    | dc == 0  = 1
    | dc == 1  = 0
    | dc <= 0  = 1
    | dc >= 1  = 0

  -- | Join de DaysCalendar basado en el operador DcSustract
  sustract :: DaysCalendar BiDay -> DaysCalendar BiDay -> DaysCalendar BiDay
  {-# INLINE sustract #-}
  sustract !dc1 !dc2 = resume DcSustract $ dc1<>dc2

  -- | Operación de resta o sustracción BiDay
  -- Quita la marca 1 de dc1 según corresponda dc2
  sustract_ ::  BiDay -> BiDay -> BiDay
  sustract_ bd1 bd2
    | bd1==0            = 0 --Cero, no hay nada a que restar
    | bd1==1 && bd2==1  = 0
    | bd1==1 && bd2==0  = 1 --Pues 1, no se resta nada

  sustractc ::  V.Vector BiDay -> V.Vector BiDay -> V.Vector BiDay
  sustractc = applyOperator sustract_

  -- | Join de DaysCalendar basado en el operador DcAdd
  add :: DaysCalendar BiDay -> DaysCalendar BiDay -> DaysCalendar BiDay
  {-# INLINE add #-}
  add !dc1 !dc2 = resume DcAdd $ dc1<>dc2

  -- | Operación de agrega (suma) BiDay
  add_ ::  BiDay -> BiDay -> BiDay
  {-# INLINE add_ #-}
  add_ !bd1 !bd2
    | bd1==1 || bd2==1  = 1
    | bd1==0 && bd2==1  = 1
    | bd1==0 && bd2==0  = 0

  addc :: V.Vector BiDay -> V.Vector BiDay -> V.Vector BiDay
  addc = applyOperator add_

  -- | Gets the mismatched items (or holes) between two 'DaysCalendar' types
  holes :: DaysCalendar BiDay -> DaysCalendar BiDay -> DaysCalendar BiDay
  {-# INLINE holes #-}
  holes !dc1 !dc2 = invert $ add dc1 dc2
  
  weekday_ :: YearCalendar -> MonthCalendar -> WeekDay -> V.Vector BiDay
  weekday_ y m wd = fmap (\x -> if x==wd then 1 else 0) wdi
    where
        wdi = weekDayIndexes y m

  -- | Get the weekDay index for the first day of the given month
  -- @
  --  firstWeekDay 2017 3 == 3 --The last day of the week for the month 03/2017 is Wednesday
  -- @
  --
  firstWeekDay :: YearCalendar -> MonthCalendar -> WeekDay
  firstWeekDay y m = read $ formatTime defaultTimeLocale "%u" t
    where t = makeUtcTime y m 1 0 0 0

  -- | Get the weekDay index for the last day of the given month
  -- @
  --  lastWeekDay 2017 3 == 5 --The last day of the week for the month 03/2017 is Friday
  -- @
  --
  lastWeekDay :: YearCalendar -> MonthCalendar -> WeekDay
  lastWeekDay y m = read $ formatTime defaultTimeLocale "%u" t
    where
      t = makeUtcTime y m u 0 0 0
      u = nDays y m

  -- | Get the weekDay index for the given calendar day
  weekDayIndex :: YearCalendar -> MonthCalendar -> IdDay -> WeekDay
  weekDayIndex y m d = read (formatTime defaultTimeLocale "%u" t)::WeekDay
    where t = makeUtcTime y m d 0 0 0

  makeIdDays :: YearCalendar -> MonthCalendar -> V.Vector IdDay
  makeIdDays y m = V.fromList [1..(nDays y m)]

  -- | Gets a vector with all the weeDay index values for a given month
  -- @
  --  weekDayIndexes 2017 1
  -- @
  weekDayIndexes :: YearCalendar -> MonthCalendar -> V.Vector IdDay
  weekDayIndexes y m = fmap (weekDayIndex y m) dd where dd = makeIdDays y m

  {- --TODO: Terminar esto
  --weekMonthIndex :: (Num Int) => YearCalendar -> MonthCalendar -> IdDay -> Maybe Int
  weekMonthIndex y m d = rf --lookup ws rf
    where
      ws = read (formatTime defaultTimeLocale "%V" (makeUtcTime y m d 0 0 0))::WeekMonth
      fw m' d' = read (formatTime defaultTimeLocale "%V" (makeUtcTime y m' d' 0 0 0))::WeekMonth
      rf = fmap (fw m) [1..(nDays y m)] --[(w1, 1), (w1+1, 2), (w1+2, 3), (w1+3, 4)]
  -}

  --Suma o resta días a una determinada lista de fechas en formato [Day]
  --moveDays (1) [2016-01-02,2016-01-03,2016-01-06,2016-01-07,2016-01-10,2016-01-11,2016-01-14,2016-01-15,2016-01-18,2016-01-19,2016-01-22,2016-01-23,2016-01-26,2016-01-27,2016-01-30,2016-01-31]
  --moveDays (-1) [2016-01-02,2016-01-03,2016-01-06,2016-01-07,2016-01-10,2016-01-11,2016-01-14,2016-01-15,2016-01-18,2016-01-19,2016-01-22,2016-01-23,2016-01-26,2016-01-27,2016-01-30,2016-01-31]
  moveDays :: Integer -> V.Vector Day -> V.Vector Day
  {-# INLINE moveDays #-}
  moveDays !n !d = fmap (\x -> addDays n x) d

  -- | Moves a calendar 'n' days
  -- Add or subtract 'n' days to the calendar given in 'dc'
  move :: Integer -> DaysCalendar BiDay -> DaysCalendar BiDay
  {-# INLINE move #-}
  move !n !dc = fromDates $ moveDays n (toDates dc)

  move_ :: Num BiDay => YearCalendar -> MonthCalendar -> Int -> V.Vector BiDay -> V.Vector BiDay
  {-# INLINE move_ #-}
  move_ y m n v = ans
    where
        !d = biDay2IdDay v
        !nd = nDays y m
        !nw = fmap (+n) d
        ans = V.fromList $ fmap (\x -> if x `elem` nw then 1 else 0) [1..nd]

  -- | Genera una fecha de tipo Day
  toDay :: YearCalendar -> MonthCalendar -> IdDay -> Day
  {-# INLINE toDay #-}
  toDay y m d = utctDay $ makeUtcTime y m d 0 0 0

  -- | Convierte a fechas un vector de DaysCalendar
  toDates :: DaysCalendar BiDay -> V.Vector Day
  {-# INLINE toDates #-}
  toDates !dc = ans''
    where
      dc' = normalize dc
      uym = eymc dc'

      ans = fmap (\(y,m) -> do
        let d = edmc y m dc'
        let dd = fmap (\i -> biDay2IdDay i) d
        let dy = fmap (\x -> idDay2Day y m x) dd
        dy
        ) uym

      ans' = V.concat $ V.toList ans
      ans'' = V.concat $ V.toList ans'

  -- | generates the type DaysCalendar based on a date vector
  fromDates :: V.Vector Day -> DaysCalendar BiDay
  fromDates d = normalize $ DaysCalendar dd
    where
      gd = fmap toGregorian d
      uym = nub $ fmap (\(x,m,_) -> (x,m)) gd
      dd = fmap (\(y', m') -> do
          let ci = V.filter (/=0) $ (fmap (\(y,m,x) -> if (y==y' && m==m') then x::IdDay else 0::IdDay) gd)
          let ct = (y'::YearCalendar, m'::MonthCalendar, idDay2BiDay ci)
          ct
        ) uym

  --Convierte  un vector IdDay en otro vector con las fechas en formato de fecha Day
  idDay2Day :: YearCalendar -> MonthCalendar -> V.Vector IdDay -> V.Vector Day
  idDay2Day y m v = V.fromList $ fmap (\x -> utctDay $ makeUtcTime y m x 0 0 0) vd
    where
        vd = V.toList $ V.take nd v
        nd = nDays y m

  decreaseOne :: Num a => a -> a
  decreaseOne x = x - 1 --Decrease one

  increaseOne :: Num a => a -> a
  increaseOne x = x + 1 --Increase One

  --Convierte una lista de IdDay en una lista de días BiDay
  idDay2BiDay :: V.Vector IdDay -> V.Vector BiDay
  idDay2BiDay v = foldr (\x acc -> (V.update acc (V.singleton ((decreaseOne x)::Int, 1)))) ini vi'
    where
      vi' = nub $ sort v
      ini = zeros_ $ V.maximum vi' --NOTE: El máximo está acotado el mayor elemento, sin embargo, considere nDays

  --Conviete una lista de dias calendario a una lista de índices de lista
  biDay2IdDay :: V.Vector BiDay -> V.Vector IdDay
  biDay2IdDay v = V.filter (\x -> x/=0) mpr
    where
      vr = V.indexed v
      mpr = fmap (\x -> if (snd x == 1) then increaseOne (fst x) else 0) vr

  -- | Actualiza el vector v con valores val en la lista de índices indl
  -- los valores de indl deben excluir el valor cero y no deben exceder el tamaño del vector v
  -- NOTE: Se usa el íncide 0 del vector como resguardo Safe de la función V.update
  update_ :: [IdDay] -> BiDay -> V.Vector BiDay -> V.Vector BiDay
  update_ indl val v = V.update v nv
    where
      nv = V.fromList $ fmap (\x -> if x <= lv then (decreaseOne x, val) else (0, 0)) indl
      lv = V.length v

  --Operación AND atómica de [BiDay]
  anddc ::  V.Vector BiDay -> V.Vector BiDay -> V.Vector BiDay
  anddc = applyOperator anddc_

  -- | Operador AND
  anddc_ ::  BiDay -> BiDay -> BiDay
  anddc_ dc1 dc2
    | (dc1==0) && (dc2==0)  = 0
    | (dc1==1) && (dc2==1)  = 1
    | otherwise             = 0

  --Primer elemento de una tupla de tres
  fst3 :: (a, b, c) -> a
  fst3 (x, _, _) = x

  --Segundo elemento de una tupla de tres
  snd3 :: (a, b, c) -> b
  snd3 (_, y, _) = y

  --Tercer elemento de una tupla de tres
  thr3 :: (a, b, c) -> c
  thr3 (_, _, z) = z

  --Los dos primeros elementos de una tupla de tres
  tft3 :: (a, b, c) -> (a, b)
  tft3 (x, y, _) = (x, y)


  -- | Obtiene la cantidad de días máximo de un mes dado
  -- nDays y m 
  --y es year de 4 dígitos
  --m es month de 2 dígitos máximo
  nDays :: Integer -> Int -> Int
  nDays = gregorianMonthLength


  {-
  main :: IO ()
  main = do
    let dc = DaysCalendar $ V.fromList [(2017, 1, V.fromList [0,1,1,1,0]),(2017, 1, V.fromList [1,1,0,0,1]),(2017, 2, V.fromList [1,0,1,0,1])]
    let ndc = resume DcOr dc
    print ndc
    -}
