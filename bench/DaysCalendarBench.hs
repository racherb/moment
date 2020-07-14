{-# LANGUAGE BangPatterns            #-}

import Gauge
import qualified Data.Vector as V
import Moment.Calendar.Internals
import Prelude hiding (or, and, reverse)
--import Control.Monad (replicateM_)
--import System.Random

cx month = step 1 1 10 $ make 2020 month (V.fromList [])
--month seed = randomR (1, 12) seed

loop :: (Eq t, Num t) => t -> DaysCalendar BiDay
{-# INLINE loop #-}
loop !n = bulk n (cx 1)
  where bulk 0 a = a
        bulk n a = bulk (n - 1) (a <> cx 2)

main :: IO ()
main =
    defaultMain $ map group [100, 1000]
  where
    group size =
        bgroup (show size)
          [ 
            bench "singleton" $ whnf singleton (2019, 12, V.fromList [])
          , bench "sort" $ whnf sort (V.fromList [1..(10^6)])
          , bench "qyearc" $ whnf qyearc2020 x
          , bench "qmonthc" $ whnf qmonthc202002 x
          , bench "normalize x" $ whnf normalize x
          , bench "normalize u<>v<>k" $ whnf normalize (u<>v<>k)
          , bench "normalize big" $ whnf normalize (loop 3000)
          , bench "and" $ whnf andx y
          , bench "or" $ whnf orx y
          , bench "match" $ whnf matchx y
          , bench "add" $ whnf addx y
          , bench "sustract" $ whnf sustractx y
          , bench "holes" $ whnf holesx y
          , bench "reverse" $ whnf reverse x
          , bench "ones" $ whnf ones x
          , bench "zeros" $ whnf zeros x
          , bench "oddd" $ whnf oddd y
          , bench "evend" $ whnf evend z
          , bench "make" $ whnf maket (V.fromList [1,0,0,0,1,1,1,0,0,0,0,1,1,1,1])
          , bench "move 1" $ whnf move1 x
          , bench "move 1000" $ whnf move1000 x
          , bench "toDates" $ whnf toDates z
          , bench "step" $ whnf step1230 y
          , bench "pulse" $ whnf pulse13430 z
          , bench "section" $ whnf section23306 y
          ]
      where
        x = step 1 1 10 $ make 2020 01 (V.fromList [])
        y = step 0 2 5 $ make 2020 01 (V.fromList [])
        z = step 1 3 20 $ make 2020 01 (V.fromList [])
        u = x<>y<>z<>x
        v = x<>y<>z<>x
        k = x<>y<>z<>x
        andx = and x
        orx = or x
        addx = add x
        sustractx = sustract x
        holesx = holes x
        matchx = Moment.Calendar.Internals.match x
        maket = make 1979 11
        move1 = move 1
        move1000 = move 1000
        step1230 = step 1 2 30
        pulse13430 = pulse 1 3 4 30
        section23306 = section 2 (3, 30) 6
        qyearc2020 = qyearc 2020
        qmonthc202002 = qmonthc 2020 02
        