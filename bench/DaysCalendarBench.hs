import Gauge
import qualified Data.Vector as V
import Moment.Calendar.Internals

main :: IO ()
main =
    defaultMain $ map group [5, 100, 1000]
  where
    group size =
        bgroup (show size)
          [ bench "normalize x" $ whnf normalize x
          , bench "normalize u<>v<>k" $ whnf normalize (u<>v<>k)
          ]
      where
        x = step 1 1 10 $ make 2020 01 (V.fromList [])
        y = step 0 2 5 $ make 2020 01 (V.fromList [])
        z = step 1 3 20 $ make 2020 01 (V.fromList [])
        u = x<>y<>z<>x<>y<>z<>x<>y<>z<>x<>y<>z<>x<>y<>z
        v = x<>y<>z<>x<>y<>z<>x<>y<>z<>x<>y<>z<>x<>y<>z
        k = x<>y<>z<>x<>y<>z<>x<>y<>z<>x<>y<>z<>x<>y<>z
        