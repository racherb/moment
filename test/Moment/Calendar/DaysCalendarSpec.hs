module Moment.Calendar.DaysCalendarSpec (spec) where
  import Test.Hspec
  import Moment.Calendar.Internals
  import qualified Data.Vector as V
  
  vEmpty :: String
  vEmpty = "DaysCalendar {unDaysCalendar = []}"
  
  vSingleton :: String
  vSingleton = "DaysCalendar {unDaysCalendar = [(2019,12,[])]}"
  
  vSingletonA :: String
  vSingletonA = "DaysCalendar {unDaysCalendar = [(2019,12,[1,1])]}"
  
  vNormalized :: String
  vNormalized = "DaysCalendar {unDaysCalendar = [(2020,2,[1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])]}"
  
  vZeros :: String
  vZeros = "DaysCalendar {unDaysCalendar = [(2020,1,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])]}"
  
  vOnes :: String
  vOnes = "DaysCalendar {unDaysCalendar = [(1979,12,[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1])]}"
  
  vMake :: String
  vMake = "DaysCalendar {unDaysCalendar = [(2020,3,[1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])]}"
  
  vSpecific :: String
  vSpecific = "DaysCalendar {unDaysCalendar = [(2016,12,[1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0])]}"
  
  vStep :: String
  vStep = "DaysCalendar {unDaysCalendar = [(2020,6,[1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0]),(2020,7,[1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0])]}"

  x :: DaysCalendar BiDay
  x = step 1 1 10 $ make 2020 01 (V.fromList [])
  y :: DaysCalendar BiDay
  y = step 0 2 5 $ make 2020 01 (V.fromList [])
  z :: DaysCalendar BiDay
  z = step 1 3 20 $ make 2020 01 (V.fromList [])

  vMix :: String
  vMix = "DaysCalendar {unDaysCalendar = [(2020,1,[1,1,1,1,1,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0])]}"

  vPulse :: String
  vPulse = "DaysCalendar {unDaysCalendar = [(2020,2,[0,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0])]}"
  
  j :: DaysCalendar BiDay
  j = step 0 1 10 $ make 2020 01 (V.fromList [])
  
  jDates :: V.Vector Day
  jDates = V.fromList [toDay 2020 01 02, toDay 2020 01 04, toDay 2020 01 06, toDay 2020 01 08, toDay 2020 01 10]

  spec :: Spec
  spec = do
    describe "Empty DaysCalendar" $ do
      it "returns empty DaysCalendar" $ do
        empty `shouldBe` (read $ vEmpty::(DaysCalendar BiDay))
    
    describe "DaysCalendar Constructors" $ do
      it "singleton constructor [with empty BiDay]" $ do
        (singleton (2019, 12, V.fromList [])) `shouldBe` (read $ vSingleton::(DaysCalendar BiDay))
      it "singleton constructor [with initial BiDay]" $ do
        singleton (2019, 12, V.fromList [1,1]) `shouldBe` (read $ vSingletonA::(DaysCalendar BiDay))
      it "zeros constructor" $ do
        (zeros $ singleton (2020, 01, V.fromList [])) `shouldBe` (read $ vZeros::(DaysCalendar BiDay))
      it "ones constructor" $ do
        (ones $ singleton (1979, 12, V.fromList [0,0,0,0,0,0::Int])) `shouldBe` (read $ vOnes::(DaysCalendar BiDay))
      it "make constructor" $ do
        make 2020 03 (V.fromList [1,1,1,0,1]) `shouldBe` (read $ vMake::(DaysCalendar BiDay))
      it "specific definition" $ do
          (DaysCalendar $ V.fromList [(2016,12, V.fromList [1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0::Int])]) `shouldBe` (read $ vSpecific::(DaysCalendar BiDay))
      it "step constructor" $ do
        (step 1 3 20 $ singleton (2020, 7, V.fromList []) <> singleton (2020, 6, V.fromList [])) `shouldBe` (read $ vStep::(DaysCalendar BiDay))
      it "pulse constructor" $ do
        (pulse 1 2 5 15 $ normalize $ singleton (2020, 2, V.fromList [])) `shouldBe` (read $ vPulse::(DaysCalendar BiDay))
    describe "Normalize DaysCalendar" $ do
      it "normalize [from singleton constructor]" $ do
        (normalize $ singleton (2020, 02, V.fromList [1,1,1,1,1])) `shouldBe` (read $ vNormalized::(DaysCalendar BiDay))  
      it "normalize [from x<>y<>z]" $ do
        normalize (x<>y<>z) `shouldBe` (read $ vMix::(DaysCalendar BiDay))
      it "normalize [from z<>y<>x]" $ do
        normalize (z<>y<>x) `shouldBe` (read $ vMix::(DaysCalendar BiDay))
      it "normalize [from z<>x<>y]" $ do
        normalize (z<>x<>y) `shouldBe` (read $ vMix::(DaysCalendar BiDay))

    describe "DaysCalendar Conversion" $ do
      it "to dates" $ do
        toDates j `shouldBe` jDates
      it "from dates" $ do
        fromDates jDates `shouldBe` j
    
    describe "DaysCalendar Queries" $ do
      it "by year" $ do
        qyearc 2020 (normalize (z<>x<>y)) `shouldBe` (read $ vMix::(DaysCalendar BiDay))
      it "by month" $ do
        qmonthc 2020 1 (normalize (z<>x<>y)) `shouldBe` (read $ vMix::(DaysCalendar BiDay))
