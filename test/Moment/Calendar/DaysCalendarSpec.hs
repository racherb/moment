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
        (ones $ singleton (1979, 12, V.fromList [0,0,0,0,0,0])) `shouldBe` (read $ vOnes::(DaysCalendar BiDay))
      it "make constructor" $ do
        make 2020 03 (V.fromList [1,1,1,0,1]) `shouldBe` (read $ vMake::(DaysCalendar BiDay))
      it "specific definition" $ do
          (DaysCalendar $ V.fromList [(2016,12, V.fromList [1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0])]) `shouldBe` (read $ vSpecific::(DaysCalendar BiDay))

    describe "Normalize DaysCalendar" $ do
      it "normalize [from singleton constructor]" $ do
        (normalize $ singleton (2020, 02, V.fromList [1,1,1,1,1])) `shouldBe` (read $ vNormalized::(DaysCalendar BiDay))  
