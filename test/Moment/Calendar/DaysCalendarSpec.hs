module Moment.Calendar.DaysCalendarSpec (spec) where
  import Test.Hspec
  import Moment.Calendar.Internals
  import qualified Data.Vector as V
  
  vEmpty :: String
  vEmpty = "DaysCalendar {unDaysCalendar = []}"

  spec :: Spec
  spec = do
    describe "Moment Calendar.Internals.empty" $ do
      it "returns empty DaysCalendar" $ do
        empty `shouldBe` (read $ vEmpty::(DaysCalendar Int))