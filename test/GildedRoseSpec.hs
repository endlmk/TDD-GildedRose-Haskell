module GildedRoseSpec (spec) where

import Test.Hspec
import GildedRose

spec :: Spec
spec =
  describe "updateQuality" $ do

    it "normal item's sell in and quality is minus 1" $
       let inventory = [Item "+5 Dexterity Vest" 10 20]
           actual = updateQuality inventory
           expected = [Item "+5 Dexterity Vest" 9 19]
       in actual `shouldBe` expected
