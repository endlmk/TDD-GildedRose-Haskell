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
    it "minus 2 quality if sell by date has passed" $
       let inventory = [Item "+5 Dexterity Vest" 0 2]
           actual = updateQuality inventory
           expected = [Item "+5 Dexterity Vest" (-1) 0]
       in actual `shouldBe` expected
    it "quality is never negative" $
       let inventory = [Item "+5 Dexterity Vest" 0 0]
           actual = updateQuality inventory
           expected = [Item "+5 Dexterity Vest" (-1) 0]
       in actual `shouldBe` expected
    it "Aged Brie increase quality" $
       let inventory = [Item "Aged Brie" 5 0]
           actual = updateQuality inventory
           expected = [Item "Aged Brie" 4 1]
       in actual `shouldBe` expected
    it "Aged Brie quality is never more than 50" $
       let inventory = [Item "Aged Brie" 5 50]
           actual = updateQuality inventory
           expected = [Item "Aged Brie" 4 50]
       in actual `shouldBe` expected
    it "Sulfuras is never sold or decrease quality" $
       let inventory = [Item "Sulfuras, Hand of Ragnaros" 0 10]
           actual = updateQuality inventory
           expected = [Item "Sulfuras, Hand of Ragnaros" 0 10]
       in actual `shouldBe` expected
    it "Backstage passes increase quality by 1 if 11 days" $
       let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 11 10]
           actual = updateQuality inventory
           expected = [Item "Backstage passes to a TAFKAL80ETC concert" 10 11]
       in actual `shouldBe` expected
    it "Backstage passes increase quality by 2 if 10 days" $
       let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 10 11]
           actual = updateQuality inventory
           expected = [Item "Backstage passes to a TAFKAL80ETC concert" 9 13]
       in actual `shouldBe` expected
    it "Backstage passes increase quality by 2 if 6 days" $
       let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 6 11]
           actual = updateQuality inventory
           expected = [Item "Backstage passes to a TAFKAL80ETC concert" 5 13]
       in actual `shouldBe` expected
    it "Backstage passes increase quality by 3 if 5 days" $
       let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 5 11]
           actual = updateQuality inventory
           expected = [Item "Backstage passes to a TAFKAL80ETC concert" 4 14]
       in actual `shouldBe` expected
    it "Backstage passes increase quality by 3 if 1 days" $
       let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 1 11]
           actual = updateQuality inventory
           expected = [Item "Backstage passes to a TAFKAL80ETC concert" 0 14]
       in actual `shouldBe` expected
    it "Backstage passes drop quality to 0 after the concert" $
       let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 0 10]
           actual = updateQuality inventory
           expected = [Item "Backstage passes to a TAFKAL80ETC concert" (-1) 0]
       in actual `shouldBe` expected
    it "Backstage passes quality is never more than 50" $
       let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 1 48]
           actual = updateQuality inventory
           expected = [Item "Backstage passes to a TAFKAL80ETC concert" 0 50]
       in actual `shouldBe` expected
    it "Conjured degrade in 2" $
       let inventory = [Item "Conjured Mana Cake" 1 2]
           actual = updateQuality inventory
           expected = [Item "Conjured Mana Cake" 0 0]
       in actual `shouldBe` expected

