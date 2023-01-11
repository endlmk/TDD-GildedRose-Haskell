module GildedRose where
import Data.Ord (clamp)

type GildedRose = [Item]

data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityItem
  where
    updateQualityItem (Item name sellIn quality) =
      let
        qualityDiff = 
          case name of 
            "Sulfuras, Hand of Ragnaros" -> 0
            "Aged Brie" -> 1
            "Backstage passes to a TAFKAL80ETC concert" ->
              if 11 <= sellIn then 1
              else if 6 <= sellIn && sellIn <= 10 then 2
              else if 1 <= sellIn && sellIn <= 5 then 3
              else negate quality
            "Conjured Mana Cake" -> normalItemDegrade sellIn * 2    
            _ -> normalItemDegrade sellIn
        sellIn' =
          if name == "Sulfuras, Hand of Ragnaros" 
            then sellIn 
            else sellIn - 1
      in Item name sellIn' (clamp (0, 50) (quality + qualityDiff))

normalItemDegrade sellIn = 
  if sellIn > 0 then (-1) else (-2)