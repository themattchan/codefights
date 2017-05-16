import Data.List
import Data.Maybe
import Data.Function
import Data.Ord

packageBoxing pkg boxes
  = fromMaybe (-1)
  . fmap snd
  . listToMaybe
  . filter (fits (sort pkg) . sort . fst)
  . sortBy (comparing (product . fst))
  $ zip boxes [0..]
 where
  fits      pkg box = checkVol pkg box && checkSize pkg box
  checkVol  pkg box = product pkg <= product box
  checkSize pkg box = all id (zipWith (<=) pkg box)
