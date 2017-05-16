import Control.Arrow ((&&&))
import Data.List
import qualified Data.Map.Lazy as M

catalogUpdate catalog updates =
 ff $ a ++ b ++ [r]
  where
    (a,r:b)  = break ((=="root") . fst) updated
    updated  = M.toList . M.map (sort . nub)
             $ M.unionWith (++) (tomap updates) (tomap catalog)
    tomap    = M.fromListWith (++) .  map (head &&& tail)
    ff       = map (\(x,xs) -> x:xs)
