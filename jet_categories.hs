import Control.Arrow ((&&&))
import Data.List
import qualified Data.Map.Lazy as M

catalogUpdate catalog updates =
  map (uncurry (:)) (a ++ b ++ [r])
  where
    (a,r:b)  = break ((=="root") . fst) updated
    updated  = M.toList . M.map (sort . nub)
             $ M.unionWith (++) (tomap catalog) (tomap updates)
    tomap    = M.fromListWith (++) . map (head &&& tail)
