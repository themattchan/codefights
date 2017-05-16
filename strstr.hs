import Debug.Trace
import qualified Data.HashMap.Strict as HM
{-
strstr :: String -> String -> Int
strstr big target = go 0 0  big target where
  go i n _ [] = n
  go i _ [] _ = -1
  go i n b@(s:ss) t@(x:xx)
    | s == x
    = go (i+1) n ss xx

    | s /= x && t == target
    = go (i+1) (i+1) ss target -- first chars dont match, skip

    | otherwise
    = go i i b target  -- in the middle of a match and found
                       -- mismatch, try again on last char
-}

strstr = flip search

-- Boyer–Moore–Horspool
preprocess :: String -> HM.HashMap Char Int
preprocess s = HM.union replace defmap where
  l       = length s
  replace = HM.fromList $ zip s (map (\i -> l-1-i) [0..l-2])
  defmap  = HM.fromList $ map (\i -> (i,l)) [minBound..maxBound]

buildLookup :: String -> HM.HashMap Int Char
buildLookup = HM.fromList . zip [0..]

{-
 function search(needle, haystack)
    T ← preprocess(needle)
    skip ← 0
    while length(haystack) - skip ≥ length(needle)
        i ← length(needle) - 1
        while haystack[skip + i] = needle[i]
            if i = 0
                return skip
            i ← i - 1
        skip ← skip + T[haystack[skip + length(needle) - 1]]
    return not-found
-}
search :: String -> String -> Int
search needle haystack = go 0
  where
    go s | length haystack - s >= length needle
         =  undefined
         | otherwise
         = -1

    table     = preprocess  needle
    haystackT = buildLookup haystack
    needleT   = buildLookup needle


test s t e = strstr s t == e

main = do
  mapM_ print tests
  print $ length $ filter (== True) tests
  where
    tests = [ test "CodefightsIsAwesome" "IA" (-1)
            , test "CodefightsIsAwesome" "IsA" 10
            , test "a" "a" 0
            , test "a" "A" (-1)
            , test "sst" "st" 1
            , test "lrnkbldxguzgcseccinlizyogwqzlifxcthdgmanjztlt" "an" 38
            , test "ffbefbdbacbccecaceddcbcaeaebfedfcfdbeecffdbbf" "cb" 9
            , test "aBcDefghaBcdEFgh" "ghb" (-1)
            , test "abcdefghabcdefghi" "ghi" 14
            , test "fefcafcdedeceadbbdaacdbdcdaeb" "ef" 1
            , test "ATErUUeUkVFVNfxfUKtntOErKmZLUpWpHRASdxjUhzzxygmnNnKabPPgPqyvCLSCZObaNNGCXQssfEEDDJIPBwtkMmTniKapBlrd" "vCLSCZObaNNGCXQssfEEDDJIPBwtkMmTniKa" 59
            , test "IckcYWDCgWkDBMudMVWZOBatEloOzayVtvsLgUSsaaFxZQAivbqtuGerRravCLSCZObaNNGCXQssfEEDDJIPBwtkMmTniKxNrWZl" "vCLSCZObaNNGCXQssfEEDDJIPBwtkMmTniKa" (-1)
            , test "GTgpEYIWKIWrlEtByHryETrBeTWNkHutWKOCvVNRSGSxaynjzTatJMKSwCLSCZObaNNGCXQssfEEDDJIPBwtkMmTniKaKfqaOtvO" "vCLSCZObaNNGCXQssfEEDDJIPBwtkMmTniKa" (-1)
            ]
