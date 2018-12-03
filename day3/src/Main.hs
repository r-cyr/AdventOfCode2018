{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.Monoid ((<>))
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Claim = Claim { getId :: !Int, getCoords :: !(Int, Int), getSize :: !(Int, Int) }
  deriving Show

parseClaim :: String -> Claim
parseClaim str = Claim id coords size
  where
    [idToken, _, coordsToken, sizeToken] = splitOn " " str
    id = read (tail idToken)
    coords = toTuple $ read <$> splitOn "," (init coordsToken)
    size = toTuple $ read <$> splitOn "x" sizeToken
    toTuple [x, y] = (x, y)

getClaimCoords :: Claim -> [(Int, Int)]
getClaimCoords claim =
  let (w, h) = getSize claim
      (x, y) = getCoords claim
   in [(x', y') | x' <- [(x + 1)..(x + w)], y' <- [(y + 1)..(y + h)]]

buildClaimsMap :: [Claim] -> Map (Int, Int) [Int]
buildClaimsMap = Map.unionsWith (<>) . fmap getClaimMap
  where
    getClaimMap claim =
      Map.fromList $ (, [getId claim]) <$> getClaimCoords claim

partOne :: [Claim] -> Int
partOne = Map.size
        . Map.filter (\x -> length x > 1)
        . buildClaimsMap

partTwo :: [Claim] -> Int
partTwo claims = getId . head $ filter isClaimNotOverlapping claims
  where
    claimsMap = buildClaimsMap claims
    isClaimNotOverlapping claim = all isNotOverlapping (getClaimCoords claim)
    isNotOverlapping x = case Map.lookup x claimsMap of
      Just claimIDs -> length claimIDs == 1
      Nothing -> False

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let claims = parseClaim <$> input
    print $ "The result for part one is: " <> (show . partOne) claims
    print $ "The result for part two is: " <> (show . partTwo) claims

