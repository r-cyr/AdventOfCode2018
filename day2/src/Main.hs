module Main where

import qualified Data.Map.Strict as M
import qualified Data.Monoid ((<>))

fillMap :: (Num a, Enum a) => String -> M.Map Char a
fillMap = foldr insertMap M.empty
  where
    insertMap c m =
      case M.lookup c m of
        Nothing -> M.insert c 1 m
        _ -> M.update (Just . succ) c m

aggregateMap :: (Num a, Eq a) => M.Map Char a -> (a, a)
aggregateMap = M.foldr aggregate (0, 0)
  where
    aggregate v (a, b) =
      case v of
        2 -> (1, b)
        3 -> (a, 1)
        _ -> (a, b)

partOne :: [String] -> Integer
partOne = uncurry (*) . foldr f (0, 0) . fmap (aggregateMap . fillMap)
  where
    f (a, b) (a', b') = (a + a', b + b')

partTwo :: [String] -> String
partTwo xs = head $ intersect <$> filter (nearlyEqual 1) possibleMatches
  where
    possibleMatches = [(x, y) | x <- xs, y <- xs]
    nearlyEqual factor (a, b) = length (filter id $ zipWith (/=) a b) == factor
    intersect (a, b) = fst <$> filter (uncurry (==)) (zip a b)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print $ "The result for part one is: " <> show (partOne input)
  print $ "The result for part two is: " <> partTwo input
