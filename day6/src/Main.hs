{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void
import qualified Data.Monoid ((<>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type Point = (Int, Int)
type BoardDimensions = (Int, Int, Int, Int)

manhattan :: Point -> Point -> Int
manhattan (x, y) (x', y') = abs(x - x') + abs(y - y')

number :: Integral a => Parser a
number = L.signed (pure ()) L.decimal

parsePoints :: String -> Set Point
parsePoints = Set.fromList . either (const []) id . traverse (runParser point "") . lines
  where
    point = (,) <$> number <* ", " <*> number

buildBoardCoordsSet :: Set Point -> Set Point
buildBoardCoordsSet points = Set.fromList [(x, y) | x <- [l .. l + r - l], y <- [t .. t + b - t]]
  where
    (t, r, b, l) = (,,,)
                <$> snd . minimumBy (compare `on` snd)
                <*> fst . maximumBy (compare `on` fst)
                <*> snd . maximumBy (compare `on` snd)
                <*> fst . minimumBy (compare `on` fst)
                 $ points

partOne :: Set Point -> Int
partOne points = findLargestArea . buildBoardCoordsSet $ points
  where
    findLargestArea = maximum . Map.elems . Map.fromListWith (+) . fmap (,1) . catMaybes . Set.foldr (\x acc -> closestPoint x:acc) []
    closestPoint coord = Map.lookupMin (Set.foldr (\point -> Map.insertWith (++) (manhattan coord point) [point]) Map.empty points)
                       >>= (\x -> if (== 1) . length . snd $ x then Just . head . snd $ x else Nothing)

partTwo :: Set Point -> Int
partTwo points = length
               . filter (< 10000)
               . fmap totalDistance
               . Set.toList . buildBoardCoordsSet
               $ points
  where
    totalDistance x = sum $ manhattan x <$> Set.toList points

main :: IO ()
main = do
  points <- parsePoints <$> readFile "input.txt"
  print $ "The result for part one is: " <> (show . partOne) points
  print $ "The result for part two is: " <> (show . partTwo) points

