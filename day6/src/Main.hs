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

boardDimensions :: Set Point -> BoardDimensions
boardDimensions = (,,,)
    <$> snd . minimumBy (compare `on` snd)
    <*> fst . maximumBy (compare `on` fst)
    <*> snd . maximumBy (compare `on` snd)
    <*> fst . minimumBy (compare `on` fst)

partOne points = succ
               . maximum
               . Map.elems
               . Map.fromListWith (+)
               . fmap (,1)
               . Map.elems
               . generateCoordMap
               . findAllPossibleCoords
               $ points
  where
    (t, r, b, l) = boardDimensions points 
    findAllPossibleCoords = Set.difference . Set.fromList $ [(x, y) | x <- [l .. l + r - l], y <- [t .. t + b - t]]
    generateCoordMap = Map.map (\[x] -> x) . Map.filter ((== 1) . length) . Map.fromSet getClosestPoint
    getClosestPoint coord = snd . fromJust . Map.lookupMin $ closestMap
      where
        closestMap = Set.foldr (\point -> Map.insertWith (++) (manhattan coord point) [point]) Map.empty points

main :: IO ()
main = do
  points <- parsePoints <$> readFile "input.txt"
  print $ partOne points

