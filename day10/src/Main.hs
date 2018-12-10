{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Void
import Data.List
import Data.Function
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Monoid


type Parser = Parsec Void String

type Point = ((Int, Int), (Int, Int))

number :: Integral a => Parser a
number = L.signed (pure ()) L.decimal

parsePoint :: Parser Point
parsePoint = (,) <$ "position=<" <*> parseTuple <* "> velocity=<" <*> parseTuple <* ">"
  where
    parseTuple = (,) <$ space <*> number <* ", " <* space <*> number

parsePoints :: String -> [Point]
parsePoints = either (const []) id . runParser (parsePoint `sepEndBy` char '\n') ""

boardCoords :: [Point] -> (Int, Int, Int, Int)
boardCoords points = (,,,)
                  <$> snd . minimumBy (compare `on` snd)
                  <*> fst . maximumBy (compare `on` fst)
                  <*> snd . maximumBy (compare `on` snd)
                  <*> fst . minimumBy (compare `on` fst)
                   $ fmap fst points

boardSize :: [Point] -> (Int, Int)
boardSize points = (r - l, b - t)
  where
    (t, r, b, l) = boardCoords points

buildBoard :: [Point] -> [(Int, Int)]
buildBoard points = [(x, y) | x <- [l .. l + r - l], y <- [t .. t + b - t]]
  where
    (t, r, b, l) = boardCoords points

step :: [Point] -> [Point]
step = fmap (\((a, b), v@(x, y)) -> ((a + x, b + y), v))

createBoard :: [Point] -> String
createBoard points = unlines
                   . transpose
                   $ fmap snd <$> groupBy ((==) `on` fst . fst) boardCharacters
  where
    pointSet = Set.fromList . fmap fst $ points
    boardCharacters = ((,) <$> id <*> getCharacter) <$> buildBoard points
    getCharacter x = if x `Set.member` pointSet then '#' else '.'

partOne :: [Point] -> String
partOne points = createBoard
               . head
               . fmap snd
               . dropWhile (\(current, previous) -> (fst . boardSize) current <= (fst . boardSize) previous)
               $ zip (tail allSteps) allSteps
  where
    allSteps = iterate step points

partTwo :: [Point] -> Int
partTwo points = head
               . fmap snd
               . dropWhile (\((current, previous), sec) -> (fst . boardSize) current <= (fst . boardSize) previous)
               $ zip (zip (tail allSteps) allSteps) [0..]
  where
    allSteps = iterate step points

main :: IO ()
main = do
  points <- parsePoints <$> readFile "input.txt"
  putStrLn $ "The result for part one is: \n" <> partOne points
  putStrLn $ "The result for part two is: " <> (show . partTwo) points

