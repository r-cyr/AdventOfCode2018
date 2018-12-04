{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Time
import Data.Void
import Data.List
import Data.Function
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Monoid ((<>))

data Guard = BeginShift { identifier :: Int, time :: LocalTime }
           | FallsAsleep { time :: LocalTime }
           | WakesUp { time :: LocalTime }
           deriving Show

type Parser = Parsec Void String

number :: Integral a => Parser a
number = L.signed (return ()) L.decimal

parseGuard :: Parser Guard
parseGuard =  try (flip BeginShift <$> guardLocalTime <* " Guard #" <*> number <* " begins shift") <|>
              try (FallsAsleep <$> guardLocalTime <* " falls asleep") <|>
              try (WakesUp <$> guardLocalTime <* " wakes up")
  where
    guardLocalTime = LocalTime <$ "[" <*> guardDay <* " " <*> guardTimeOfDay <* "]"
    guardDay = fromGregorian <$> number <* "-" <*> number <* "-" <*> number
    guardTimeOfDay = (\h m -> TimeOfDay h m 0) <$> number <* ":" <*> number

parseGuards input = sortBy (compare `on` time) guards
  where
    guards = either (const []) id . traverse (runParser parseGuard "") $ lines input

minutes :: LocalTime -> Int
minutes = todMin . localTimeOfDay

minuteRange :: LocalTime -> LocalTime -> [Int]
minuteRange start end = [minutes start .. (pred . minutes) end]

minutesPerShiftMap :: [Guard] -> [(Int, [Int])]
minutesPerShiftMap = Map.assocs . foldr (uncurry (Map.insertWith (++))) Map.empty . go 0
  where
    go _ (BeginShift id _:xs) = go id xs
    go id (FallsAsleep st:WakesUp wt:xs) = (id, minuteRange st wt):go id xs
    go _ _ = []

partOne :: [Guard] -> Int
partOne = ((*) <$> fst <*> mostSleptMinute) . mostAsleep
  where
    mostAsleep = maximumBy (compare `on` length . snd) . minutesPerShiftMap
    mostSleptMinute = head . maximumBy (compare `on` length) . group . sort . snd

partTwo :: [Guard] -> Int
partTwo guards = ((*) <$> fst <*> head . snd) mostFrequentlyAsleepAtSameMin
  where
    mostSleptMinuteAggregated = fmap (\(gid, xs) -> (gid, mostSleptMinute xs)) (minutesPerShiftMap guards)
    mostSleptMinute = maximumBy (compare `on` length) . group . sort
    mostFrequentlyAsleepAtSameMin = minimumBy (flip compare `on` length . snd) mostSleptMinuteAggregated

main :: IO ()
main = do
  guards <- parseGuards <$> readFile "input.txt"
  print $ "The result of part one is: " <> (show . partOne) guards
  print $ "The result of part one is: " <> (show . partTwo) guards

