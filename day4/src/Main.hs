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

partOne :: [Guard] -> Int
partOne guards = fst mostAsleep * mostSleptMinute (snd mostAsleep)
  where
    minutesRange = accumulateMinutes guards
    minutesMap = foldr (\(k, v) acc -> Map.insertWith (++) k v acc) Map.empty minutesRange
    mostAsleep = maximumBy (compare `on` length . snd) (Map.assocs minutesMap)
    mostSleptMinute = head . maximumBy (compare `on` length) . group . sort

--partTwo :: [Guard] -> Int
partTwo guards = fst winner * (head . snd) winner
  where
    minutesRange = accumulateMinutes guards
    minutesMap = foldr (\(k, v) acc -> Map.insertWith (++) k v acc) Map.empty minutesRange
    mostSleptMinuteTuples = fmap (\(id, xs) -> (id, mostSleptMinute xs)) (Map.assocs minutesMap)
    mostSleptMinute = maximumBy (compare `on` length) . group . sort
    winner = minimumBy (flip compare `on` length . snd) mostSleptMinuteTuples

accumulateMinutes :: [Guard] -> [(Int, [Int])]
accumulateMinutes = go 0
  where
    go _ (BeginShift id localTime:xs) = go id xs
    go id (FallsAsleep sleepTime:WakesUp wakeTime:xs) =
      (id, [(todMin . localTimeOfDay) sleepTime .. (pred . todMin . localTimeOfDay) wakeTime]):go id xs
    go id [] = []

main :: IO ()
main = do
  guards <- parseGuards <$> readFile "input.txt"
  print $ partTwo guards

