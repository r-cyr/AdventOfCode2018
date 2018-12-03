module Main where

import Data.Monoid ((<>))
import qualified Data.Set as S

decodeInput :: String -> [Integer]
decodeInput = fmap readNumber . lines
  where
    isPositiveNumber word = head word == '+'
    readNumber word =
      let num =
           if isPositiveNumber word 
             then tail word
             else word
       in read num

partOne :: [Integer] -> Integer
partOne = sum

partTwo :: [Integer] -> Integer
partTwo freqChanges = findFirstFrequencyAppearingTwice S.empty frequencies
  where
    frequencies = scanl (+) 0 (cycle freqChanges)
    findFirstFrequencyAppearingTwice set (freq:freqs) =
      if freq `S.member` set
        then freq
        else findFirstFrequencyAppearingTwice (S.insert freq set) freqs

main :: IO ()
main = do
  input <- decodeInput <$> readFile "input.txt"
  print $ "The result for part one is: " <> show (partOne input)
  print $ "The result for part two is: " <> show (partTwo input)
