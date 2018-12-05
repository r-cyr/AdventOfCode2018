module Main where

import Data.List
import Data.Function
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid ((<>))

caseInsensitiveEqual :: Char -> Char -> Bool
caseInsensitiveEqual a b = toLower a == toLower b

react :: String -> String
react xs = if length filtered /= length xs then react filtered else xs
  where
    filtered = go [] xs
    reacts a b = caseInsensitiveEqual a b && a /= b
    myInit [] = []
    myInit xs = init xs
    myLast [] = []
    myLast xs = [last xs]
    go acc (x:y:xs) = if reacts x y then go (myInit acc) (myLast acc ++ xs) else go (acc ++ [x]) (y:xs)
    go acc [x] = acc ++ [x]
    go acc [] = acc

partOne :: String -> Int
partOne = length . react

partTwo :: String -> Int
partTwo xs = Set.findMin
           . Set.map (length . react . flip withoutUnit xs)
           $ Set.fromList ['a'..'z']
  where
    withoutUnit x = filter (not . caseInsensitiveEqual x)

main :: IO ()
main = do
  input <- init <$> readFile "input.txt"
  print $ "The result of part one is: " <> (show . partOne) input
  print $ "The result of part two is: " <> (show . partTwo) input
