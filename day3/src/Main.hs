{-# LANGUAGE TupleSections #-}
{-# Language OverloadedStrings #-}

module Main where

import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Monoid ((<>))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

data Claim = Claim
  { getId :: Int
  , getCoords :: (Int, Int)
  , getSize :: (Int, Int)
  } deriving (Show)

type Parser = Parsec Void String

number :: Parser Int
number = L.signed (return ()) L.decimal

claimParser :: Parser Claim
claimParser = Claim <$> claimId <* " " <*> claimCoords <* " " <*> claimSize
  where
    claimId = "#" *> number
    claimCoords = "@ " *> ((,) <$> number <* "," <*> number <* ":")
    claimSize = (,) <$> number <* "x" <*> number

getClaimCoords :: Claim -> [(Int, Int)]
getClaimCoords claim =
  let (w, h) = getSize claim
      (x, y) = getCoords claim
   in [(x', y') | x' <- [(x + 1) .. (x + w)], y' <- [(y + 1) .. (y + h)]]

buildClaimsMap :: [Claim] -> Map (Int, Int) [Int]
buildClaimsMap = Map.unionsWith (<>) . fmap getClaimMap
  where
    getClaimMap claim =
      Map.fromList $ (, [getId claim]) <$> getClaimCoords claim

partOne :: [Claim] -> Int
partOne = Map.size . Map.filter (\x -> length x > 1) . buildClaimsMap

partTwo :: [Claim] -> Int
partTwo claims = getId . head $ filter isClaimNotOverlapping claims
  where
    claimsMap = buildClaimsMap claims
    isClaimNotOverlapping claim = all isNotOverlapping (getClaimCoords claim)
    isNotOverlapping x =
      case Map.lookup x claimsMap of
        Just claimIDs -> length claimIDs == 1
        Nothing -> False

parseClaims :: [String] -> [Claim]
parseClaims input = either (const []) id (traverse (parse claimParser "") input)

main :: IO ()
main = do
  claims <- parseClaims . lines <$> readFile "input.txt"
  print $ "The result for part one is: " <> (show . partOne) claims
  print $ "The result for part two is: " <> (show . partTwo) claims
