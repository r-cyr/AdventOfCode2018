{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.Monoid ((<>))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

data Node = Node {
  header :: (Int, Int),
  children :: [Node],
  metadata :: [Int]
  } deriving Show

type Parser = Parsec Void String

number :: Integral a => Parser a
number = L.signed (pure ()) L.decimal

parseNumbers :: String -> [Int]
parseNumbers = either (const []) id . runParser parseNumberList ""
  where
    parseNumberList = number `sepEndBy` space

createTree :: [Int] -> Node
createTree = fst . go
  where
    go (qtyChildren:qtyMeta:xs) =
      (Node (qtyChildren, qtyMeta) children (take qtyMeta remains), drop qtyMeta remains)
        where
          (children, remains) =
            if qtyChildren > 0
            then foldl (\(c, r) _ -> let (c', r') = go r in (c <> [c'],r')) ([], xs) [1..qtyChildren]
            else ([], xs)

partOne :: [Int] -> Int
partOne = sumMeta . createTree
  where
    sumMeta (Node _ children meta) = sum meta + sum (sumMeta <$> children)

partTwo :: [Int] -> Int
partTwo = sumMetaWithChild . createTree
  where
    sumMetaWithChild (Node (qtyChildren, _) children meta) =
      if qtyChildren > 0
      then let indexToChildMap = Map.fromList $ zip [1..] children
               validChildren   = catMaybes $ flip Map.lookup indexToChildMap <$> meta
            in sum (sumMetaWithChild <$> validChildren)
      else sum meta

main :: IO ()
main = do
  numbers <- parseNumbers <$> readFile "input.txt"
  print $ "The result for part one is: " <> (show . partOne) numbers
  print $ "The result for part two is: " <> (show . partTwo) numbers
