module Main where

import qualified Data.Monoid ((<>))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq((:<|)), (<|), (><))
import qualified Data.Sequence as Seq
import Data.List
import Data.Monoid ((<>))


emptyCircle :: Seq Int
emptyCircle = Seq.singleton 0

rotateClockwise :: Int -> Seq Int -> Seq Int
rotateClockwise x xs = let (a, b) = Seq.splitAt (Seq.length xs - x) xs in b >< a

rotateCClockwise :: Int -> Seq Int -> Seq Int
rotateCClockwise x xs = let (a, b) = Seq.splitAt x xs in b >< a

placeMarble :: Seq Int -> Int -> (Seq Int, Int)
placeMarble circle marble
  | isMultipleOf23 marble = (rotateClockwise 1 . removeCurrentMarble $ shifted7CounterClockwise, marble + extraScore)
  | otherwise = ((marble <|) . rotateClockwise 1 $ circle, 0)
    where
      shifted7CounterClockwise = rotateCClockwise 7 circle
      extraScore = currentMarble shifted7CounterClockwise
      isMultipleOf23 m = m `rem` 23 == 0

currentMarble :: Seq Int -> Int
currentMarble Seq.Empty = 0
currentMarble (a :<| xs) = a

removeCurrentMarble :: Seq Int -> Seq Int
removeCurrentMarble = Seq.drop 1

partOne :: Int -> Int -> Int
partOne players numberOfMarbles = maximum
                                . IntMap.elems
                                . snd
                                . foldl play (emptyCircle, IntMap.empty)
                                $ zip (cycle [1..players]) [1..numberOfMarbles]
  where
    play (circle, scores) (player, marble) =
      let (newCircle, score) = placeMarble circle marble
       in (newCircle, IntMap.insertWith (+) player score scores)

partTwo :: Int -> Int -> Int
partTwo = partOne

main :: IO ()
main = do
  print $ "The result for part one is: " <> show (partOne 426 72058)
  print $ "The result for part two is: " <> show (partTwo 426 7205800)

