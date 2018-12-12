module Main where

import Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed as UV
import Data.Vector as BV
import qualified Data.Vector as BV
import Data.Maybe
import Data.List
import Control.Applicative
import Debug.Trace
import Data.Function

powerLevel :: Int -> (Int, Int) -> Int
powerLevel serialNumber (x, y) = 
  let rackId = x + 10
      hundredDigits x = x `rem` 1000 `div` 100
   in hundredDigits (((rackId * y) + serialNumber) * rackId) - 5

generateNextCoord :: Int -> (Int, Int) -> (Int, Int)
generateNextCoord stride (x, y) = if x < stride then (x + 1, y) else (1, y + 1)

createGrid :: Int -> (Int, UV.Vector (Int, Int))
createGrid size = (size, coords)
  where
    coords = UV.iterateN (size * size) (generateNextCoord size) (1, 1)

createPowerGrid :: Int -> Int -> (Int, UV.Vector Int)
createPowerGrid serialNumber size = (size, UV.map (powerLevel serialNumber) (snd . createGrid $ size))

createSummedAreaTable :: (Int, UV.Vector Int) -> (Int, UV.Vector Int)
createSummedAreaTable grid@(size, v) = (size, UV.convert . snd $ summedAreaGrid)
  where
    grid'@(size', v') = createGrid size
    summedAreaGrid@(size'', v'') = (size, summedArea `BV.map` (BV.convert v'))
    summedArea (x, y) =
      getGridValue  (x    , y    ) grid  +
      getGridValueB (x    , y - 1) summedAreaGrid +
      getGridValueB (x - 1, y    ) summedAreaGrid -
      getGridValueB (x - 1, y - 1) summedAreaGrid

getGridValueB :: (Int, Int) -> (Int, BV.Vector Int) -> Int
getGridValueB (x, y) (size, v) = fromMaybe 0 $ liftA2 (\x' y' -> v BV.! (((y - 1) * size) + (x - 1))) (isValidOffset x) (isValidOffset y)
  where
    isValidOffset off = if off - 1 >= 0 && off - 1 < size then Just (off - 1) else Nothing

getGridValue :: (Int, Int) -> (Int, UV.Vector Int) -> Int
getGridValue (x, y) (size, v) = fromMaybe 0 $ liftA2 (\x' y' -> v UV.! (((y - 1) * size) + (x - 1))) (isValidOffset x) (isValidOffset y)
  where
    isValidOffset off = if off - 1 >= 0 && off - 1 < size then Just (off - 1) else Nothing

partOne serialNumber = largestAreaPerPoint
  where
    gridSize = 300
    grid = createPowerGrid serialNumber gridSize
    summedAreaTable = createSummedAreaTable grid
    largestAreaPerPoint = UV.maximumBy (compare `on` snd) $ UV.map findlargestArea $ snd . createGrid $ gridSize
    findlargestArea c@(x, y) =
      let largestSquare = min (gridSize - x) (gridSize - y)
          www elem =
            let a = getGridValue (x, y) summedAreaTable
                b = getGridValue (x + elem, y) summedAreaTable
                c' = getGridValue (x, y + elem) summedAreaTable
                d = getGridValue (x + elem, y + elem) summedAreaTable
             in ((c, elem), a + d - b - c')
          minVal = 0
       in UV.maximumBy (compare `on` snd) $ UV.map www $ UV.enumFromN minVal (largestSquare + 1)

main :: IO ()
main = do
  print $ partOne 5034
