module Util where

import Numeric

padOutStrLeft :: Int -> [Char] -> [Char]
padOutStrLeft n str
  | length str < n = padOutStrLeft n $ ' ':str
  | otherwise = str

padOutStrRight :: Int -> [Char] -> [Char]
padOutStrRight n str
  | length str < n = padOutStrRight n $ str++[' ']
  | otherwise = str

ticksToSecs :: Int -> Int
ticksToSecs ticks = ceiling $ fromIntegral ticks / (10.0 :: Double)

showFloatPretty :: Float -> [Char]
showFloatPretty float = showFFloatAlt (Just 2) float ""

getProgress :: Int -> Int -> Float
getProgress current most = fromIntegral current / fromIntegral most

averageDamages :: [Int] -> Float
averageDamages damages = (fromIntegral $ sum damages) / (fromIntegral $ length damages)
