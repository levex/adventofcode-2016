module BathroomSecurityPartTwo where

import Data.List
import Data.Maybe

type Position = (Int, Int)
type Direction = (Int, Int)

codeMap :: String
codeMap = concat
    [ "       "
    , "   1   "
    , "  234  "
    , " 56789 "
    , "  ABC  "
    , "   D   "
    , "       "
    ]

getCharAt :: Position -> Char
getCharAt (x, y)
  = codeMap !! (y * 7 + x)

getPositionOfChar :: Char -> Position
getPositionOfChar c
  = ((fromJust (elemIndex c codeMap)) `mod` 7,
     (fromJust (elemIndex c codeMap)) `div` 7)

move' :: Position -> Direction -> Position
move' (px, py) (dx, dy) = (px + dx, py + dy)

move :: Position -> Direction -> Position
move prev dir
  | getCharAt (move' prev dir) == ' ' = prev
  | otherwise                         = move' prev dir

getDirection :: Char -> Direction
getDirection 'U' = ( 0, -1)
getDirection 'L' = (-1,  0)
getDirection 'R' = ( 1,  0)
getDirection 'D' = ( 0,  1)

testInput :: String
testInput = "ULL\nRRDDD\nLURDL\nUUUUD"

startPosition :: Position
startPosition = getPositionOfChar '5'

solve' :: String -> Char -> Char
solve' str startChar
  = getCharAt $ foldl move (getPositionOfChar startChar) (map getDirection str)

solve :: [String] -> Char -> [Char]
solve [] _
  = []
solve (x : xs) c
  = solve' x c : solve xs (solve' x c)
