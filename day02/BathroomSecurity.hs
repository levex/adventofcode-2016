module BathroomSecurity where

import Data.List

-- 1 2 3
-- 4 5 6
-- 7 8 9

move :: Int -> Char -> Int
move 1 'U' = 1
move 1 'L' = 1
move 1 'R' = 2
move 1 'D' = 4

move 2 'U' = 2
move 2 'L' = 1
move 2 'R' = 3
move 2 'D' = 5

move 3 'U' = 3
move 3 'L' = 2
move 3 'R' = 3
move 3 'D' = 6

move 4 'U' = 1
move 4 'L' = 4
move 4 'R' = 5
move 4 'D' = 7

move 5 'U' = 2
move 5 'L' = 4
move 5 'R' = 6
move 5 'D' = 8

move 6 'U' = 3
move 6 'L' = 5
move 6 'R' = 6
move 6 'D' = 9

move 7 'U' = 4
move 7 'L' = 7
move 7 'R' = 8
move 7 'D' = 7

move 8 'U' = 5
move 8 'L' = 7
move 8 'R' = 9
move 8 'D' = 8

move 9 'U' = 6
move 9 'L' = 8
move 9 'R' = 9
move 9 'D' = 9

testInput :: String
testInput = "ULL\nRRDDD\nLURDL\nUUUUD"

solve :: String -> [Int]
solve = drop 1 . reverse . solve' [5] . lines

solve'' :: Int -> String -> Int
solve'' n chars = foldl move n chars

solve' :: [Int] -> [String] -> [Int]
solve' prev []
  = prev
solve' prev (x : xs)
  = solve' ((solve'' (head prev) x) : prev) xs
