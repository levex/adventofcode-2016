module NoTimeForATaxiCabPartTwo where

import Text.ParserCombinators.Parsec
import Data.List
import qualified Data.Set as Set

data Direction
  = North
  | East
  | South
  | West
  deriving (Ord, Eq, Show, Enum)

data Turn
  = Right90
  | Left90
  deriving (Eq, Show)

type Steps = Int

type Instruction = (Turn, Steps)

type Position = (Int, Int)

type Me = (Position, Direction)

turn :: Direction -> Turn -> Direction
turn North Left90 = West
turn North Right90 = East
turn East Left90 = North
turn East Right90 = South
turn South Left90 = East
turn South Right90 = West
turn West Left90 = South
turn West Right90 = North

moveInDir :: Direction -> Position -> Position
moveInDir North (x, y) = (x,     y - 1)
moveInDir East (x, y)  = (x + 1, y)
moveInDir South (x, y) = (x,     y + 1)
moveInDir West (x, y)  = (x - 1, y)

instruction :: GenParser Char st Instruction
instruction = (,) <$> ((char 'R' *> pure Right90) <|> (char 'L' *> pure Left90))
                  <*> (read <$> (many1 digit))

parseInput :: String -> Either ParseError [Instruction]
parseInput str
  = parse (instruction `sepBy` (string ", ")) "<interactive>" str

move :: Me -> Instruction -> [Me]
move (pos, dir) (t, step)
  = zip newPoses (repeat newTurn) 
  where
    newTurn = turn dir t
    newPoses = foldl (\(x : xs) _ -> moveInDir newTurn x : x : xs) [pos] [1..step]

firstRepeated :: (Eq a) => [a] -> a
firstRepeated []
  = undefined
firstRepeated (x : xs)
  = case elem x xs of
      True -> x
      False -> firstRepeated xs

remdups :: (Eq a) => [a] -> [a]
remdups []
  = []
remdups (x : [])
  = [x]
remdups (x : xx : xs)
  | x == xx   = remdups (x : xs)
  | otherwise = x : remdups (xx : xs)


execInput
  = firstRepeated . remdups . map fst . reverse . execInput'

execInput' str
  = foldl move' [((0, 0), North)] parsed
  where
    move' acc i
      = move (head acc) i ++ acc
    parsed = case parseInput str of
        Left _ -> undefined
        Right ins -> ins
