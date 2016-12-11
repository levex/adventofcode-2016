module NoTimeForATaxiCab where

import Text.ParserCombinators.Parsec

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Show, Enum)

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

moveInDir :: Direction -> Steps -> Position -> Position
moveInDir North steps (x, y) = (x,         y - steps)
moveInDir East steps (x, y)  = (x + steps, y)
moveInDir South steps (x, y) = (x,         y + steps)
moveInDir West steps (x, y)  = (x - steps, y)

instruction :: GenParser Char st Instruction
instruction = (,) <$> ((char 'R' *> pure Right90) <|> (char 'L' *> pure Left90))
                  <*> (read <$> (many1 digit))

parseInput :: String -> Either ParseError [Instruction]
parseInput str
  = parse (instruction `sepBy` (string ", ")) "<interactive>" str

move :: Me -> Instruction -> Me 
move (pos, dir) (t, step)
  = (newPos, newTurn)
  where
    newTurn = turn dir t
    posAfterTurn = (pos, newTurn)
    newPos = moveInDir newTurn step pos

execInput :: String -> Me
execInput str
  = foldl move ((0, 0), North) parsed
  where
    parsed = case parseInput str of
        Left _ -> undefined
        Right ins -> ins
