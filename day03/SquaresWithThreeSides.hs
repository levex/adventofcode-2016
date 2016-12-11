module SquaresWithThreeSides where

import Text.ParserCombinators.Parsec

type Triangle = (Int, Int, Int)

triangle :: GenParser Char st Triangle
triangle = (,,) <$> try (spaces *> (read <$> (many1 digit)))
                <*> try (spaces *> (read <$> (many1 digit)))
                <*> try (spaces *> (read <$> (many1 digit)))

parseInput :: String -> Either ParseError [Triangle]
parseInput str = parse (triangle `sepEndBy` newline) "" str

isCorrectTriangle :: Triangle -> Bool
isCorrectTriangle (a, b, c)
  = a + b > c && a + c > b && b + c > a

solve :: String -> Int
solve str
  = case parseInput str of
      Right trs -> length . filter isCorrectTriangle $ trs
      Left _ -> undefined
