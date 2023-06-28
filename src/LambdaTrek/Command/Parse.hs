module LambdaTrek.Command.Parse where

import Control.Monad
import Data.Char
import Data.Maybe (listToMaybe)
import LambdaTrek.Command
import LambdaTrek.Units
import Text.ParserCombinators.ReadP

digit :: ReadP Int
digit = do
  ds <- munch1 isDigit
  pure $ read ds

parseEngineMove :: ReadP Command
parseEngineMove = do
  _ <- string "MOV"
  skipSpaces
  x <- digit
  skipSpaces
  y <- digit
  eof
  when (x < 0 || x > 14) $ fail "Invalid X"
  when (y < 0 || y > 14) $ fail "Invalid Y"
  pure $ EngineMove x y

parseQuadRefX :: ReadP QuadrantRefX
parseQuadRefX = do
  c <- get
  case c of
    'A' -> pure A
    'B' -> pure B
    'C' -> pure C
    'D' -> pure D
    'E' -> pure E
    'F' -> pure F
    'G' -> pure G
    'H' -> pure H
    _   -> fail $ "Invalid Quadrant X-Coordinate: " ++ [c]

parseQuadRefY :: ReadP QuadrantRefY
parseQuadRefY = do
  c <- get
  case c of
    'I' -> pure I
    'J' -> pure J
    'L' -> pure L
    'M' -> pure M
    'N' -> pure N
    'O' -> pure O
    'P' -> pure P
    'Q' -> pure Q
    _   -> fail $ "Invalid Quadrant Y-Coordinate: " ++ [c]

parseJumpMove :: ReadP Command
parseJumpMove = do
  _ <- string "JMP"
  skipSpaces
  x <- parseQuadRefX
  skipSpaces
  y <- parseQuadRefY
  eof
  pure $ JumpMove (QuadrantCoord x y)

parseCommand :: ReadP Command
parseCommand = choice [parseEngineMove, parseJumpMove]

runCommandParser :: String -> Maybe Command
runCommandParser = fmap fst . listToMaybe . readP_to_S parseCommand
