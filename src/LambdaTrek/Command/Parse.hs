{-# LANGUAGE OverloadedStrings #-}

module LambdaTrek.Command.Parse where

import Data.Char
import Data.Text (Text)
import LambdaTrek.Command
import LambdaTrek.Units
import Text.ParserCombinators.ReadP

digit :: ReadP Int
digit = do
  ds <- munch1 isDigit
  pure $ read ds

parseEngineMove :: ReadP (Either CommandParseError Command)
parseEngineMove = do
  _ <- string "MOV"
  skipSpaces
  x <- digit
  skipSpaces
  y <- digit
  eof
  if x < 0 || x > 14 || y < 0 || y > 14
    then pure
         . Left
         $ InvalidEngineMove "Invalid move: X and Y must be from 0 to 14"
    else pure . Right $ EngineMove x y

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

parseJumpMove :: ReadP (Either CommandParseError Command)
parseJumpMove = do
  _ <- string "JMP"
  skipSpaces
  x <- parseQuadRefX
  skipSpaces
  y <- parseQuadRefY
  eof
  pure . Right $ JumpMove (QuadrantCoord x y)

parseCommand :: ReadP (Either CommandParseError Command)
parseCommand = choice [parseEngineMove, parseJumpMove]

runCommandParser :: String -> Either CommandParseError Command
runCommandParser = handleParseResult . readP_to_S parseCommand
  where
    handleParseResult
      :: [(Either CommandParseError Command, String)]
      -> Either CommandParseError Command
    handleParseResult [] = Left NoCommand
    handleParseResult ((result, _):_) = result

data CommandParseError
  = InvalidEngineMove Text
  | NoCommand
  deriving (Eq, Show)

renderCommandParseError :: CommandParseError -> Text
renderCommandParseError (InvalidEngineMove msg) = msg
renderCommandParseError NoCommand = "No command"
