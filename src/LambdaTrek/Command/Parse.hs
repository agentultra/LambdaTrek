{-# LANGUAGE OverloadedStrings #-}

{-
Module: LambdaTrek.Command.Parse

Commands are UPPERCASED before being passed to 'runCommandParser'
-}
module LambdaTrek.Command.Parse where

import Data.Char
import Data.Functor
import Data.Text (Text)
import LambdaTrek.Command
import LambdaTrek.Simulation.Ship
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
  (x, y) <- parseCoordinate
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

parsePhaserManual :: ReadP PhaserMode
parsePhaserManual = string "MANUAL" $> PhaserManual

-- phasers 23
-- phasers 23 manual
parseFirePhasers :: ReadP (Either CommandParseError Command)
parseFirePhasers = do
  _ <- string "PHASERS"
  skipSpaces
  amt <- digit
  skipSpaces
  fireMode <- option PhaserAutomatic parsePhaserManual
  eof
  pure . Right $ FirePhasers amt fireMode

parseDock :: ReadP (Either CommandParseError Command)
parseDock = do
  _ <- string "DOCK"
  eof
  pure . Right $ Dock

parseShieldsUp :: ReadP ShieldState
parseShieldsUp = do
  _ <- string "UP"
  pure ShieldsUp

parseShieldsDown :: ReadP ShieldState
parseShieldsDown = do
  _ <- string "DOWN"
  pure ShieldsDown

parseShieldState :: ReadP ShieldState
parseShieldState = choice [parseShieldsUp, parseShieldsDown]

parseShields :: ReadP (Either CommandParseError Command)
parseShields = do
  _ <- string "SHIELDS"
  skipSpaces
  state <- parseShieldState
  eof
  pure . Right $ Shields state

parseTransfer :: ReadP (Either CommandParseError Command)
parseTransfer = do
  _ <- string "TRANSFER"
  skipSpaces
  amt <- digit
  eof
  pure . Right $ Transfer amt

parseCoordinate :: ReadP (Int, Int)
parseCoordinate = do
  x <- digit
  skipSpaces
  y <- digit
  pure (x, y)

parseCoordinates :: ReadP [(Int, Int)]
parseCoordinates = sepBy1 parseCoordinate skipSpaces <* eof

parseTorpedo :: ReadP (Either CommandParseError Command)
parseTorpedo = do
  _ <- string "TORPEDO"
  skipSpaces
  amt <- digit
  skipSpaces
  coords <- parseCoordinates
  if amt < 0 || amt > 3
    then pure
         . Left
         $ InvalidFireTorpedo "Number of torpedos must be 0 > x <= 3"
    else if length coords /= amt
    then pure
         . Left
         $ InvalidFireTorpedo "Number of coordinates must match number of torpoedos"
    else pure . Right $ FireTorpedo amt coords

parseLongRangeScan :: ReadP (Either CommandParseError Command)
parseLongRangeScan = do
  _ <- string "LRS"
  skipSpaces
  coord <- parseCoordinate
  case coord of
    (x, y) | x >= 0 && x <= 3 && y >= 0 && y <= 3 ->
             pure . Right $ LongRangeScan coord
    _ -> pure . Left . InvalidLongRangeScan $ "Coordinate values must be 0, 1, 2, or 3"

parseCommand :: ReadP (Either CommandParseError Command)
parseCommand
  = choice
  [ parseEngineMove
  , parseJumpMove
  , parseFirePhasers
  , parseDock
  , parseShields
  , parseTransfer
  , parseTorpedo
  , parseLongRangeScan
  ]

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
  | InvalidFireTorpedo Text
  | InvalidLongRangeScan Text
  | NoCommand
  deriving (Eq, Show)

renderCommandParseError :: CommandParseError -> Text
renderCommandParseError (InvalidEngineMove msg) = msg
renderCommandParseError (InvalidFireTorpedo msg) = msg
renderCommandParseError (InvalidLongRangeScan msg) = msg
renderCommandParseError NoCommand = "No command"
