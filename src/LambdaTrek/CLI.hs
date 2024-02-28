module LambdaTrek.CLI where

import LambdaTrek.Config
import Options.Applicative

cliParser :: Parser GameConfig
cliParser
  = GameConfig
  <$> option auto
  ( long "num-enemies"
    <> short 'e'
    <> help "Number of enemies to generate"
    <> showDefault
    <> value 20
    <> metavar "INT"
  )
  <*> pure 4
  -- TODO (james): should we expose this?
  -- May need to update generation algorithm
  -- <*> option auto
  -- ( long "enemies-per-sector"
  --   <> help "Maximum number of enemies to place per sector"
  --   <> showDefault
  --   <> value 4
  --   <> metavar "INT"
  -- )
  <*> option auto
  ( long "num-stations"
    <> short 's'
    <> help "Number of space stations to generate"
    <> showDefault
    <> value 2
    <> metavar "INT"
  )
