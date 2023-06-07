module LambdaTrek.Units where

data QuadrantRefX
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  deriving (Eq, Show)

data QuadrantRefY
  = I
  | J
  | L
  | M
  | N
  | O
  | P
  | Q
  deriving (Eq, Show)


data QuadrantCoord
  = QuadrantCoord
  { quadrantCoordX :: QuadrantRefX
  , quadrantCoordY :: QuadrantRefY
  }
  deriving (Eq, Show)
