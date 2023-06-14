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
  deriving (Eq, Ord, Show)

data QuadrantRefY
  = I
  | J
  | L
  | M
  | N
  | O
  | P
  | Q
  deriving (Eq, Ord, Show)


data QuadrantCoord
  = QuadrantCoord
  { quadrantCoordX :: QuadrantRefX
  , quadrantCoordY :: QuadrantRefY
  }
  deriving (Eq, Ord, Show)
