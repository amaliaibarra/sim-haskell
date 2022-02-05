module Elements (Elements, isChild) where

data Elements = 
    Dirt (Int, Int) | 
    Obstacle (Int, Int) |
    Crib (Int, Int) |
    Child (Int, Int) Bool |
    Robot (Int, Int) Bool 
    deriving (Show, Eq)
