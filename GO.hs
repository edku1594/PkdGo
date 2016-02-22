import Control.Exception
import Prelude hiding(catch)
import Control.Monad
import Test.HUnit -- test framework

import Data.Array
import Data.List

-----------------------------------------------------
-- Ex.1
-- ****************** data types ********************
data Stone = Black | White
data Cell = Empty | Stone deriving (Show)

type Playfield = [[Cell]]
type Pos = (Int,Int)


{- Ex.2
type Kolumn = Int
type Rad = Int
type Move = (Kolumn,Rad) -- (Int,Int) som spelarens input

-----type Player = String

-- readMove :: IO Move -- reads input from player (?)
-}

---https://github.com/haskelllive/haskelllive/blob/episode-1/Chess.hs