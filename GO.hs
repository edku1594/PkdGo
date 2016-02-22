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

-- instance Show Stone where     
-- show Black = "X"					( Inget att tänka på nu )
-- show White = "O"

-- initialBoard makes a nxn board (n lists with n elements each within a list)
initialBoard :: Int -> Cell -> Playfield
initialBoard n cell = replicate n (replicate n cell) -- blir bara en lång rad (inte kolumner å rader)

oppositeCell :: Cell -> Cell
oppositeCell Stone = Empty
oppositeCell Empty = Stone


-- kollar om cellen är tom?
-- isEmpty :: Playfield -> Pos -> Bool
-- isEmpty board (kolumn,rad) = Empty == (getCell board (kolumn,rad)) -- Den klagar på "=="

-- tar fram vad som finns i den positionen
getCell :: Playfield -> Pos -> Cell
getCell board (kolumn,rad) = board!!kolumn!!rad -- (kolumn = vilken lista, rad = n:te elementet i listan)
-- Ex: [[0,1],[2,3],[4,5]]!!1!!1 -> 3


-- playStone ska sätta ut en sten (ändrar Empty till Stone på den pos)
-- playStone :: Pos -> Cell
-- playStone (kolumn,rad) = playStone' 

-- playStone' :: Playfield -> Pos -> Cell -> Cell
-- playStone' board (kolumn,rad) 
--}

{- Ex.2
type Kolumn = Int
type Rad = Int
type Move = (Kolumn,Rad) -- (Int,Int) som spelarens input

-----type Player = String

-- readMove :: IO Move -- reads input from player (?)
-}

-- ********** Hemsidor **************
-- https://github.com/haskelllive/haskelllive/blob/episode-1/Chess.hs
-- https://github.com/steshaw/hsChess
-- https://github.com/mcmaniac/Go