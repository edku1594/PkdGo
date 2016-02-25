import Control.Exception
import Prelude hiding(catch)
import Control.Monad
import Test.HUnit -- test framework

import Data.Array
import Data.List

-----------------------------------------------------
-- Ex.1
-- ****************** data types ********************
data Stone = Black | White deriving (Eq, Show)
data Cell = Empty | Stone deriving (Eq, Show)
data Player = A | B

type Playfield = [[Cell]]
type Pos = (Int,Int)

-- instance Show Stone where     
-- show Black = "X"					( Inget att tänka på nu )
-- show White = "O"

{--
main :: IO ()
main = do 
  putStrLn "Welcome to Go."					( Från Nim.hs )
  gameState <- genGameState 
  play gameState
  
putStrLn "Play again? yes / no"
continue <- getLine
when (continue == "yes") $ do main
--}


-- initialBoard makes a nxn board (n lists with n elements each within a list)
initialBoard :: Int -> Cell -> Playfield
initialBoard n cell = replicate n (replicate n cell) -- blir bara en lång rad (inte kolumner å rader)

-- Opposite of Stone and Empty
oppositeCell :: Cell -> Cell
oppositeCell Stone = Empty
oppositeCell Empty = Stone


-- kollar om cellen är Empty
isEmpty :: Playfield -> Pos -> Bool
isEmpty board (kolumn,rad) = if (getCell board (kolumn,rad)) == Empty then True else False 
-- Ex: isEmpty [[Empty,Empty],[Stone,Empty]] (1,0) -> False
--	   isEmpty [[Empty,Empty],[Stone,Empty]] (1,1) -> True

-- tar fram vad som finns i den positionen
getCell :: Playfield -> Pos -> Cell
getCell board (kolumn,rad) = board!!kolumn!!rad -- (kolumn = vilken lista, rad = n:te elementet i listan)
-- Ex: getCell [[Empty,Empty],[Empty,Empty],[Stone, Empty]] (2,1) -> Empty
--     getCell [[Empty,Empty],[Empty,Empty],[Stone, Empty]] (2,0) -> Stone


-- playStone ska sätta ut en sten (ändrar Empty till Stone på den pos)
-- playStone :: Playfield -> Pos -> [Cell] -> Playfield
-- playStone board (kolumn,rad) sten = if getCell board (kolumn,rad) == Empty then (replaceCell board (kolumn,rad) sten) else board

-- ska göra det playStone ska göra.. typ
replaceList :: Playfield -> Pos -> Cell -> [Cell] -> Playfield
replaceList board (lista,elementet) c re =
	if and [lista >= 0, lista < length board]
	then take lista board ++ (re: (drop (lista+1) board))
	else board
-- Ex: replaceCell [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] (_,_) Stone (replaceElement [Empty,Empty,Empty] (_,_) Stone) -> [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]
--	   replaceCell [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] (_,_) Stone (replaceElement [Empty,Empty,Empty] (_,_) Stone) -> [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]

-- replaceElement är i princip vad replaceCell ska göra.
replaceElement :: [Cell] -> Pos -> Cell -> [Cell]
replaceElement board (lista,elementet) c = 
	if and [elementet >= 0, elementet < length board] --getCell board (lista,elementet) == Empty måste vara med!
	then take elementet board ++ (c: (drop (elementet+1) board))
	else board
-- Ex: replaceElement [Empty,Empty,Empty] (_,0) Stone -> [Stone,Empty,Empty]
--	   replaceElement [Empty,Empty,Empty] (_,2) Stone -> [Empty,Empty,Stone]
--	   replaceElement [Empty,Empty,Empty] (_,3) Stone -> [Empty,Empty,Empty]


-- playStone' :: Playfield -> Pos -> Cell -> Cell
-- playStone' board (kolumn,rad) 


{- Ex.2
type Kolumn = Int
type Rad = Int
type Move = (Kolumn,Rad) -- (Int,Int) som spelarens input

-----type Player = String

-- readMove :: IO Move -- reads input from player (?)
-}
{-
********** Hemsidor **************
https://github.com/haskelllive/haskelllive/blob/episode-1/Chess.hs
https://github.com/steshaw/hsChess
https://github.com/mcmaniac/Go
-}