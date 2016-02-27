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
playStone :: Playfield -> Pos -> Cell -> IO Playfield
playStone board (xlist,yelem) c = 
		if isEmpty board (xlist,yelem) == True then do
			putStrLn "Do you want make this move yes/no?" 
			yes<-getLine 
			if (yes=="yes") then replaceCell board (xlist,yelem) c else do putStrLn "make another move" 
			playStone board (xlist,yelem) c 
		else do
			putStrLn "Stone. Invalid Move" 
			playStone board (xlist,yelem) c


-- replaceCell byter ut ett element på Pos 
--replaceCell :: [a] -> Pos -> a -> [a]
replaceCell board (xlist,yelem) c  = replaceCell' board xlist (replaceCell' (board!!xlist) yelem c)

-- replaceCell' (auxiliary funktion) gör allt jobb åt replaceCell
replaceCell' :: [a] -> Int -> a -> [a]
replaceCell' board i c = 
	if and [i >= 0, i < length board]		--getCell board (lista,elementet) == Empty måste vara med!
	then take i board ++ (c: (drop (i+1) board))
	else board

replaceEl :: [a] -> Int -> a -> [a]
replaceEl xs i x = (take i xs) ++ (x : (drop (i+1) xs))

replace :: [[a]] -> (Int, Int) -> a -> [[a]]
replace xs (y, x) s = replaceEl xs y (replaceEl (xs!!y) x s)


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