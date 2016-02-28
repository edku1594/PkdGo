import Control.Exception
import Prelude hiding(catch)
import Control.Monad
import Test.HUnit -- test framework

import Data.Array
import Data.List -- concat en annan funktion än den som finns i vanliga bibliotek

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
-- instance Show Cell where
-- show Empty = "-"

{-- inspiration från Nim.hs
main :: IO ()
main = do 
	putStrLn "Welcome to Go."
	playfield <- initialBoard
	play playfield
  
putStrLn "Play again? yes / no"
continue <- getLine
when (continue == "yes") $ do main
--}


-- initialBoard makes a nxn board (n lists with n elements each within a list)
initialBoard :: Int -> Cell -> IO Playfield
initialBoard n cell = replicate n (replicate n cell) -- blir bara en lång rad (inte kolumner å rader)

-- Opposite of Stone and Empty
oppositeCell :: Cell -> Cell
oppositeCell Stone = Empty
oppositeCell Empty = Stone


-- kollar om cellen är Empty
isEmpty :: Playfield -> Pos -> Bool
isEmpty board (xlist,yelem) = 
	if (getCell board (xlist,yelem)) == Empty 
	then True 
	else False 
-- Ex: isEmpty [[Empty,Empty],[Stone,Empty]] (1,0) -> False
--	   isEmpty [[Empty,Empty],[Stone,Empty]] (1,1) -> True

-- tar fram vad som finns i den positionen
--getCell :: Playfield -> Pos -> Cell
getCell board (xlist,yelem) = board!!xlist!!yelem -- (kolumn = vilken lista, rad = n:te elementet i listan)
-- Ex: getCell [[Empty,Empty],[Empty,Empty],[Stone, Empty]] (2,1) -> Empty
--     getCell [[Empty,Empty],[Empty,Empty],[Stone, Empty]] (2,0) -> Stone


-- replaceCell byter ut ett element på Pos
-- PRE: får inte vara en stone där sen tidigare.
replaceCell :: [[Cell]] -> Pos -> Cell -> [[Cell]]
replaceCell board (xlist,yelem) c  = replaceCell' board xlist (replaceCell' (board!!xlist) yelem c)

-- replaceCell' (auxiliary funktion) gör allt jobb åt replaceCell
replaceCell' :: [a] -> Int -> a -> [a]
replaceCell' board i c = 
	if and [i >= 0, i < length board]		--isEmpty måste vara med!
	then take i board ++ (c: (drop (i+1) board))
	else board

-- kopierat från Nim.hs
-- getLine sen använda read för att få fram rätt typ.
readMove :: IO Pos
readMove = do
  catch (do
    line <- getLine 
    evaluate (read line))  -- evaluate required to force conversion of line to Move
    ((\_ -> do   -- exception handler
       putStrLn "Invalid input. Correct format: (pileNumber,amount)"
       readMove) :: SomeException -> IO Pos)
	
{- inspiration från Nim.hs
placeStone :: Playfield -> Pos -> Cell -> IO Playfield
placeStone board (xlist, yelem) c = do
	putStrLn "Your move."
	move <- readMove
	if isEmpty board move
	then do
		putStrLn "Do you wanna place stone at" ++ (show move) ++ "?"
		yes <- getLine
		when (yes=="yes") $ do (replaceCell board move c)
		unless (yes=="yes") $ do 		-- Kanske inte behövs?
			putStrLn "Make another move"
			playStone board (xlist,yelem) c
		
		
		replaceCell

-}		
		

{- ******************** Test cases ***************
test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

-}	

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