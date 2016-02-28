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
