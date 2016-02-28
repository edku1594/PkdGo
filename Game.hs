import Control.Exception
import Prelude hiding(catch)
import Control.Monad -- för att använda IO
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
-- show Black = "X"					( Inget att tänka på nu, använda ascii? )
-- show White = "O"
-- instance Show Cell where
-- show Empty = "-"

{-  
putStrLn "Play again? yes / no"
continue <- getLine
when (continue == "yes") $ do main
--}


-- startBoard makes a nxn board (n lists with n elements each within a list) -- Ändra namn från initialBoard till startBoard (28/2)
startBoard :: Int -> Cell -> Playfield
startBoard n cell = return (replicate n (replicate n cell)) -- blir bara en lång rad (inte kolumner å rader)

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
-- PRE: xlist,yelem >= 0, xlist,yelem < length board
--getCell :: Playfield -> Pos -> Cell
getCell board (xlist,yelem) = board!!xlist!!yelem
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
            putStrLn "Invalid input. Correct format: (x,y)"
            readMove) :: SomeException -> IO Pos)


readSize :: IO Int
readSize = do
    catch (do
        line <- getLine 
        evaluate (read line))  -- evaluate required to force conversion of line to Move
        ((\_ -> do   -- exception handler
            putStrLn "Invalid input. Correct format: Int"
            readSize) :: SomeException -> IO Int)
	
-- inspiration från Nim.hs
placeStone :: Playfield -> IO Playfield
placeStone game = do
    putStrLn "Your move."
    move <- readMove
    if isEmpty game move == True
    then do
		--putStrLn "Sure you wanna place stone there? yes or no"
		--yes <- getLine
		--when (yes=="yes") $ do
		-- putStrLn "You put your stone at" ++
        return (replaceCell game move (Stone))
		
		--unless (yes=="no") $ do 		-- Kanske inte behövs?
		--	putStrLn "Make another move"
		--	placeStone board (xlist,yelem)
    else do
		--putStrLn "You can't place at" ++ move ++ ". There's a"  ++ ( show (getCell board move)) ++ "there."
        putStrLn "Invalid move, make another one."
        placeStone game

-- inspiration från Nim.hs
main :: IO ()
main = do 
    putStrLn "Welcome to Go."
    putStrLn "Which size would you like your board to be? 2-19"
    size <- getLine
    game <- (startBoard size Empty)
	return game
		
play game = do
	putStrLn "hello"
	newGame <- placeStone game
  
  {- if victory newGame then do
    putStrLn "Player won!"
    putStrLn ""
    putStrLn "Play again? yes / no"
    continue <- getLine
    when (continue == "yes") $ do main
   else do
    newNewGameState <- computerMove newGameState  
    if victory newNewGameState then do
      putStrLn "Computer won!"
      putStrLn ""
      putStrLn "Play again? yes / no"
      continue <- getLine
      when (continue == "yes") $ do main
     else
      play newNewGameState
-}


-- printMove :: Player -> Pos -> IO ()
-- printMove player pos = putStrLn $ " Puts stone at " ++ (show pos) "."

{- gameState
   Purpose: Generate a fresh game state
   Pre: None
   Post: A valid game state
   Side effect: None (at present)
-}
		
		

{- ******************** Test cases ***************
test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

-}	

-----type Player = String

-- readMove :: IO Move -- reads input from player (?)


-- ********** Hemsidor **************
-- https://github.com/haskelllive/haskelllive/blob/episode-1/Chess.hs
-- https://github.com/steshaw/hsChess
-- https://github.com/mcmaniac/Go