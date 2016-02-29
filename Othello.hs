import Control.Exception
import Prelude hiding(catch)
import Control.Monad -- för att använda IO
import Test.HUnit -- test framework

import Data.Array
import Data.List -- concat en annan funktion än den som finns i vanliga bibliotek

-- få ett visuelt bräde
--- få den inte att gå ut från brädet
-- få upp dagböckerna och koden på studentportalen

---- IO Gloss funktion som visar brädet som en bild.
--- räkna tomma rutor å stenar
-- pass funktion



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


-- emptyBoard makes a 8x8 board (8 lists with 8 elements each within a list)
emptyBoard :: Playfield
emptyBoard = replicate 8 (replicate 8 Empty) -- blir bara en lång rad (inte kolumner å rader)


startBoard :: Playfield
startBoard = (replaceCell emptyBoard (3,3) (Stone)) 
    --putStrLn "Hello! Welcome to reversi!"
--    board1 <- (replaceCell emptyBoard (3,3) (Stone)) -- Vit
--    board2 <- (replaceCell board1 (3,4) (Stone)) -- Svart
--    board3 <- (replaceCell board2 (4,4) (Stone)) -- Svart
--    board4 <- (replaceCell board3 (4,3) (Stone)) -- Vit
--    return board4



-- HELLLLLLLLLP MEEEEH
victory :: Playfield-> Bool
victory pf = undefined


-- Opposite of Stone and Empty
oppositeCell :: Cell -> Cell
oppositeCell Stone = Empty
oppositeCell Empty = Stone

-- Opposite of Black and White
oppositeColor :: Stone -> Stone
oppositeColor White = Black
oppositeColor Black = White

-- kollar om cellen är Empty
-- Ex: isEmpty [[Empty,Empty],[Stone,Empty]] (1,0) -> False
--	   isEmpty [[Empty,Empty],[Stone,Empty]] (1,1) -> True
isEmpty :: Playfield -> Pos -> Bool
isEmpty board (xlist,yelem) = 
    if (getCell board (xlist,yelem)) == Empty 
    then True 
    else False 


-- tar fram vad som finns i den positionen
-- PRE: xlist,yelem >= 0, xlist,yelem < length board
-- Ex: getCell [[Empty,Empty],[Empty,Empty],[Stone, Empty]] (2,1) -> Empty
--     getCell [[Empty,Empty],[Empty,Empty],[Stone, Empty]] (2,0) -> Stone
-- Klagar på att index (Pos) är för stor eller för liten.
-- Ex: getCell [[Empty,Empty]] (1,1) -> Exception: Prelude.!!: index too large
getCell :: Playfield -> Pos -> Cell
getCell board (xlist,yelem) = board!!xlist!!yelem
{-    if and [xlist >=0 && yelem >=0, xlist <= length board && yelem <= length board]
    then board!!xlist!!yelem
    else insert maybe
-}


-- replaceCell byter ut ett element på Pos
-- PRE: får inte vara en stone där sen tidigare.
replaceCell :: Playfield -> Pos -> Cell -> Playfield
replaceCell board (xlist,yelem) c  = replaceCell' board xlist (replaceCell' (board!!xlist) yelem c)

-- replaceCell' (auxiliary funktion) gör allt jobb åt replaceCell
replaceCell' :: [a] -> Int -> a -> [a]
replaceCell' board i c =
    if and [i >= 0, i < 9]    --isEmpty måste vara med!
    then take i board ++ (c: (drop (i+1) board))
    else board 

{-
-- kopierat från Nim.hs
-- getLine sen använda read för att få fram rätt typ.
-- Fattar inte riktigt va den gör.
-- PRE: Pos>=0 && Pos < length gameState
readMove :: IO Pos
readMove = do
    catch (do
        line <- getLine 
        evaluate (read line))  -- evaluate required to force conversion of line to Move
        ((\_ -> do   -- exception handler
            putStrLn "Invalid input. Correct format: (x,y). Give a new one."
            readMove) :: SomeException -> IO Pos)

readSize :: IO Int
readSize = do
    catch (do
        line <- getLine 
        evaluate (read line))  -- evaluate required to force conversion of line to Move
        ((\_ -> do   -- exception handler
            putStrLn "Invalid input. Correct format: Int. Give a new one"
            readSize) :: SomeException -> IO Int)


-- inspiration från Nim.hs
playerOne :: Playfield -> IO Playfield
playerOne gameState = do
    putStrLn "Your move player 1."
    move <- readMove  -- move >= 0 && move < length board (antingen här lr i getCell funktionen)
    if isEmpty gameState move == True
    then do
        putStrLn $ "Player 1 puts a stone at " ++ show (move) ++ "."
        return (replaceCell gameState move (Stone))
    else do
        putStrLn $ "You can't place at " ++ show (move) ++ ". There's a "  ++ ( show (getCell gameState move)) ++ " there."
        putStrLn "Invalid move, make another one."
        playerOne gameState

playerTwo :: Playfield -> IO Playfield
playerTwo gameState = do
    putStrLn "Your move player 2."
    move <- readMove  -- move >= 0 && move < length board (antingen här lr i getCell funktionen)
    if isEmpty gameState move == True
    then do
        putStrLn $ "Player 2 puts a stone at " ++ show (move) ++ "."
        return (replaceCell gameState move (Stone))
    else do
        putStrLn $ "You can't place at " ++ show (move) ++ ". There's a "  ++ ( show (getCell gameState move)) ++ " there."
        putStrLn "Invalid move, make another one."
        playerTwo gameState


printGameState :: Playfield -> IO ()
printGameState = undefined

-- inspiration från Nim.hs
-- startar Go
main :: IO Playfield
main = do 
    putStrLn "Welcome to Go. Enter a size between 5-19"
    size <- readSize
    if and [size > 5, size < 20]
    then do
        gameState <- (startBoard size Empty)
        play gameState
    else do
        putStrLn "Invalid size. Give a new one."
        main
    
-- inspiration från Nim.hs
-- PRE: ens input>=0 && input < length board
-- play loopar spelet non-stop om man inte skriver ett lågt/högt index.
play :: Playfield -> IO Playfield
play gameState = do
    newGame <- playerOne gameState
    return newGame
    newnewGame <- playerTwo newGame
    return newnewGame
    play newnewGame

	-}

--printMove :: Pos -> IO ()
--printMove pos = putStrLn $ " Puts stone at " ++ (show pos) ++ "."

{- gameState
   Purpose: Generate a fresh game state
   Pre: None
   Post: A valid game state
   Side effect: None (at present)
-}

-- ******************** Test cases ***************
-- test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

-- ********** Hemsidor **************
-- https://github.com/haskelllive/haskelllive/blob/episode-1/Chess.hs
-- https://github.com/steshaw/hsChess
-- https://github.com/mcmaniac/Go