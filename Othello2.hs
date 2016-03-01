import Control.Exception
import Prelude hiding(catch)
import Control.Monad -- för att använda IO
import Test.HUnit -- test framework

--import Data.Maybe -- fromJust $ elemIndex 3 [1,2,3,4] -> 2

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
--data Stone = Black | White deriving (Eq, Show)
data Cell = Empty | Black | White deriving (Eq, Show)
data Player = A | B

type Playfield = [[Cell]]
type Pos = (Int,Int)

{-
instance Show Cell where 
--colorToAscii :: Cell -> String    
colorToAscii color =
    case color of
        Black -> 'X'                -- ( Inget att tänka på nu, använda ascii? )
        White -> 'O'
        Empty -> '-'
-}

-- emptyBoard makes a 8x8 board (8 lists with 8 elements each within a list)
emptyBoard :: Playfield
emptyBoard = replicate 8 (replicate 8 Empty) -- blir bara en lång rad (inte kolumner å rader)

-- Puts 4 stones at 4 given position. This is the start of the game.
startBoard :: Playfield
startBoard = (replaceCell (replaceCell (replaceCell (replaceCell emptyBoard (4,3) (Black)) (4,4) (White)) (3,4) (Black)) (3,3) (White))

-- Taken from Board.hs
outside :: Pos -> Bool
outside (a, b) = a <= 0 || b <= 0 || a > 8 || b > 8
-- inside = not . outside

-- HELLLLLLLLLP MEEEEH
victory :: Playfield-> Bool
victory pf = undefined


-- Opposite of Stone and Empty
--oppositeCell :: Cell -> Cell
--oppositeCell Stone = Empty
--oppositeCell Empty = Stone

-- Opposite of Black and White
oppositeColor :: Cell -> Cell
oppositeColor White = Black
oppositeColor Black = White
oppositeColor _ = Empty

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
    if and [i >= 0, i < 8]    --isEmpty måste vara med!
    then take i board ++ (c: (drop (i+1) board))
    else board 


-- ********************************************************************************************************************
--[funktion | x <- board, filtergrej]
--getPos = if getCell b pos == White then ... else getcell b pos+1

-- Takes out all the given color's position.
getPosition :: Playfield -> Cell -> [Pos]
getPosition board color = foldl (++) [] [[(x,y) | y <- [0..7], getCell board (x,y) == color] | x <- [0..7]]

-- Är det svart runt omkring den vita ?
possibleColor :: [Pos] -> Playfield -> color [Pos]
possibleColor gP color board = possibleColor (getPosition board color) (oppositeColor color) board
{-basfall för (x,y)
(x-1,y) -> vänster
(x-1,y-1) -> vänster dia.upp
(x,y-1) -> upp
(x+1,y-1) -> höger dia.upp
(x+1,y) -> höger
(x+1,y+1) -> höger dia.ner
(x,y+1) -> ner
(x-1,y+1) -> vänster dia.ner
-}


possibleMove :: Playfield -> Cell -> [Pos]
possibleMove board color = undefined

flip = undefined

score = undefined

-- *************************************************************************************************************** 

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


-- inspiration från Nim.hs
playerOne :: Playfield -> IO Playfield
playerOne gameState = do
    putStrLn "Your move player 1."
    move <- readMove  -- move >= 0 && move < length board (antingen här lr i getCell funktionen)
    if isEmpty gameState move == True
    then do
        putStrLn $ "Player 1 puts a stone at " ++ show (move) ++ "."
        return (replaceCell gameState move (Black))
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
        return (replaceCell gameState move (White))
    else do
        putStrLn $ "You can't place at " ++ show (move) ++ ". There's a "  ++ ( show (getCell gameState move)) ++ " there."
        putStrLn "Invalid move, make another one."
        playerTwo gameState

printGameState :: Playfield -> IO ()
printGameState pf = undefined


-- inspiration från Nim.hs
-- startar Go
main :: IO ()
main = do 
    putStrLn "Welcome to Othello."
    play startBoard
    
-- inspiration från Nim.hs
-- PRE: ens input>=0 && input < length board
-- play loopar spelet non-stop om man inte skriver ett lågt/högt index.
play :: Playfield -> IO ()
play gameState = do
    newGame <- playerOne gameState
    return newGame
    newnewGame <- playerTwo newGame
    return newnewGame
    play newnewGame

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