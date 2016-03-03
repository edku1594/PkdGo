import Control.Exception
import Prelude hiding(catch)
import Control.Monad -- för att använda IO
import Test.HUnit -- test framework
import System.Exit

--import Prelude hiding --(Leftt, Rightt)
--import Data.Maybe -- fromJust $ elemIndex 3 [1,2,3,4] -> 2


--{-# LANGUAGE ParallelListComp #-} --till getOppNeigh

--import Data.Array
--import Data.List -- concat en annan funktion än den som finns i vanliga bibliotek

-- få ett visuelt bräde
--- få den inte att gå ut från brädet
-- få upp dagböckerna och koden på studentportalen

---- IO Gloss funktion som visar brädet som en bild.
--- räkna tomma rutor å stenar 



-----------------------------------------------------
-- Ex.1
-- ****************** data types ********************
--data Stone = Black | White deriving (Eq, Show)
data Cell = Wall | Empty | Black | White deriving (Eq, Show)
data Player = A | B

type Playfield = [[Cell]]
type Pos = (Int,Int)

data Direction = Leftt | UpperLeft | Up | UpperRight | Rightt | LowerRight | Down | LowerLeft deriving (Eq,Show)

-- Converts cells into strings
makeString :: Cell -> String
makeString Wall = "#"
makeString Empty = "_"
makeString White = "O"
makeString Black = "X"

-- Converts strings into cells
makeCell :: String -> Cell
makeCell "#" = Wall
makeCell "_" = Empty
makeCell "O" = White
makeCell "X" = Black

-- Opposite of Black and White
oppositeColor :: Cell -> Cell
oppositeColor White = Black
oppositeColor Black = White
oppositeColor _ = Empty


-- Prints out a cell in a board
printCellAsString :: Cell -> IO()
printCellAsString a = do putChar '|'
                         putStrLn (makeString a)

--stringCellsHead :: Playfield -> [String]
--stringCellsHead xs = map makeString (head xs)

--stringCellsTail :: Playfield -> [String]
--stringCellsTail xs = map makeString (tail xs)

--stringCells :: Playfield -> Playfield
--stringCells board = map.map makeString board

--testar = do
--         map makeString [a]

--makeLineBoard :: Playfield -> String
--makeLineBoard [] = []
--makeLineBoard (x:xs) = "|" ++ show x ++ makeLineBoard xs


makeLineBoard :: [Cell] -> String -> IO()
makeLineBoard [] = []
makeLineBoard (x:xs) = do
                         putStrLn "|" ++ show x ++ makeLineBoard xs
                         printCellAsString x

stringBoard :: [[Cell]] -> String
stringBoard board = "|\n" ++ stringBoard' board

stringBoard' :: [[Cell]] -> String
stringBoard' [] = []
stringBoard' (x:xs) = makeLineBoard x ++ "|\n" ++ stringBoard' xs

putRow :: [Cell] -> IO()
putRow r = do mapM_ printCellAsString r
              putStr "|\n"

putBoard        :: [[Cell]] -> IO()
putBoard a      = do
                    putStr " _ _ _ _ _ _ _ _\n"
                    mapM_ putRow a
                    putStr ""



-- emptyBoard makes a 10x10 board (10 lists with 10 elements each within a list)
emptyBoard :: Playfield
emptyBoard = replicate 10 (replicate 10 Empty) -- blir bara en lång rad (inte kolumner å rader)

-- Puts 4 stones at 4 given position. This is the start of the game.
startBoard :: Playfield
startBoard = (replaceCell (replaceCell (replaceCell (replaceCell (replaceCell emptyBoard (0,0) (Wall))(5,4) (Black)) (5,5) (White)) (4,5) (Black)) (4,4) (White))

-- Taken from Board.hs (somewhere from gitHub, got it saved as a file)
--outside :: Pos -> Bool
--outside (a, b) = a <= 0 || b <= 0 || a > 8 || b > 8
-- inside = not . outside

-- if totStonesBlack > totStonesWhite or reverse or Eq
victory :: Playfield-> Bool
victory pf = undefined


-- Checks if it's Empty
-- Ex: isEmpty [[Empty,Empty],[Black,Empty]] (1,0) -> False
--     isEmpty [[Empty,Empty],[White,Empty]] (1,1) -> True
isEmpty :: Playfield -> Pos -> Bool
isEmpty board (xlist,yelem) = 
    if (getCell board (xlist,yelem)) == Empty 
    then True 
    else False 


-- returns what's on given position
-- PRE: xlist,yelem >= 0, xlist,yelem < length board
-- Ex: getCell [[Empty,Empty],[Empty,Empty],[White, Empty]] (2,1) -> Empty
--     getCell [[Empty,Empty],[Empty,Empty],[Black, Empty]] (2,0) -> Black
-- Index (Pos) can't be too large or too small.
-- Ex: getCell [[Empty,Empty]] (1,1) -> Exception: Prelude.!!: index too large
getCell :: Playfield -> Pos -> Cell
getCell board (xlist,yelem) = board!!xlist!!yelem
{-    if and [xlist >=0 && yelem >=0, xlist <= length board && yelem <= length board]
    then board!!xlist!!yelem
    else insert maybe
-}


-- replaceCell replaces a Cell with a given c on given position.
-- PRE: Can't already be a black or white stone on given position.
replaceCell :: Playfield -> Pos -> Cell -> Playfield
replaceCell board (xlist,yelem) c  = replaceCell' board xlist (replaceCell' (board!!xlist) yelem c)

-- replaceCell' (auxiliary funktion) does everything for replaceCell
replaceCell' :: [a] -> Int -> a -> [a]
replaceCell' board i c =
    if and [i >= 0, i < 8]    --isEmpty måste vara med!
    then take i board ++ (c: (drop (i+1) board))
    else board 


-- ********************************************************************************************************************
--[funktion | x <- board, filtergrej]

-- Takes out all the given color's position.
getPosition :: Playfield -> Cell -> [Pos]
getPosition board color = concat [[(x,y) | y <- [1..6], getCell board (x,y) == color] | x <- [1..6]]

-- Är det svart runt omkring den vita ?
--possibleColor :: [Pos] -> Playfield -> color [Pos]
--possibleColor gP board color = possibleColor (getPosition board color) (oppositeColor color) board


--[Pos] -> getN \x i xs 


-- Returns the positions (of the opposite color) that's around givin pos of a color
getOppNeigh :: Playfield -> Pos -> Cell -> [(Pos,Direction)]
getOppNeigh board pos color = concat [[(ppos,dir)] | pos <- (getPosition board color), (ppos,dir) <- (getSurrStones (pos)), (getCell board ppos) == (oppositeColor color)] -- ]
--getOppNeigh board color = foldl (++) [] [[ii] | ii <- (getListNeigh i), (getCell board ii) == (oppositeColor color) | i <- (getPosition board color)]

-- if it encounters an Empty cell at the direction then it will get its pos.
--goToDir :: Monad m -> Playfield -> Cell -> [((Pos,Direction))] -> m (Pos,Cell)
goToDir board color (((x,y),dir):gON)
    | snd ((x,y),dir) == Leftt && (isEmpty board (x-1,y)) = return ((x-1,y),(getCell board (x-1,y)))
    | snd ((x,y),dir) == Up && (isEmpty board (x,y-1)) = return ((x,y-1),(getCell board (x,y-1)))
    | snd ((x,y),dir) == Down && (isEmpty board (x,y+1)) = return ((x,y+1),(getCell board (x,y+1)))
    | snd ((x,y),dir) == Rightt && (isEmpty board (x+1,y)) = return ((x+1,y),(getCell board (x+1,y)))
    | snd ((x,y),dir) == UpperLeft && (isEmpty board (x-1,y-1)) = return ((x-1,y-1),(getCell board (x-1,y-1)))
    | snd ((x,y),dir) == UpperRight && (isEmpty board (x+1,y-1)) = return ((x+1,y-1),(getCell board (x+1,y-1)))
    | snd ((x,y),dir) == LowerLeft && (isEmpty board (x-1,y+1)) = return ((x-1,y+1),(getCell board (x-1,y+1)))
    | snd ((x,y),dir) == LowerRight && (isEmpty board (x+1,y+1)) = return ((x+1,y+1),(getCell board (x+1,y+1)))
    | otherwise = goToDir board color gON
--    | getCell board (x,y) == oppositeColor color 
--    | snd ((x,y),dir) == Leftt && ((getCell board (x-1,y)) == (oppositeColor color)) = goToDir board color gON
--    | snd ((x,y),dir) == Leftt && ((getCell board (x-1,y)) == (oppositeColor color)) = goToDir board color gON

-- binds all the base cases with a direction.
getSurrStones :: Pos -> [(Pos,Direction)]
getSurrStones (x,y) = 
    let
        (xN,xP)=(x-1,x+1)
        (yN,yP)=(y-1,y+1)
    in
        zip 
            [(xN,y),(xN,yN),(x,yN),(xP,yN),(xP,y),(xP,yP),(x,yP),(xN,yP)] 
            [Leftt,UpperLeft,Up,UpperRight,Rightt,LowerRight,Down,LowerLeft]



goDirection :: [(Pos, Direction)] -> Bool
goDirection xs = undefined



--getIndex :: [Pos] -> Int
--getIndex gLN = foldl (++) [] [gLN!!n | n <- [0..7]]

oppositeDirection :: Direction -> Direction
oppositeDirection Leftt = Rightt
oppositeDirection UpperLeft = LowerRight
oppositeDirection Up = Down
oppositeDirection UpperRight = LowerLeft
oppositeDirection Rightt = Leftt
oppositeDirection LowerRight = UpperLeft
oppositeDirection Down = Up
oppositeDirection LowerLeft = UpperRight


{-base cases for (x,y)
((x-1),y) -> left
((x-1), (y-1)) -> upper left
(x,(y-1)) -> up
((x+1),(y-1)) -> upper right
((x+1),y) -> right
((x+1),(y+1)) -> lower right
(x,(y+1)) -> down
((x-1),(y+1)) -> lower left
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
        line <- getLine 
        if line == "quit" then exitSuccess else
            catch (evaluate (read line))  -- evaluate required to force conversion of line to Move
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
    putStrLn $ stringBoard gameState
    newGame <- playerOne gameState
    putStrLn $ stringBoard newGame
    newnewGame <- playerTwo newGame
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
-- https://github.com/mcmaniac/GoT
