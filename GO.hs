import Control.Exception
import Prelude hiding(catch)
import Control.Monad

import Data.Array
import Data.List

-----------------------------------------------------
{- Ex.1
type Playfield = [[Empty]]
type Empty = Maybe Stone
data Stone = Stone SColor
data SColor = White | Black 
[
[e, e, e, e, e]
[e, e, e, e, e]
[e, e, e, e, e]
[e, e, e, e, e]
[e, e, e, e, e]
]
-}

{- Ex.2
type Kolumn = Int
type Rad = Int
type Move = (Kolumn,Rad) -- (Int,Int) som spelarens input

-----type Player = String

-- readMove :: IO Move -- reads input from player (?)
-}

---https://github.com/haskelllive/haskelllive/blob/episode-1/Chess.hs