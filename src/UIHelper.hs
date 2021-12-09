module UIHelper (Game(..),Direction(..), Grid, Tile, printTile, initGame, updateList, red, blue, unknown)
              -- insertRandomTile, stuckCheck, leftGrid, checkFull, scoreGrid, mainLogic,keepTrying)
        where
import Data.Maybe
import Data.List
import Data.Matrix
import Data.Ord
import Prelude

import System.Random
import Control.Monad.IO.Class
import System.IO.Unsafe

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

-- import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
-- import qualified Data.Sequence as S
-- import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

type Tile = Int
type Grid = [[Tile]]

printTile :: Tile -> String
printTile t = case t of
    8->"elephant"
    7->"lion"
    6->"tiger"
    5->"cheetah"
    4->"dog"
    3->"cat"
    2->"rat"
    1->"ant"
    0->" "
    _ ->"?" 

--- Game definitions: --
--- data Player = Red | Blue | Unknown deriving (Eq, Show)
-- 1 For red, -1 for blue, 0 for Unknown
red :: Int
red = 1
blue :: Int
blue = -1
unknown :: Int
unknown = 0

updateList :: Int->  (Int,Int) -> [[Int]] ->[[Int]]
updateList new_v (x,y) m = toLists ( setElem new_v ((x+1),(y+1)) (fromLists m) )

data Game = Game
  { _grid  :: Grid
  , _score :: Int
  , _done  :: Bool
  , _cursor:: [[Bool]]
  ,_playerMap ::[[Int]]
  , _selected :: Bool
  ,_cursorx :: Int
  ,_cursory :: Int 
  ,_player :: Int
  ,_step :: Int
  ,_winner :: Int
  } deriving (Eq, Show)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Eq, Show)

-- add options for bot later
initGame :: Game -> IO Game
initGame g= do
  pure $
    g