module UIHelper (Game(..), Direction(..), Grid, Tile, Player(..), printTile, initGame)
              -- insertRandomTile, stuckCheck, leftGrid, checkFull, scoreGrid, mainLogic,keepTrying)
        where

import Data.Maybe
import Data.List
import Prelude

import System.Random
import Control.Monad.IO.Class
import System.IO.Unsafe

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

type Tile = Maybe String
type Grid = [[Tile]]

printTile :: Tile -> String
printTile t = case t of
 Just n -> show n
 Nothing -> " "

--- Game definitions: --
data Player = Red | Blue | Unknown deriving (Eq, Show)
-- Game State:
data Game = Game
  { _grid  :: Grid
  , _score :: Int
  , _done  :: Bool
  , _cursor:: [[Bool]]
  , _selected :: Bool
  ,_cursorx :: Int
  ,_cursory :: Int 
  ,_player :: Player
  } deriving (Eq, Show)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Eq, Show)

-- add options for bot later
initGame :: IO Game
initGame = do
  pure $
    Game { 
          _grid = [[Just "?",Just "?",Just "?",Just "?"],
                    [Just "?",Just "?",Just "?",Just "?"],
                    [Just "?",Just "?",Just "?",Just "?"],
                    [Just "?",Just "?",Just "?",Just "?"]]
        , _score = 0
        , _done = False
        ,_cursor = [[True, False, False, False],
                    [False, False, False, False],
                    [False, False, False, False],
                    [False, False, False, False]]
        ,_cursorx = 0
        ,_cursory = 0
        ,_selected = False
        ,_player = Red
        }