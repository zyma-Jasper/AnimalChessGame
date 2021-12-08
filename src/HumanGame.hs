{-# LANGUAGE OverloadedStrings #-}
module HumanGame (humanPlayer, Tick, Name, drawUI, theMap, move, initGame) where

import UIHelper
  (Game(..), Direction(..), Grid, Tile,Player(..), printTile, initGame)

import Data.Maybe
import Data.List
import Prelude

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor, attrName
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
-- marks passing of time
data Tick = Tick

type Name = ()

-- color attributes:

gameOverAttr, redBg, blueBg, brblBg, cyanBg, bcyanBg, yellowBg, byellowBg, greenBg, bgreenBg,  whiteBg  :: AttrName
gameOverAttr = "gameOver"
redBg = attrName "redBg"
blueBg = attrName "blueBg"
brblBg = attrName "brblBg"
cyanBg = attrName "cyanBg"
bcyanBg = attrName "bcyanBg"
magBg = attrName "magBg"
bmagBg = attrName "bmagBg"
yellowBg = attrName "yellowBg"
byellowBg = attrName "byellowBg"
greenBg = attrName "greenBg"
bgreenBg = attrName "bgreenBg"
whiteBg = attrName "whiteBg"


fst3 :: (Bool, [[String]], [[UIHelper.Player]]) -> Bool
fst3 (x1, x2, x3) = x1

snd3 :: (Bool, [[String]], [[UIHelper.Player]]) -> [[String]]
snd3 (x1, x2, x3) = x2

thd3 :: (Bool, [[String]], [[UIHelper.Player]]) -> [[UIHelper.Player]]
thd3 (x1, x2, x3) = x3

_fst3 :: (Tile, Bool, UIHelper.Player) -> Tile
_fst3 (x1, x2, x3) = x1

_snd3 :: (Tile, Bool, UIHelper.Player) -> Bool
_snd3 (x1, x2, x3) = x2

_thd3 :: (Tile, Bool, UIHelper.Player) -> UIHelper.Player
_thd3 (x1, x2, x3) = x3

_fst3_ :: ([Tile], [Bool], [UIHelper.Player]) -> [Tile]
_fst3_ (x1, x2, x3) = x1

_snd3_ :: ([Tile], [Bool], [UIHelper.Player]) -> [Bool]
_snd3_ (x1, x2, x3) = x2

_thd3_ :: ([Tile], [Bool], [UIHelper.Player]) -> [UIHelper.Player]
_thd3_ (x1, x2, x3) = x3

rotatePlayer :: UIHelper.Player->UIHelper.Player
rotatePlayer p =
  case p of 
    UIHelper.Red ->  UIHelper.Blue
    UIHelper.Blue -> UIHelper.Red

stringToTile :: [[String]] -> [[Tile]]
stringToTile ss = ss--[[Just t|t<-r]| r<-ss]

playerColor :: UIHelper.Player -> AttrName
playerColor p = case p of
  UIHelper.Red -> redBg
  UIHelper.Blue -> brblBg
  UIHelper.Unknown -> whiteBg
  

theMap :: AttrMap
theMap = attrMap V.defAttr
  [
  (gameOverAttr, fg V.red `V.withStyle` V.bold),
  (redBg, fg V.red `V.withStyle` V.bold),
  (blueBg, U.fg V.blue `V.withStyle` V.bold),
  (brblBg, U.fg V.brightBlue `V.withStyle` V.bold),
  (cyanBg, U.fg V.cyan),
  (bcyanBg, U.fg V.brightCyan),
  (yellowBg, U.fg V.yellow),
  (byellowBg, U.fg V.brightYellow),
  (magBg, U.fg V.magenta `V.withStyle` V.bold) ,
  (bmagBg, U.fg V.brightMagenta),
  (greenBg, U.fg V.green `V.withStyle` V.bold),
  (bgreenBg, U.fg V.brightGreen),
  (whiteBg, U.fg V.white `V.withStyle` V.bold)
  ]

-- define App
app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

humanPlayer :: IO ()
--- @Description: This is the function that called by the main function.
humanPlayer = do
  chan <- newBChan 10
  forkIO $ forever $ do --- while(true)
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g

gameIsOver :: (Bool,UIHelper.Player)
gameIsOver = (False, UIHelper.Unknown)

isGameOver :: Game -> Bool
isGameOver g = fst gameIsOver
  -- (checkFull (_grid g)) && (stuckCheck (_grid g))

step :: Game -> Game
step g =
  if isGameOver g then g{_done = True}
  else g

--- write down the keyboard input logic


newCursorMap :: Int->Int->[[Bool]]
--- @Description: Input the new cursor point(x,y), return the new cursop map with the point is marked as True.
--- @Param: x(Int),y(Int): point of the cursor
--- @Return: 4*4 Bool map, just the cursor point is True 
newCursorMap x y = 
  case x of
    0 -> 
      case y of
        0 ->[[True,False,False,False],[False,False,False,False],[False,False,False,False],[False,False,False,False]]
        1 ->[[False,True,False,False],[False,False,False,False],[False,False,False,False],[False,False,False,False]]
        2 ->[[False,False,True,False],[False,False,False,False],[False,False,False,False],[False,False,False,False]]
        3 ->[[False,False,False,True],[False,False,False,False],[False,False,False,False],[False,False,False,False]]
    1->
        case y of
        0 ->[[False,False,False,False],[True,False,False,False],[False,False,False,False],[False,False,False,False]]
        1 ->[[False,False,False,False],[False,True,False,False],[False,False,False,False],[False,False,False,False]]
        2 ->[[False,False,False,False],[False,False,True,False],[False,False,False,False],[False,False,False,False]]
        3 ->[[False,False,False,False],[False,False,False,True],[False,False,False,False],[False,False,False,False]]
    2->  
      case y of
        0 ->[[False,False,False,False],[False,False,False,False],[True,False,False,False],[False,False,False,False]]
        1 ->[[False,False,False,False],[False,False,False,False],[False,True,False,False],[False,False,False,False]]
        2 ->[[False,False,False,False],[False,False,False,False],[False,False,True,False],[False,False,False,False]]
        3 ->[[False,False,False,False],[False,False,False,False],[False,False,False,True],[False,False,False,False]]
    3->
      case y of
        0 ->[[False,False,False,False],[False,False,False,False],[False,False,False,False],[True,False,False,False]]
        1 ->[[False,False,False,False],[False,False,False,False],[False,False,False,False],[False,True,False,False]]
        2 ->[[False,False,False,False],[False,False,False,False],[False,False,False,False],[False,False,True,False]]
        3 ->[[False,False,False,False],[False,False,False,False],[False,False,False,False],[False,False,False,True]]

selectRequest :: Int->Int->UIHelper.Player->Bool
selectRequest x y player = True

selectLeft :: Game->Game
selectLeft  g = 
  g{_cursory=new_y, _cursor=cursor_map}
  where x = _cursorx g 
        y = _cursory g
        new_y = max (y-1) 0
        cursor_map = newCursorMap x new_y

selectRight :: Game->Game
selectRight  g = 
    g{_cursory=new_y, _cursor=cursor_map}
    where x = _cursorx g 
          y = _cursory g
          new_y = min (y+1) 3
          cursor_map = newCursorMap x new_y

selectDown:: Game->Game
selectDown  g = 
    g{_cursorx=new_x, _cursor=cursor_map}
    where x = _cursorx g 
          y = _cursory g
          new_x = min (x+1) 3
          cursor_map = newCursorMap new_x y       

selectUp :: Game->Game
selectUp  g = 
  g{_cursorx=new_x, _cursor=cursor_map}
  where x = _cursorx g 
        y = _cursory g
        new_x = max (x-1) 0
        cursor_map = newCursorMap new_x y




sendMoveRequest :: Int->Int->Int->Int->UIHelper.Player->(Bool,[[String]], [[UIHelper.Player]])
sendMoveRequest x_old y_old x_new y_new player = (True, [["?", "?", "ant", "?"],["?", "?", "?", "?"],["?", "?", "?", "?"],["?", "?", "?", "?"]], [[UIHelper.Unknown, UIHelper.Unknown, UIHelper.Red, UIHelper.Unknown],[UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown],[UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown],[UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown]])


moveLeft :: Game->Game
moveLeft  g = 
  case legal of 
      False -> g
      True -> g{_selected = False, _player = rotatePlayer (_player g), _playerMap = new_player_map, _grid = new_display_map, _cursory=new_y, _cursor=cursor_map}
      where
          x = _cursorx g 
          y = _cursory g
          new_y = max (y-1) 0
          cursor_map = newCursorMap x new_y
          retv = sendMoveRequest x y x (y-1) (_player g)
          legal = fst3 retv
          new_display_map = stringToTile (snd3 retv)
          new_player_map = thd3 retv

moveRight :: Game->Game
moveRight  g =     
    case legal of 
      False -> g
      True -> g{_selected = False, _player = rotatePlayer (_player g), _playerMap = new_player_map, _grid = new_display_map, _cursory=new_y, _cursor=cursor_map}
      where
          x = _cursorx g 
          y = _cursory g
          new_y = min (y+1) 3
          cursor_map = newCursorMap x new_y
          retv = sendMoveRequest x y x (y+1) (_player g)
          legal = fst3 retv
          new_display_map = stringToTile (snd3 retv)
          new_player_map = thd3 retv
    

moveDown:: Game->Game
moveDown  g = 
   case legal of 
    False -> g
    True -> g{_selected = False, _player = rotatePlayer (_player g), _playerMap = new_player_map, _grid = new_display_map, _cursorx=new_x, _cursor=cursor_map}
    where
        x = _cursorx g 
        y = _cursory g
        new_x = min (x+1) 3
        cursor_map = newCursorMap new_x y 
        retv = sendMoveRequest x y (x+1) y (_player g)
        legal = fst3 retv
        new_display_map = stringToTile (snd3 retv)
        new_player_map = thd3 retv
         

moveUp :: Game->Game
moveUp  g = 
  case legal of 
   False -> g
   True -> g{_selected = False, _player = rotatePlayer (_player g), _playerMap = new_player_map, _grid = new_display_map, _cursorx=new_x, _cursor=cursor_map}
  where
      x = _cursorx g 
      y = _cursory g
      new_x = max (x-1) 0
      cursor_map = newCursorMap new_x y 
      retv = sendMoveRequest x y (x-1) y (_player g)
      legal = fst3 retv
      new_display_map = stringToTile (snd3 retv)
      new_player_map = thd3 retv



move :: Direction -> Game -> Game
move dir g = case dir of
  UIHelper.Up -> 
    case (_selected g) of
      True -> moveUp g
      False -> selectUp g
  UIHelper.Down -> 
    case (_selected g) of
      True -> moveDown g
      False -> selectDown g
  UIHelper.Left -> 
    case (_selected g) of
      True -> moveLeft g
      False -> selectLeft g
  UIHelper.Right -> 
    case (_selected g) of
      True -> moveRight g
      False -> selectRight g

selectAndCancel :: Game->Game
selectAndCancel g = 
  case (_selected g) of
    True -> g{_selected = False} -- cancel the selected grid
    False -> 
      case legal of 
        True -> g{_selected = True} 
        False -> g
        where 
          legal = selectRequest (_cursorx g) (_cursory g) (_player g)

sendFlipRequest :: Int->Int->UIHelper.Player->(Bool,[[String]], [[UIHelper.Player]])
sendFlipRequest x y player = (True, [["?", "ant", "?", "?"],["?", "?", "?", "?"],["?", "?", "?", "?"],["?", "?", "?", "?"]], [[UIHelper.Unknown, UIHelper.Red, UIHelper.Unknown,UIHelper.Unknown],[UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown],[UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown],[UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown]])

flipChess :: Game->Game
flipChess g = do
  if (_selected g) == False
    then g
    else do
      let retv = sendFlipRequest (_cursorx g) (_cursory g) (_player g)
      let legal = fst3 retv
      let new_display_map = stringToTile (snd3 retv)
      let new_player_map = thd3 retv
      if legal == False
        then g
        else g{_selected = False, _player = rotatePlayer (_player g), _playerMap = new_player_map, _grid = new_display_map}
      

-- input handle
handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ move UIHelper.Up g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ move UIHelper.Down g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ move UIHelper.Right g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ move UIHelper.Left g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'f') [])) = continue $ flipChess g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent (V.EvKey V.KEnter []))        = continue $ selectAndCancel g
handleEvent g _                                     = continue g

-- Drawing
drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 4) (drawStats g) <+> drawGrid g <+> padLeft (Pad 4) drawCommandRule]

drawCommandRule :: Widget Name
drawCommandRule = hLimit 29
  $ vBox [drawInfo, padTop (Pad 1) $ drawRule]

drawRule :: Widget Name
drawRule = withBorderStyle BS.unicodeBold
  $ hLimit 29
  $ B.borderWithLabel (str "Game Rule")
  $ vBox $ map drawR
  $ [ "The value of the beast is"
    , "elephant > lion > tiger >"
    , "cheetah > dog > cat > rat"
    , "> ant."
    , " "
    , "However, both rat and ant"
    , "can eat elephant."
    ]
  where
    drawR rule = (padRight Max $ padLeft (Pad 1) $ padRight (Pad 1) $ str rule)

drawInfo :: Widget Name
drawInfo = withBorderStyle BS.unicodeBold
  $ hLimit 29
  $ B.borderWithLabel (str "Commands")
  $ vBox $ map (uncurry drawKey)
  $ [ ("Left", "←")
    , ("Right", "→")
    , ("Down", "↓")
    , ("Up", "↑")
    , ("Restart", "r")
    , ("Quit", "q or esc")
    , ("Select/Cancel", "Enter")
    , ("Flip", "f")
    ]
  where
    drawKey act key = (padRight Max $ padLeft (Pad 1) $ str act)
                      <+> (padLeft Max $ padRight (Pad 1) $ str key)

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore g , padTop (Pad 2) $ drawGameOver (_done g)]

drawScore :: Game -> Widget Name
drawScore g = withBorderStyle BS.unicodeBold
  $ withAttr (playerColor (_player g))
  $ B.borderWithLabel (str "Player")
  $ C.hCenter
  $ padAll 1
  $ str $ show (_player g)

drawGameOver :: Bool -> Widget Name
drawGameOver done =
  if done
    then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
    else emptyWidget

colorTile val = case val of
  "2" -> withAttr blueBg $ str val
  "4" -> withAttr brblBg $ str val
  "8" -> withAttr cyanBg $ str val
  "16" -> withAttr bcyanBg $ str val
  "32" -> withAttr magBg $ str val
  "64" -> withAttr bmagBg $ str val
  "128" -> withAttr yellowBg $ str val
  "256" -> withAttr byellowBg $ str val
  "512" -> withAttr greenBg $ str val
  "1024" -> withAttr bgreenBg $ str val
  "2048" -> withAttr whiteBg $ str val
  _ -> str val

-- drawBorder :: (Tile, Bool) -> _
-- drawBorder tp =  case (snd tp) of
--       True ->  B.borderWithLabel (withAttr magBg $ str "-----")  $ C.hCenter $ padAll 1 $ colorTile $ printTile (fst tp)
--       False->  B.border $ C.hCenter $ padAll 1 $ colorTile $ printTile (fst tp)

getBorderLabel :: Bool -> UIHelper.Player ->Widget Name
getBorderLabel  s p = case s of 
    True -> if (p == UIHelper.Red) then (withAttr redBg $ str "Select") else (withAttr blueBg $ str "Select")
    False -> if (p == UIHelper.Red) then (withAttr redBg $ str "----------") else (withAttr blueBg $ str "----------")
    
drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (withAttr magBg $ str "BeastChess")
  $ vBox rows
  where
    rows = [hBox $ tilesInRow r (_selected g) (_player g)| t<- (zip3 (_grid g) (_cursor g) (_playerMap g)) , r <- [zip3 (_fst3_ t) (_snd3_ t) (_thd3_ t)] ]
    tilesInRow row selected player= [hLimit 9 $ withBorderStyle BS.unicodeBold $ withAttr (playerColor (_thd3 tp)) $ (if (_snd3 tp ==False) then  B.border else (B.borderWithLabel $ getBorderLabel selected player) )  $ C.hCenter $ padAll 1 $ colorTile $ printTile (_fst3 tp) | tp <- row]

