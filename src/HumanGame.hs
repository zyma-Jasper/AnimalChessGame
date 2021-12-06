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

gameOverAttr, blueBg, brblBg, cyanBg, bcyanBg, yellowBg, byellowBg, greenBg, bgreenBg,  whiteBg  :: AttrName
gameOverAttr = "gameOver"
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

theMap :: AttrMap
theMap = attrMap V.defAttr
  [
  (gameOverAttr, fg V.red `V.withStyle` V.bold),
  (blueBg, U.fg V.blue),
  (brblBg, U.fg V.brightBlue),
  (cyanBg, U.fg V.cyan),
  (bcyanBg, U.fg V.brightCyan),
  (yellowBg, U.fg V.yellow),
  (byellowBg, U.fg V.brightYellow),
  (magBg, U.fg V.magenta),
  (bmagBg, U.fg V.brightMagenta),
  (greenBg, U.fg V.green),
  (bgreenBg, U.fg V.brightGreen),
  (whiteBg, U.bg V.white)
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


isGameOver :: Game -> Bool
isGameOver g = False
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


move :: Direction -> Game -> Game
move dir g = case dir of
  UIHelper.Up -> selectUp g
  UIHelper.Down -> selectDown g
  UIHelper.Left -> selectLeft g
  UIHelper.Right -> selectRight g


-- input handle
handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ move UIHelper.Up g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ move UIHelper.Down g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ move UIHelper.Right g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ move UIHelper.Left g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent (V.EvKey V.KEnter []))        = halt g
handleEvent g _                                     = continue g

-- Drawing
drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 4) (drawStats g) <+> drawGrid g <+> padLeft (Pad 4) drawInfo]

drawInfo :: Widget Name
drawInfo = withBorderStyle BS.unicodeBold
  $ hLimit 20
  $ B.borderWithLabel (str "Commands")
  $ vBox $ map (uncurry drawKey)
  $ [ ("Left", "←")
    , ("Right", "→")
    , ("Down", "↓")
    , ("Restart", "r")
    , ("Quit", "q or esc")
    , ("select or move or eat", "Enter")
    , ("flip", "f")
    ]
  where
    drawKey act key = (padRight Max $ padLeft (Pad 1) $ str act)
                      <+> (padLeft Max $ padRight (Pad 1) $ str key)

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (_score g) , padTop (Pad 2) $ drawGameOver (_done g)]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ withAttr gameOverAttr
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

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
    True -> if (p == UIHelper.Red) then (withAttr magBg $ str "Selected") else (withAttr blueBg $ str "Selected")
    False -> if (p == UIHelper.Red) then (withAttr magBg $ str "----------") else (withAttr blueBg $ str "----------")
    
drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (withAttr magBg $ str "2048")
  $ vBox rows
  where
    rows = [hBox $ tilesInRow r (_selected g) (_player g)| t<- (zip (_grid g) (_cursor g)) , r <- [zip (fst t) (snd t)] ]
    tilesInRow row selected player= [hLimit 9 $ withBorderStyle BS.unicodeBold $ (if (snd tp ==False) then  B.border else (B.borderWithLabel $ getBorderLabel selected player) )  $ C.hCenter $ padAll 1 $ colorTile $ printTile (fst tp) | tp <- row]

