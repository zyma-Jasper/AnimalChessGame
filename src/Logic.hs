
module Logic () where
import UIHelper
  (Game(..), Direction(..), Grid, Tile, Player(..), printTile, initGame)

SelectRequest :: Int->Int->UIHelper.Player->Bool
SelectRequest x y player = True