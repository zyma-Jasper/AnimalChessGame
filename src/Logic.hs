module Logic (initRandomMap, sendFlipRequest,selectRequest, sendMoveRequest, isGameOver) where
import Data.List
import Data.Matrix
import Data.Ord
import System.Random
import System.IO.Unsafe
import UIHelper
  (Game(..),Direction(..), Grid, Tile, printTile, initGame, updateList, red, blue, unknown)




---- Helper function in the Logic.hs

-- >>> randomR (0,10) (mkStdGen 1)
-- (10,80028 40692)
--

-- >>> genRandomMap' [-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16] 16 (mkStdGen 1)
-- [-8,-2,-16,-12,-14,-10,-6,-3,-9,-15,-7,-1,-11,-5,-4,-13]
--

-- >>> genRandomPlayerMap (genRandomMap [-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16] 16 (mkStdGen 1))
-- [1,1,-1,-1,-1,-1,1,1,-1,-1,1,1,-1,1,1,-1]
--
genRandomPlayerMap [] = []
genRandomPlayerMap l = 
    if head l < -8 then
        (UIHelper.blue : (genRandomPlayerMap (tail l)))
    else (UIHelper.red : (genRandomPlayerMap (tail l)))


genRandomMap' _ 0= []
genRandomMap' l n = do
    let t = unsafePerformIO $ getStdRandom $ randomR (0, ((length l) - 1))
    let num = l !! t
    let l1 = delete num l
    (num:(genRandomMap' l1 (n-1)))

genRandomMap l = map f l where
    f = \x -> 
        if x < -8 then
            x + 8
        else x


isGameOver :: Game ->(Bool, Game)
isGameOver g = do
    let canFlip = [ ((_grid g)!!r!!c) < 0| r<-[0..3], c<-[0..3]]
    let idxs = [(r,c) |r<-[0..3], c<-elemIndices (_player g) ((_playerMap g)!!r)]
    let canUp = [fst (sendMoveRequest v ((fst v)-1, (snd v)) g ) | v<-idxs]
    let canDown = [fst (sendMoveRequest v ((fst v)+1, (snd v)) g ) | v<-idxs]
    let canLeft = [fst (sendMoveRequest v ((fst v), (snd v)-1) g ) | v<-idxs]
    let canRight = [fst (sendMoveRequest v ((fst v), (snd v)+1) g ) | v<-idxs]
    if (elem True canFlip) || (elem True canUp) ||(elem True canDown) ||(elem True canLeft) || (elem True canRight)
        then 
        if (_step g) > 200
            then (True,g{_done=True, _winner = UIHelper.unknown})
        else do
            let winner = gameOver (_playerMap g)
            if winner /=0 
                then (True,g{_done=True, _winner = winner})
            else (False, g)
    else 
        (True, g{_done=True, _winner = (-(_player g)) })



gameOver l = 
    if elem 1 l1 && not (elem (-1) l1) then
        UIHelper.red
    else if elem (-1) l1 && not (elem 1 l1) then
        UIHelper.blue
    else UIHelper.unknown
    where
        l1 = concat l

---Interface implementation
initRandomMap :: Bool -> Game
initRandomMap  isDemo = 
    let omap =  if isDemo == True
                    then [-8, -11, -12, -1, -14, -7, -10, -5, -13, -6, -16, -4, -2, -9, -3, -15]
                    --- [-1,-2,-3,-4,-5,-6,-7,-8, -13,-14,-15,-16,-9,-10,-11,-12] This is for the corner case map
                else (genRandomMap' [-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16] 16 )
    in  Game { 
          _grid = toLists (fromList 4 4 (genRandomMap omap))
        , _score = 0
        , _done = False
        ,_cursor = [[True, False, False, False],
                    [False, False, False, False],
                    [False, False, False, False],
                    [False, False, False, False]]
        ,_playerMap = toLists (fromList 4 4 (genRandomPlayerMap omap))
        ,_cursorx = 0
        ,_cursory = 0
        ,_selected = False
        ,_player = UIHelper.red
        ,_step = 0
        ,_winner = UIHelper.unknown
        }
    
    





sendFlipRequest :: Int->Int->Game->(Bool,Game)
sendFlipRequest x y g = do
    let legal  = (x>=0) && (x<=3) && (y>=0) && (y<=3)
    if legal==False
        then (False,g)
        else do
            let t = (_grid g)!!x!!y
            if t<0
                then do
                    let new_grid = updateList (-t) (x,y) (_grid g)
                    let new_player = (- (_player g))
                    let s = (_step g)
                    (True, g{_grid = new_grid,  _player = new_player, _selected = False, _step=s+1})
                else 
                    (False,g)

selectRequest :: Int->Int->Game->Bool
selectRequest x y g = do
    let legal  = (x>=0) && (x<=3) && (y>=0) && (y<=3)
    if legal==False --- index illegal
        then False
    else do -- whether current play can choose the grid(map value is negative(to flip) or positive but matches color)
        let v = (_grid g)!!x!!y
        let p = (_playerMap g)!!x!!y
        (v<0 || p==(_player g) )


isBigger :: Int->Int->Bool
isBigger x y = case x of
    8 ->  (y>=3) && (y<=7)
    1 ->  (y==8)
    2->  (y==8) || (y==1)
    _ -> (x>y)


sendMoveRequest :: (Int,Int)->(Int,Int)->Game->(Bool,Game)
sendMoveRequest (x0,y0) (x1,y1) g = do 
    let legal = (x0>=0)&&(x0<=3)&&(x1>=0)&&(x1<=3)&&(y0>=0)&&(y0<=3)&&(y1>=0)&&(y1<=3)
    if legal == False
        then (False, g) --- index illegal
        else do
            let dst_v = (_grid g)!!x1!!y1
            let src_v = (_grid g)!!x0!!y0
            let dst_p = (_playerMap g)!!x1!!y1
            let src_p = (_playerMap g)!!x0!!y0
            let p = (_player g)
            if ( dst_v ==0  || (dst_p/=src_p && (isBigger src_v dst_v) ) )--dst grid is empty(move) || dst is another players and smaller than mine
                then do --move operation(grid: dst=src, src=0, playermap: dst=src src=empty)
                     let grid1 = updateList src_v (x1,y1) (_grid g) ---dst = src
                     let grid2 = updateList 0 (x0,y0) grid1 --src=0
                     let playermap1 = updateList src_p (x1,y1) (_playerMap g)
                     let playermap2 = updateList unknown (x0,y0) playermap1
                     let s = (_step g)
                     (True, g{_grid = grid2, _playerMap=playermap2, _selected=False, _player = (-p), _step=s+1})
            else (False, g)






    
    










-- type Vector = [Int]

-- create :: Num x => Int -> Int -> Matrix x
-- create a b = zero a b

-- update :: Num a => a -> (Int,Int) -> Matrix a -> Matrix a
-- update x (r,c) m = setElem x (r,c) m

-- flipE :: (Ord a, Num a, Eq a) => Int -> Int -> Matrix a -> Matrix a
-- flipE r c m = do
--     let t = getElem r c m
--     if t < 0 --- If unflipped, then the matrix =-1
--         then update (-t) (r,c) m
--     else m

-- move :: (Ord a, Num a, Eq a) => (Int,Int) -> (Int,Int) -> Matrix a -> Matrix a
-- move (r1,c1) (r2,c2) m = do 
--     let t1 = getElem r1 c1 m 
--     let t2 = getElem r2 c2 m
--     if t2 == 0
--         then do
--             let temchess = update t1 (r2,c2) m
--             update 0 (r1,c1) temchess
--     else m

-- eat :: (Ord a, Num a, Eq a) => (Int,Int) -> (Int,Int) -> Matrix a -> Matrix a
-- eat (r1,c1) (r2,c2) m = do
--     let t1 = getElem r1 c1 m
--     let t2 = getElem r2 c2 m
--     if ((t1 < 0) || (t2 < 0) || ((t1 <= 8) && (t2 <= 8)) || ((t1 > 8) && (t2 > 8)))
--         then m
--     else if (t1 > 8)
--         then do
--             let tem = t1 - 8
--             if tem > t2
--                 then do
--                     let temchess = update t1 (r2,c2) m
--                     update 0 (r1,c1) temchess
--             else if ((t2 == 8) && ((tem == 1) || (tem == 2)))
--                 then do
--                     let temchess = update t1 (r2,c2) m
--                     update 0 (r1,c1) temchess
--                 else m
--         else do
--             let tem = t2 - 8
--             if tem < t1
--                 then do
--                     let temchess = update t1 (r2,c2) m
--                     update 0 (r1,c1) temchess
--             else if ((tem == 8) && ((t1 == 1) || (t1 == 2)))
--                 then do
--                     let temchess = update t1 (r2,c2) m
--                     update 0 (r1,c1) temchess
--                 else m


-- someFunc :: IO ()
-- someFunc = do
--             putStrLn "hello"
--             putStrLn "Thanks"
--             let chess1 = (create 4 4)
--             putStrLn $ show (chess1)
--             --let i = 0
--             --let q = 6
--             let chess2 = update (-5) (1,1) chess1
--             let chess1 = chess2
--             putStrLn $ show (chess1)

--             let chess2 = (flipE 1 1 chess1)
--             let chess1 = chess2
--             putStrLn $ show (chess1)

--             let chess2 = flipE 1 1 chess1
--             let chess1 = chess2
--             putStrLn $ show (chess1)

--             let chess2 = (move (1,1) (1,2) chess1)
--             let chess1 = chess2
--             putStrLn $ show (chess1)

--             let chess2 = update 14 (2,2) chess1
--             let chess1 = chess2
--             putStrLn $ show (chess1)

--             let chess2 = (move (1,2) (2,2) chess1)
--             let chess1 = chess2
--             putStrLn $ show (chess1)

--             let chess2 = (eat (1,2) (2,2) chess1)
--             let chess1 = chess2
--             putStrLn $ show (chess1)

--             let chess2 = (eat (2,2) (1,2) chess1)
--             let chess1 = chess2
--             putStrLn $ show (chess1)
--             --let chess1 = updateMatrix chess2 (-11) (2,1)
--             --let i = 0
--             --putStrLn $ show (chess1)
