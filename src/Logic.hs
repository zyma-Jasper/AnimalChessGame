
module Logic (initRandomMap, sendFlipRequest,selectRequest, sendMoveRequest) where
import Data.Matrix
import Data.Ord
import UIHelper
  (Game(..),Direction(..), Grid, Tile, printTile, initGame, updateList, red, blue, unknown)




---- Helper function in the Logic.hs






---Interface implementation
initRandomMap :: Game
initRandomMap  = 
    Game { 
          _grid = [
                        [-2, -1, -1, -7],
                        [-8, -4, -3, -5],
                        [-7, -6, -5, -3],
                        [-6, -8, -4, -2]
                    ]
        , _score = 0
        , _done = False
        ,_cursor = [[True, False, False, False],
                    [False, False, False, False],
                    [False, False, False, False],
                    [False, False, False, False]]
        ,_playerMap = [
                            [UIHelper.red, UIHelper.blue, UIHelper.red, UIHelper.blue],
                            [UIHelper.red, UIHelper.blue, UIHelper.red, UIHelper.blue],
                            [UIHelper.red, UIHelper.blue, UIHelper.red, UIHelper.blue],
                            [UIHelper.red, UIHelper.blue, UIHelper.red, UIHelper.blue]
                        ]
        ,_cursorx = 0
        ,_cursory = 0
        ,_selected = False
        ,_player = UIHelper.red
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
                    (True, g{_grid = new_grid,  _player = new_player, _selected = False})
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
                     (True, g{_grid = grid2, _playerMap=playermap2, _selected=False, _player = (-p)})
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
