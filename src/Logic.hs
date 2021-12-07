
module Logic (someFunc) where
import Data.Matrix
import Data.Ord
import UIHelper
  (Game(..), Direction(..), Grid, Tile, Player(..), printTile, initGame)

SelectRequest :: Int->Int->UIHelper.Player->Bool
SelectRequest x y player = True

type Vector = [Int]

create :: Num x => Int -> Int -> Matrix x
create a b = zero a b

update :: Num a => a -> (Int,Int) -> Matrix a -> Matrix a
update x (r,c) m = setElem x (r,c) m

flipE :: (Ord a, Num a, Eq a) => Int -> Int -> Matrix a -> Matrix a
flipE r c m = do
    let t = getElem r c m
    if t < 0
        then update (-t) (r,c) m
    else m

move :: (Ord a, Num a, Eq a) => (Int,Int) -> (Int,Int) -> Matrix a -> Matrix a
move (r1,c1) (r2,c2) m = do
    let t1 = getElem r1 c1 m
    let t2 = getElem r2 c2 m
    if t2 == 0
        then do
            let temchess = update t1 (r2,c2) m
            update 0 (r1,c1) temchess
    else m

eat :: (Ord a, Num a, Eq a) => (Int,Int) -> (Int,Int) -> Matrix a -> Matrix a
eat (r1,c1) (r2,c2) m = do
    let t1 = getElem r1 c1 m
    let t2 = getElem r2 c2 m
    if ((t1 < 0) || (t2 < 0) || ((t1 <= 8) && (t2 <= 8)) || ((t1 > 8) && (t2 > 8)))
        then m
    else if (t1 > 8)
        then do
            let tem = t1 - 8
            if tem > t2
                then do
                    let temchess = update t1 (r2,c2) m
                    update 0 (r1,c1) temchess
            else if ((t2 == 8) && ((tem == 1) || (tem == 2)))
                then do
                    let temchess = update t1 (r2,c2) m
                    update 0 (r1,c1) temchess
                else m
        else do
            let tem = t2 - 8
            if tem < t1
                then do
                    let temchess = update t1 (r2,c2) m
                    update 0 (r1,c1) temchess
            else if ((tem == 8) && ((t1 == 1) || (t1 == 2)))
                then do
                    let temchess = update t1 (r2,c2) m
                    update 0 (r1,c1) temchess
                else m


someFunc :: IO ()
someFunc = do
            putStrLn "hello"
            putStrLn "Thanks"
            let chess1 = (create 4 4)
            putStrLn $ show (chess1)
            --let i = 0
            --let q = 6
            let chess2 = update (-5) (1,1) chess1
            let chess1 = chess2
            putStrLn $ show (chess1)

            let chess2 = (flipE 1 1 chess1)
            let chess1 = chess2
            putStrLn $ show (chess1)

            let chess2 = flipE 1 1 chess1
            let chess1 = chess2
            putStrLn $ show (chess1)

            let chess2 = (move (1,1) (1,2) chess1)
            let chess1 = chess2
            putStrLn $ show (chess1)

            let chess2 = update 14 (2,2) chess1
            let chess1 = chess2
            putStrLn $ show (chess1)

            let chess2 = (move (1,2) (2,2) chess1)
            let chess1 = chess2
            putStrLn $ show (chess1)

            let chess2 = (eat (1,2) (2,2) chess1)
            let chess1 = chess2
            putStrLn $ show (chess1)

            let chess2 = (eat (2,2) (1,2) chess1)
            let chess1 = chess2
            putStrLn $ show (chess1)
            --let chess1 = updateMatrix chess2 (-11) (2,1)
            --let i = 0
            --putStrLn $ show (chess1)
