import Control.Monad
import Data.Maybe
--8.7
-- data Representation = Quater Int | Dime Int | Nickel Int | Penny Int deriving Show
ways :: [Int] -> Int -> Maybe [[Int]]
ways _ n | n<0 =Nothing
ways [x] n 
     | n `mod` x == 0 = Just [[n `div` x]]
     | otherwise = Nothing
ways (x:xs) n 
     | n >= x = let count = n `div` x
                    solutions = map (\m->cons m $ ways xs (n-(x*m))) [0..count]
                    in Just (join $ catMaybes solutions)
     | otherwise = cons 0 $ ways xs n

cons :: Int -> Maybe [[Int]] -> Maybe [[Int]]
cons x solutions = liftM (\ll->map (x:) ll) $ solutions

--8.8
nQueen :: Int -> [Int] -> [[Int]]
nQueen n layout 
       |n == length layout = [layout]
       |n > length layout =  concat $ map (nQueen n) possibleLayouts
                             where validPos = filter (isValid layout) [1..n]
                                   possibleLayouts = map (build layout) validPos

isValid :: [Int] -> Int -> Bool
isValid [] _ = True
isValid xs@(x:xs') n = and [x/=n,
                           abs (n-x)/=length xs,
                           isValid xs' n]

build :: [Int] -> Int -> [Int]
build xs x = xs ++ [x]
                           
