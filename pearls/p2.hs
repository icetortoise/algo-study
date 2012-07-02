import Data.List
import System.Random

-- msc :: Ord a => [a] -> Int -> Maybe (Int, Int, a)
-- msc [] prev = Nothing
-- msc [x] prev = Just (prev,0,x)
-- msc [x,y] prev= if x < y then Just (prev, 1, x) else Just (prev+1, 0, y)
-- msc xs prev= mscImpl xs vs (msc us 0) prev
--     where (us,vs) = splitAt ((length xs) `div` 2) xs

-- mscImpl :: Ord a => [a] -> [a] -> Maybe (Int, Int, a) -> Int -> Maybe (Int, Int, a)
-- mscImpl xs vs (Just (0,o,x)) prev = Just (prev, o + length (filter (>x) vs), x)
-- mscImpl xs vs (Just (i,_,x)) prev = msc (snd (splitAt i xs)) (i+prev)

-- randomRs (1,100) (mkStdGen 234245)

tails' [] = []
tails' (x:xs) = (x:xs) : tails xs

scount' x xs = length (filter (x<) xs)
table' :: [Int] -> [(Int,Int)]
table' xs = [(z, scount' z zs) | z:zs <- tails' xs]
msc' :: [Int] -> Int
msc' = maximum . map snd . table'

-- 

table [x] = [(x,0)]
table xs = join (m-n) (table ys) (table zs)
    where m = length xs
          n = m `div` 2
          (ys,zs) = splitAt n xs

-- note that this is a sorted merge: if txs/tys are sorted or they are singletons, then the result is sorted.
-- and evalutions to join are always beginning with singletons, because 'table' does a divide-and-conque!!
join 0 txs [] = txs
join n [] tys = tys
join n txs@((x,c):txs') tys@((y,d):tys')
     | x < y = (x,c+n):join n txs' tys
     | x >= y = (y,d):join (n-1) txs tys'

msc xs = maximum $ map snd (table xs)
