import Data.List

qSort :: [Int] -> ([Int]->Int)-> (Int, [Int])
qSort [] _ = (0,[])
qSort [x] _ = (0, [x])
qSort xs pivotFunc = (p_count+l_s_count+r_s_count, ls++[xs!!pivot]++rs)
           where pivot = pivotFunc xs
                 (p_count, lPartitioned, rPartitioned) = qPartition xs pivot
                 (l_s_count, ls) = qSort lPartitioned pivotFunc
                 (r_s_count, rs) = qSort rPartitioned pivotFunc

alwaysFirst :: [Int] -> Int
alwaysFirst _ = 0

alwaysLast :: [Int] -> Int
alwaysLast xs = (length xs) - 1

medianOfThree :: [Int] -> Int
medianOfThree [x] = 0
medianOfThree [x,y] = 0
medianOfThree xs = result
                   where (hp,mp,tp)=headMedianTail xs
                         h = xs!!hp
                         m = xs!!mp
                         t = xs!!tp
                         (c,ss) = qSort [h,m,t] alwaysFirst
                         picked = ss!!1
                         (Just result) = elemIndex picked xs

headMedianTail :: [Int] -> (Int,Int,Int)
headMedianTail xs = (0, ((length xs)-1) `div` 2, (length xs) -1)


qPartition :: [Int] -> Int -> (Int, [Int], [Int])
qPartition xs 0 = ((length xs) -1, lastToHead ls, rs)
                     where (ls,rs) = foldl (\(ms,ns) y -> if (head xs) > y 
                                                       then (ms++[y], headTailReverse ns)
                                                       else (ms, ns ++ [y]))
                                     ([],[])
                                     (tail xs)
qPartition xs pivot = qPartition xs' 0
                      where (ls,rs) = splitAt pivot xs
                            xs' = (head rs) : (tail ls) ++ [(head ls)] ++ (tail rs)

-- simulating the behavior of the partition mentioned in class, it is based on value swap, and is not haskell code. doing this only to pass the test....

headTailReverse :: [a] -> [a]
headTailReverse [] = []
headTailReverse ns = (tail ns) ++ [(head ns)]

lastToHead :: [a] -> [a]
lastToHead [] = []
lastToHead xs = last xs : init xs
