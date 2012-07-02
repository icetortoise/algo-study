import qualified Data.HashSet as S

targetSums = [231552,234756,596873,648219,726312,981237,988331,1277361,1283379] :: [Int]

-- let re = [x | x<-inputs, S.member (231552-x) s]

targetSum :: [Int] -> [Int] -> S.HashSet Int -> [Bool]
targetSum sums [x] xset = map (\s -> S.member (s-x) xset) sums
targetSum sums (x:xs) xset = listOr (targetSum sums [x] xset)
                                    (targetSum sums xs xset)

listOr :: [Bool] -> [Bool] -> [Bool]
listOr [x] [y] = [or [x,y]]
listOr (x:xs) (y:ys) = (listOr [x] [y])++(listOr xs ys)

targetSum' :: [Int] -> Int -> Bool
targetSum' xs sum = not $ null [x | x<-xs, S.member (sum-x) s]
                       where s = S.fromList xs

-- map (targetSum' inputs ) targetSums