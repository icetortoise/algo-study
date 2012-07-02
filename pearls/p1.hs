import Data.Array
import Data.List

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

minfree :: [Int] -> Int
minfree xs = head([0..length xs] \\ xs)

-- (\\) :: Eq a => [a] -> [a] ->[a]
-- us \\ vs = filter (\x-> not (x `elem` vs))
--            us

minfree' xs = minfrom 0 (length xs, xs)
minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (n, xs) | n == 0 = a
                  | m == b - a = minfrom b (n - m, vs)
                  | otherwise = minfrom a (m, us)
                                where (us,vs) = partition (<b) xs
                                      b = a + 1 + (n `div` 2)
                                      m = length us
