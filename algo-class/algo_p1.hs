-- count_inversion :: [Int] -> Int

count_inversion' :: [Int] -> Int
count_inversion' [x] = 0
count_inversion' (x:xs) = (length $ filter (\y -> x > y) xs) + (count_inversion' xs)

run_n_by_n = do
  f <- readFile "/ms/user/a/andywu/lisp_study/pearls/algo_p1.input"
  putStrLn $ show (count_inversion' (map read (lines f) :: [Int]))

count_inversion :: [Int] -> (Int,[Int])
count_inversion [x] = (0, [x])
count_inversion xs = (left_count + right_count + splits, merge_sorted)
    where (left_count, left_sorted) = count_inversion left_xs
          (right_count, right_sorted) = count_inversion right_xs
          (splits, merge_sorted) = merge_and_count left_sorted right_sorted
          (left_xs, right_xs) = splitAt ((length xs) `div` 2) xs

merge_and_count :: [Int] -> [Int] -> (Int, [Int])
merge_and_count xs [] = (0, xs)
merge_and_count [] ys = (0, ys)
merge_and_count (x:xs) (y:ys) 
    | x < y  = let (c,ss) = merge_and_count xs (y:ys)
               in  (c,(x:ss))
    | x >= y = let (c,ss) = merge_and_count (x:xs) ys
               in (c+(length (x:xs)),(y:ss))

run_n_by_logn = do
  f <- readFile "/ms/user/a/andywu/lisp_study/pearls/algo_p1.input"
  putStrLn $ show (fst (count_inversion (map read (lines f) :: [Int])))
