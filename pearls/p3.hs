f:: Int -> Int -> Int
f x y = x + y + 1
invert f z = [(x,y)|x<-[0..z],y<-[0..z],f x y==z]

invert' f z = [(x,y)|x<-[0..z-(f 0 0)],y<-[0..z-x-(f 0 0)],f x y==z]

bsearch g (a,b) z
        | a + 1 == b = a
        | g m <= z =bsearch g (m,b) z
        | otherwise = bsearch g (a,m) z
        where m = (a+b) `div` 2

invert'' f z = find (0,m) f z n
               where m = bsearch (\y->f 0 y) (-1, z+1) z
                     n = bsearch (\x->f x 0) (-1, z+1) z

find (u,v) f z n
     | u > n || v < 0 = []
     | z' < z = find (u+1, v) f z n
     | z' == z = (u,v):find (u+1, v+1) f z n
     | z' > z = find (u,v-1) f z n
     where z' = f u v

