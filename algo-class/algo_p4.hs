import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe

type Graph = V.Vector (Int, [Int])

buildGraph :: [[Int]] -> Graph
buildGraph es = foldl mergeEdge (V.fromList [(1,[])]) es

revertEdge :: [[Int]] -> [[Int]]
revertEdge [] = []
revertEdge ([a,b]:xs) = [b,a]:(revertEdge xs)

mergeEdge :: Graph -> [Int] -> Graph
mergeEdge g [from, to] 
          | V.null g = V.fromList [(from, [to])]
          | lFrom == from = g V.// [((V.length g) - 1, (from, lTos ++ [to]))]
          | otherwise = V.snoc g (from, [to])
                     where (lFrom,lTos) = V.last g

dfsOrder :: ([Int] ,V.Vector Bool) -> Graph -> Int -> ([Int], V.Vector Bool)
dfsOrder (order,marks) g n 
         | isNothing un = (n:order, marks')
         | otherwise = let result = dfsOrder (order, marks') g (fromJust un)
                       in dfsOrder result g n
         where (_, neighbors) = g V.! (n - 1)
               marks' = mark marks n
               un = firstUnvisited marks' neighbors

dfs :: (V.Vector Bool, [Int]) -> Graph -> Int -> (V.Vector Bool, [Int])
dfs (marks, nodes) g n 
            | isNothing un = (marks', nodes')
            | otherwise = dfs (marks', nodes') g $ fromJust un 
            where (_, neighbors) = g V.! (n - 1)
                  marks' = mark marks n
                  nodes' = (n:nodes)
                  un = firstUnvisited marks' neighbors

scc :: ([[Int]], V.Vector Bool) -> Graph -> [Int] -> ([[Int]], V.Vector Bool)
scc (sccs, marks) g order 
         | isNothing un = (sccs, marks)
         | otherwise = let (marks', nodes) = dfs (marks, []) g $fromJust un
                       in scc ((nodes:sccs), marks') g order
          where un = firstUnvisited marks order

mark :: V.Vector Bool -> Int -> V.Vector Bool
mark ms n = ms V.// [(n - 1, True)]

visited :: V.Vector Bool -> Int -> Bool
visited marks n = marks V.! (n - 1)

firstUnvisited :: V.Vector Bool -> [Int] -> Maybe Int
firstUnvisited marks ns 
               | us == [] = Nothing
               | otherwise = Just (head us)
               where us = filter (not . (visited marks)) ns

allVisited :: V.Vector Bool -> [Int] -> Bool
allVisited marks ns = L.all (visited marks) ns
                                   
count :: [x] -> Int -> Int
count [] already = already
count (x:xs) already = count xs (already+1)

-- f <- B.readFile "./algo_p4.input"
-- let ss = map B.unpack $ B.lines f
-- let ints = map (map (read :: String->Int) . words) ss
