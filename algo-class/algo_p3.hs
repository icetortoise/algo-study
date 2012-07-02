import Data.List 
import Random (randomRIO)

data Node = Node String | SuperNode [String] deriving (Show, Eq)
type NodeEntry = (Node,[Node]) 
data Graph = GraphAdjNodes [NodeEntry] deriving Show

minEdgesCount :: Graph -> Int -> IO Int
minEdgesCount g i = do
          r:rs <- mapM (\x -> do
                            mcutGraph <- mcut g
                            let (GraphAdjNodes [x, y]) = mcutGraph
                            return (length $ snd x))
                     [1..i]
          return $ foldl min r rs

mcut :: Graph -> IO Graph
mcut g = do
     (n1, n2) <- pickEdge g
     let (GraphAdjNodes newEntries) = contract g n1 n2 in
          if (length newEntries) == 2
             then return (GraphAdjNodes newEntries)
             else mcut (GraphAdjNodes newEntries)

contract :: Graph -> Node -> Node -> Graph
contract g n1 n2 = replaceNode (replaceNode mergedGraph n1 mergedNode) n2 mergedNode
         where entry1 = nodeEntry g n1
               entry2 = nodeEntry g n2
               (GraphAdjNodes xs) = removeNode (removeNode g n1) n2
               mergedNodeEntry = mergeNode entry1 entry2
               (mergedNode, _) = mergedNodeEntry
               mergedGraph = (GraphAdjNodes (mergedNodeEntry:xs))

mergeNode :: NodeEntry -> NodeEntry -> NodeEntry
mergeNode (Node n1, e1s) (Node n2, e2s) = (SuperNode [n1, n2], e1sRemoveN2 ++ e2sRemoveN1)
          where e1sRemoveN2 = filter (/= Node n2) e1s  
                e2sRemoveN1 = filter (/= Node n1) e2s  
mergeNode (SuperNode n1s, e1s) (Node n2, e2s) = (SuperNode (n2:n1s), e1sRemoveN2 ++ e2sRemoveN1s)
          where e1sRemoveN2 = filter (/= Node n2) e1s
                e2sRemoveN1s = filter (/= SuperNode n1s) e2s
mergeNode (Node n1, e1s) (SuperNode n2s, e2s) = mergeNode (SuperNode n2s, e2s) (Node n1, e1s)
mergeNode (SuperNode n1s, e1s) (SuperNode n2s, e2s) 
          = (SuperNode (n1s ++ n2s), e1sRemoveN2s ++ e2sRemoveN1s)        
          where e1sRemoveN2s = filter (/= SuperNode n2s) e1s
                e2sRemoveN1s = filter (/= SuperNode n1s) e2s


nodeEntry :: Graph -> Node -> NodeEntry
nodeEntry (GraphAdjNodes xs) node = (head $ filter (\(n, an)-> node == n) xs)

removeNode :: Graph -> Node -> Graph
removeNode (GraphAdjNodes xs) node = GraphAdjNodes $ filter (\(n, an)-> node /= n) xs

replaceNode :: Graph -> Node -> Node -> Graph
replaceNode (GraphAdjNodes xs) old new = GraphAdjNodes $ map (\(n, an)-> (n, updateList an old new)) xs

updateList :: (Eq a) => [a] -> a -> a -> [a]
updateList [] _ _ = []
updateList (x:xs) old new 
    | old == x = (new:updateList xs old new)
    | old /= x  = (x:updateList xs old new)

pick :: [a] -> IO a
pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)

pickEdge :: Graph -> IO (Node, Node)
pickEdge g = pick (edges g)

edges :: Graph -> [(Node, Node)]
edges (GraphAdjNodes []) = []
edges (GraphAdjNodes (x:xs)) = (edgesOf x) ++ (edges (GraphAdjNodes xs))

edgesOf :: NodeEntry -> [(Node, Node)]
edgesOf (n, ns) = map (\x -> (n, x)) ns

-- f <- readFile "/Users/andywu/projects/pearls/algo_p3.input"
-- let g = map words $ lines f
-- let gg =  map (\(x:xs) -> (Node x, map Node xs)) g
-- let gra = GraphAdjNodes gg