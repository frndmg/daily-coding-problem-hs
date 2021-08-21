{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module GraphBridges743 where

import Control.Monad.State
import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Set as S

type Node = Int

newtype Edge = Edge (Node, Node) deriving (Show)

newtype Graph = Edges [Edge]
  deriving (Show)

neighbors :: Node -> Graph -> S.Set Node
neighbors n (Edges es) = (S.delete n . S.fromList . join) [[m, o] | Edge (m, o) <- es, o == n || m == n]

nodes :: Graph -> S.Set Node
nodes (Edges es) = (S.fromList . join . map (\(Edge (a, b)) -> [a, b])) es

instance Eq Edge where
  Edge (v1, w1) == Edge (v2, w2) = (v1 == v2 && w1 == w2) || (v1 == w2 && w1 == v2)
  e1 /= e2 = not (e1 == e2)

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y : xs)
  | x == y = xs
  | otherwise = removeFirst x xs

removeEdge :: Edge -> Graph -> Graph
removeEdge e (Edges es) = Edges (removeFirst e es)

type BridgesState = (Int, M.Map Node Int, S.Set Node)

bridges :: Graph -> Int
bridges (Edges []) = 0
bridges graph =
  let (bridges', _, _) =
        execState
          (forM_ nodes' (go graph graph 0)) -- go forall strong components of the graph
          (0, M.empty, nodes')
   in bridges'
  where
    nodes' :: S.Set Node
    nodes' = nodes graph

    go :: Graph -> Graph -> Int -> Node -> State BridgesState Int
    go originalGraph availableGraph depth n = do
      (_, heights, unexploredNodes) <- get

      if not (S.member n unexploredNodes)
        then do
          -- if n was already visited
          return $ M.findWithDefault depth n heights
        else do
          -- mark n as visited
          modify' $ second (S.delete n)
          modify' $ first (M.insert n depth)

          let nNeighbors = S.toList $ neighbors n availableGraph

          if null nNeighbors
            then return depth
            else do
              -- compute node's children heights
              minChildrenHeight <-
                forM
                  nNeighbors
                  ( \m ->
                      go
                        originalGraph
                        (removeEdge (Edge (n, m)) originalGraph)
                        (depth + 1)
                        m
                  )

              -- if no children height is less than the depth of current node, then
              -- it is a bridge

              let countBridges = length . filter (> depth) $ minChildrenHeight

              modify' $ \(bridges', a, b) -> (bridges' + countBridges, a, b)

              let nHeight = M.findWithDefault depth n heights
                  minHeight = min nHeight (minimum minChildrenHeight)

              -- save node height
              modify' $ first (M.insert n minHeight)

              -- when (n == 1) $ do
              --   (bridges, heights, unexploredNodes) <- get
              --   error $
              --     "depth: " ++ show depth
              --       ++ ", minChildrenHeight: "
              --       ++ show minChildrenHeight
              --       ++ ", nNeighbors: "
              --       ++ show nNeighbors
              --       ++ ", bridges: "
              --       ++ show bridges
              --       ++ ", unexploredNodes: "
              --       ++ show unexploredNodes
              --       ++ ", heights: "
              --       ++ show heights
              --       ++ ", availableGraph: "
              --       ++ show availableGraph

              return minHeight

-- >>> Edge (1, 2) /= Edge (2, 1)
-- False

-- >>> bridges (Edges [Edge (1, 2), Edge (2, 1), Edge (3, 2), Edge (4, 3), Edge (4, 1)])
-- 0

-- >>> bridges (Edges [])
-- 0
