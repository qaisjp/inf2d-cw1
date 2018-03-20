-- Inf2d Assignment 1 2017-2018
-- Matriculation number:
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy, minimumBy, maximumBy)
import Test.QuickCheck --TODO delete

import Debug.Trace
import ConnectFour

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6

{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the 3pm Tuesday 13th March 2018.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]


-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.
next:: Branch -> [Branch]
next [] = []
next branch = map (\node -> node : branch) [
    node
    | node <- possibilities
    , not $ elem node branch
    , is_in_grid node
    ]
    where
        (x, y) = head branch
        possibilities = [
                (x, y+1),
                (x, y-1),
                (x+1, y),
                (x-1, y)
            ]

-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
checkArrival:: Node -> Node -> Bool
checkArrival destination curNode = destination == curNode

-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.
breadthFirstSearch::Node-> (Branch-> [Branch])-> [Branch]->[Node]-> Maybe Branch
breadthFirstSearch destination next [] exploredList = Nothing
breadthFirstSearch destination next branches exploredList
    -- | trace "breadthFirstSearch" False = undefined
    | not $ null current = Just (head current) -- if heads of any existing branches are at destination, return one of the branches
    | all (`elem` exploredList) heads = Nothing -- explored them all, return nothing
    | otherwise = breadthFirstSearch destination next nextBranches (heads ++ exploredList) -- go another level down if not explored all
    where
        nextBranches = concat [next branch | branch <- branches]

        heads :: [Node]
        heads = map head nextBranches

        current :: [Branch]
        current = filter (\b -> checkArrival destination (head b)) branches


-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.

depthFirstSearch::Node-> (Branch-> [Branch])-> [Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next  branches exploredList
    -- | trace "depthFirstSearch" False = undefined
    | not $ all (`elem` exploredList) heads = depthFirstSearch destination next nextBranches (heads ++ exploredList) -- go another level down if not explored all
    | not $ null current = Just (head current) -- if heads of any existing branches are at destination, return one of the branches
    | otherwise = Nothing
    where
        nextBranches = concat [next branch | branch <- branches]

        heads :: [Node]
        heads = map head nextBranches

        current :: [Branch]
        current = filter (\b -> checkArrival destination (head b)) branches

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree..

depthLimitedSearch::Node-> (Branch-> [Branch])-> [Branch]-> Int-> [Node]-> Maybe Branch
depthLimitedSearch destination next  branches d exploredList
    -- | trace "depthLimitedSearch" False = undefined
    | (&&) (d > 0) $ not $ all (`elem` exploredList) heads = depthLimitedSearch destination next nextBranches (d-1) (heads ++ exploredList) -- go another level down if not explored all
    | not $ null current = Just (head current) -- if heads of any existing branches are at destination, return one of the branches
    | otherwise = Nothing
    where
        nextBranches = concat [next branch | branch <- branches]

        heads :: [Node]
        heads = map head nextBranches

        current :: [Branch]
        current = filter (\b -> checkArrival destination (head b)) branches

-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
iterDeepSearch:: Node-> (Branch-> [Branch])-> Node-> Int-> Maybe Branch
iterDeepSearch dest next initial d
    -- | trace "iterDeepSearch" False = undefined
    | result == Nothing = iterDeepSearch dest next initial (d+1)
    | otherwise = result
    where
        result = depthLimitedSearch dest next [[initial]] d []

-- a misunderstanding of iterDeepSearch where we try for a defined depth and then go level by level afterwards
-- rather than going for d and starting the search over for d+1
-- iterDeepSearch destination next initialNode d = search [[initialNode]] d []
--     where
--         search branches d exploredList
--             | (&&) (d > 0) $ not $ all (`elem` exploredList) heads = search nextBranches (d-1) (heads ++ exploredList) -- go another level down if not explored all
--             | not $ null current = Just (head current) -- if heads of any existing branches are at destination, return one of the branches
--             | not $ all (`elem` exploredList) heads = search nextBranches 1 (heads ++ exploredList)
--             | otherwise = Nothing
--             where
--                 nextBranches = concat [next branch | branch <- branches]

--                 heads :: [Node]
--                 heads = map head nextBranches

--                 current :: [Branch]
--                 current = filter (\b -> checkArrival destination (head b)) branches

--                 result :: [Branch]
--                 result = filter (\b -> checkArrival destination (head b)) nextBranches

-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the current position and the destination position.
manhattan::Node-> Node-> Int
manhattan (x, y) (x', y') = (abs $ x - x') + (abs $ y - y')

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.
bestFirstSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> [Branch]-> [Node]-> Maybe Branch
bestFirstSearch dest next heuristic branches exploredList
    -- | trace "iterDeepSearch" False = undefined
    | otherwise = iter (nearest branches) exploredList
    where
        nearest :: [Branch] -> [Branch]
        nearest = sortBy (\a b -> compare (heuristic $ head a) (heuristic $ head b))
        
        iter :: [Branch] -> [Node] -> Maybe Branch
        iter branches explored
            | checkArrival dest (head $ head branches) = Just (head branches)
            | not $ all (`elem` explored) nextHeads = iter nearestNext (nextHeads ++ explored)
            | otherwise = Nothing
            where
                nearestNext = nearest (concatMap next branches)
                nextHeads = map head nearestNext

-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> (Branch-> Int)-> [Branch]-> [Node]-> Maybe Branch
aStarSearch dest next heuristic cost branches exploredList
    -- | trace "iterDeepSearch" False = undefined
    | otherwise = iter (nearest branches) exploredList  
    where
        supercost :: Branch -> Int
        supercost a = (cost a) + (heuristic $ head a)

        nearest :: [Branch] -> [Branch]
        nearest = sortBy (\a b -> compare (supercost a) (supercost b))
        
        iter :: [Branch] -> [Node] -> Maybe Branch
        iter branches explored
            | checkArrival dest (head $ head branches) = Just (head branches)
            | not $ all (`elem` explored) nextHeads = iter nearestNext (nextHeads ++ explored)
            | otherwise = Nothing
            where
                nearestNext = nearest (concatMap next branches)
                nextHeads = map head nearestNext

-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch-> Int
cost = length





-- In this section, the function determines the score of a terminal state, assigning it a value of +1, -1 or 0:
eval :: Game-> Int
eval game
    | checkWin game maxPlayer = 1
    | checkWin game minPlayer = -1
    | otherwise = 0

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax :: Role -> Game -> Int
minimax p game
    | terminal game = eval game
    | (p == maxPlayer) || (p == minPlayer) = fn $ map (minimax $ switch p) (moves game p)
    where
        fn = if p == maxPlayer then maximum else minimum

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

alphabeta :: Role -> Game -> Int
alphabeta player game = fn [game] (2) (-2)
    where
        fn = if player == maxPlayer then maxV else minV
        maxV gs a b
            | terminal (head gs) && (length gs == 1) = eval (head gs)
            | otherwise = maxVLoop gs a b (-2)

        minV gs a b
            | terminal (head gs) && (length gs == 1) = eval (head gs)
            | otherwise = minVLoop gs a b (2)
        
        maxVLoop [] _ _ v = v
        maxVLoop (x:xs) a b v
            | v' >= b = v'
            | otherwise = maxVLoop xs (max v' a) b v'
            where v' = max v $ minV (moves x maxPlayer) a b

    
        minVLoop [] _ _ v = v
        minVLoop (x:xs) a b v
            | v' <= a = v'
            | otherwise = minVLoop xs a (min v' b) v'
            where v' = min v $ maxV (moves x minPlayer) a b
        

{- Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms below.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
-}

is_in_grid :: Node -> Bool
is_in_grid (x, y) = and [(x >= 1), (y >= 1), (x <= gridWidth_search), (y <= gridLength_search)]
