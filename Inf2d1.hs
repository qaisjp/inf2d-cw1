-- Inf2d Assignment 1 2017-2018
-- Matriculation number:
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
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

is_in_grid :: Node -> Bool
is_in_grid (x, y) = and [(x >= 1), (y >= 1), (x <= gridWidth_search), (y <= gridLength_search)]

-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
checkArrival:: Node -> Node -> Bool
checkArrival destination curNode = destination == curNode

-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.
breadthFirstSearch::Node-> (Branch-> [Branch])-> [Branch]->[Node]-> Maybe Branch
breadthFirstSearch destination next branches exploredList = undefined


-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.

depthFirstSearch::Node-> (Branch-> [Branch])-> [Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next  branches exploredList = undefined

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree..

depthLimitedSearch::Node-> (Branch-> [Branch])-> [Branch]-> Int-> [Node]-> Maybe Branch
depthLimitedSearch destination next  branches d exploredList = undefined

-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
iterDeepSearch:: Node-> (Branch-> [Branch])-> Node-> Int-> Maybe Branch
iterDeepSearch destination next initialNode d = undefined

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
bestFirstSearch destination next heuristic branches exploredList = undefined

-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> (Branch-> Int)-> [Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost branches exploredList = undefined


-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch-> Int
cost branch = undefined





-- In this section, the function determines the score of a terminal state, assigning it a value of +1, -1 or 0:
eval :: Game-> Int
eval game = undefined

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Role-> Game-> Int
minimax player game = undefined


-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

alphabeta:: Role-> Game-> Int
alphabeta  player game = undefined


{- Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms below.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
-}