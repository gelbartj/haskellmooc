{-# LANGUAGE TupleSections #-}
module Set9b where

import Mooc.Todo

import Data.List

--------------------------------------------------------------------------------
-- Ex 1: In this exercise set, we'll solve the N Queens problem step by step.
-- N Queens is a generalisation of the Eight Queens problem described in
-- Wikipedia: https://en.wikipedia.org/wiki/Eight_queens_puzzle
--
-- We'll be working with a two-dimensional coordinate system for indexing the
-- queens on a chessboard of arbitrary size. (1,1) represents the top left
-- corner. (1,2) is the next square on the top row, (1,3) is the one after that,
-- (2,1) is the first square on the second row, (2,2) is the second square in
-- the second row, and so on. In general, the coordinates are of the form
-- (row,column). The idea is that the following arrangement of queens on a 8x8
-- board will be encoded as the list [(1,8),(2,6),(3,4),(5,7)] of coordinates:
--
--   .......Q
--   .....Q..
--   ...Q....
--   ........
--   ......Q.
--   ........
--   ........
--   ........
--
-- The first exercise is warmup. We'll define two helper functions that we're
-- going to use later: nextRow, and nextCol. nextRow increases the row by one
-- and sets column to 1. nextCol only increases the column by one. (By analogy,
-- nextRow works like line break and carriage return while nextCol works like
-- the space bar in a typewriter.)
--
-- Examples:
--   nextRow (1,1) ==> (2,1)
--   nextRow (4,7) ==> (5,1)
--   nextCol (1,1) ==> (1,2)
--   nextCol (4,7) ==> (4,8)
--
-- Before we start, remember type aliases? We define some of them just to make
-- the roles of different function arguments clearer without adding syntactical
-- overhead:

type Row   = Int
type Col   = Int
type Coord = (Row, Col)

nextRow :: Coord -> Coord
nextRow (i,j) = (i + 1, 1)

nextCol :: Coord -> Coord
nextCol (i,j) = (i, j + 1)

--------------------------------------------------------------------------------
-- Ex 2: Implement the function prettyPrint that, given the size of
-- the chessboard and a list of distinct coordinates of queens (that
-- is, a list of (row,col) pairs), prints the chessboard with the
-- queens on it. Empty squares must be printed as '.'s and queens as
-- 'Q's. The special line break character '\n' must be appended to the
-- end of each row.
--
-- Examples:
--   prettyPrint 3 [(1,1),(2,3),(3,2)] ==> "Q..\n..Q\n.Q.\n"
--   prettyPrint 3 [(2,3),(1,1),(3,2)] ==> "Q..\n..Q\n.Q.\n"
--   prettyPrint 3 [(1,3),(2,1),(3,2)] ==> "..Q\nQ..\n.Q.\n"
--
-- To see how the result looks like with the line breaks correctly printed, use
-- putStrLn in GHCI. To open this module in GHCI, run 'stack ghci Set9b.hs'.
--
-- Examples:
--   *Set9b> putStrLn $ prettyPrint 4 [(1,1),(1,4),(4,1),(4,4)]
--   Q..Q
--   ....
--   ....
--   Q..Q
--
--   *Set9b> putStrLn $ prettyPrint 4 [(4,4),(1,4),(1,1),(4,1)]
--   Q..Q
--   ....
--   ....
--   Q..Q
--
--   *Set9b> putStrLn $ prettyPrint 7 [(1,1),(2,3),(3,5),(4,7),(5,2),(6,4),(7,6)]
--   Q......
--   ..Q....
--   ....Q..
--   ......Q
--   .Q.....
--   ...Q...
--   .....Q.
--
-- Hint: Remember the function elem? elem x xs checks if the list xs contains
-- the element x, e.g. elem 1 [2,5,1] ==> True, elem 1 [2,5,2] ==> False.
--
-- Challenge: Try defining prettyPrint without elem by just iterating over all
-- coordinates one at a time. (For those who've had a course in data structures
-- and algorithms, this challenge is about finding an O(n^2) solution in terms
-- of the width (or height) n of the chess board; the na??ve solution with elem
-- takes O(n^3) time. Just ignore the previous sentence, if you're not familiar
-- with the O-notation.)

emptyBoard :: Size -> String
emptyBoard size = go size where
    go 0 = ""
    go counter = replicate size '.' ++ "\n" ++ go (counter - 1)

replaceChar :: Int -> Char -> String -> String
replaceChar idx newChar str = take idx str ++ [newChar] ++ drop (idx + 1) str

prettyPrintFast :: Size -> [Coord] -> String
prettyPrintFast size [] = emptyBoard size
prettyPrintFast size coords = go (sort coords) 1 1 where
    go :: [Coord] -> Int -> Int -> String
    go [] row col
     | row > size = ""
     | otherwise = replicate (size - col + 1) '.' ++ "\n" ++ go [] (row + 1) 1
    go cs@(coord':coords') row col
     | row > size = ""
     | coord' == (row, col) = (if col == size then "Q\n" else "Q") ++ go coords' nextRow nextCol
     | col == size = ".\n" ++ go cs (row + 1) 1
     | otherwise = replicate colsRemaining '.' ++ go cs row (col + colsRemaining)
     where 
         colsRemaining
            | row == fst coord' = snd coord' - col
            | otherwise = size - col
         nextRow = if col == size then row + 1 else row
         nextCol = if col == size then 1 else col + 1

-- "Naive" solution
prettyPrintComp :: Size -> [Coord] -> String
prettyPrintComp size coords = [ newChar | row <- [1..size], col <- [1..size+1],
    let newChar | (row, col) `elem` coords = 'Q' | col==size+1 = '\n' | otherwise = '.'
        ]

-- This is horrendously slow!
prettyPrintRep :: Size -> [Coord] -> String
prettyPrintRep size = foldr (\(row, col) board -> 
    replaceChar ((row - 1) * (size+1) + (col - 1)) 'Q' board ) (emptyBoard size)

prettyPrint = prettyPrintFast

makeCoords :: Int -> [(Int, Int)]
makeCoords size = [ (i, i `mod` 40) | i <- [1..size] ]

--------------------------------------------------------------------------------
-- Ex 3: The task in this exercise is to define the relations sameRow, sameCol,
-- sameDiag, and sameAntidiag that check whether or not two coordinates of the
-- form (i,j) :: (Row, Col) on a table of indeterminate size are on the same
-- column, diagonal (top left to bottom right), or antidiagonal (bottom left to
-- top right) respectively. Indeterminate size of the table means that these
-- relations should work for tables of all sizes. (You may assume that all
-- coordinates will be positive.)
--
-- Examples:
--   sameRow (1,1) (1,1) ==> True
--   sameRow (1,1) (2,1) ==> False
--   sameRow (1,1) (1,2) ==> True
--   sameCol (1,1) (4,1) ==> True
--   sameCol (1,1) (4,2) ==> False
--   sameDiag (1,1) (2,2) ==> True
--   sameDiag (1,1) (1,2) ==> False
--   sameAntidiag (1,1) (1,2) ==> False
--   sameAntidiag (2,10) (5,7) ==> True
--   sameAntidiag (500,5) (5,500) ==> True

sameRow :: Coord -> Coord -> Bool
sameRow (i,j) (k,l) = i == k

sameCol :: Coord -> Coord -> Bool
sameCol (i,j) (k,l) = j == l

sameDiag :: Coord -> Coord -> Bool
sameDiag (i,j) (k,l) = (k - i) == (l - j)

sameAntidiag :: Coord -> Coord -> Bool
sameAntidiag (i,j) (k,l) = (k - i) == negate (l - j)

--------------------------------------------------------------------------------
-- Ex 4: In chess, a queen may capture another piece in the same row, column,
-- diagonal, or antidiagonal in one step. This danger zone, where pieces can be
-- captured by a queen (indicated here with the character '#') looks like this:
--
--   .#.#.#..
--   ..###...
--   ###Q####
--   ..###...
--   .#.#.#..
--   #..#..#.
--   ...#...#
--   ...#....
--
-- For multiple queens, the danger zone is the union of the danger zones for
-- individual queens. This means that all coordinates belonging to the danger
-- zones of one or more individual queens also belongs to the collective danger
-- zone of all queens on the board. For example, if we add a second queen to the
-- coordinates (4,6), the danger zone grows:
--
--   .###.#..
--   ..####.#
--   ###Q####
--   #####Q##
--   .#.####.
--   #..#.###
--   ..##.#.#
--   .#.#.#..
--
-- Implement the function danger that checks if a coordinate belongs to the
-- collective danger zone of the given list of (coordinates of) queens.
-- Graphically speaking, we want to check if the square at the given coordinates
-- looks like '.' rather than '#'. (You may assume that the given coordinate
-- will be different from all the coordinates in the stack.)
--
-- Examples:
--  danger (5,2) [] ==> False
--  danger (5,2) [(1,2)] ==> True
--  danger (5,2) [(4,3)] ==> True
--  danger (4,5) [(3,4),(4,6)] ==> True
--  danger (5,3) [(3,4),(4,6)] ==> False
--  danger (5,3) [(3,4),(4,6),(7,5),(6,2),(8,1)] ==> True
--
-- Hint: Use the relations of the previous exercise!
--
-- Lists of coordinates of queens will be later used in a First in Last Out
-- (LIFO) manner, so we give this type the alias Stack:
-- https://en.wikipedia.org/wiki/Stack_(abstract_data_type)

type Size      = Int
type Candidate = Coord
type Stack     = [Coord]

danger :: Candidate -> Stack -> Bool
danger coord = any danger' where
    danger' queen = any (($ coord) . ($ queen)) [sameRow, sameCol, sameDiag, sameAntidiag]

--------------------------------------------------------------------------------
-- Ex 5: In this exercise, the task is to write a modified version of
-- prettyPrint that marks those empty squares with '#' that are in the
-- collective danger zone of the given stack of queens. You may assume that
-- none of the queens in the stack are in the danger zone of another queen.
--
-- Examples:
--   *Set9b> putStrLn $ prettyPrint2 3 []
--   ...
--   ...
--   ...
--
--   *Set9b> putStrLn $ prettyPrint2 4 [(1,2),(2,4)]
--   #Q##
--   ###Q
--   .###
--   .#.#
--
--   *Set9b> putStrLn $ prettyPrint2 9 [(5,5)]
--   #...#...#
--   .#..#..#.
--   ..#.#.#..
--   ...###...
--   ####Q####
--   ...###...
--   ..#.#.#..
--   .#..#..#.
--   #...#...#
--
-- (For those that did the challenge in exercise 2, there's probably no O(n^2)
-- solution to this version. Any working solution is okay in this exercise.)

prettyPrint2Comp :: Size -> Stack -> String
prettyPrint2Comp size queens = [ newChar | row <- [1..size], col <- [1..size+1],
    let newChar | (row, col) `elem` queens = 'Q' | col==size+1 = '\n' | danger (row, col) queens = '#' |  otherwise = '.'
        ]

type CoordQDanger = ((Int, Int), Char)

dangerMap :: Int -> Stack -> [CoordQDanger]
dangerMap size queens = [((row, col), '#') | row <- [1..size], col <- [1..size], danger (row, col) queens,
   (row, col) `notElem` queens]

dangerMapFast :: Int -> Stack -> [CoordQDanger]
dangerMapFast 0 _ = []
dangerMapFast _ [] = []
dangerMapFast size queens = filter (\x -> fst x `notElem` queens) ((sort . nub) $ concatMap (($ queens) . concatMap) [rowCoords, colCoords, diagCoords, antiDiagCoords]) where

    rowCoords :: Coord -> [CoordQDanger]
    rowCoords queenCoord = [((row, col), '#') | col <- [1..size], col /= snd queenCoord, let row = fst queenCoord]

    colCoords :: Coord -> [CoordQDanger]
    colCoords queenCoord = [((row, col), '#') | row <- [1..size], row /= fst queenCoord, let col = snd queenCoord]

    antiDiagCoords :: Coord -> [CoordQDanger]
    antiDiagCoords queenCoord = makeDiag startCoord where
        makeDiag (row, col)
         | row > size || col > size = []
         | (row, col) == queenCoord = makeDiag (row+1, col+1)
         | otherwise = ((row, col), '#'):makeDiag (row+1, col+1)
        startCoord = uncurry go queenCoord
        go row' col' = if row' == 1 || col' == 1 then (row', col') else go (row' - 1) (col' - 1)

    diagCoords :: Coord -> [CoordQDanger]
    diagCoords queenCoord = makeDiag startCoord where
        makeDiag (row, col)
         | row < 1 || col > size = []
         | (row, col) == queenCoord = makeDiag (row-1, col+1)
         | otherwise = ((row, col), '#'):makeDiag (row-1, col+1)
        startCoord = uncurry go queenCoord
        go row' col' = if row' == size || col' == 1 then (row', col') else 
            go (row' + 1) (col' - 1)


prettyPrint2Fast :: Size -> Stack -> String
prettyPrint2Fast size [] = emptyBoard size
prettyPrint2Fast size coords = go (sort ((map (,'Q') coords) ++ 
    dangerMapFast size coords)) 1 1 where
    go :: [CoordQDanger] -> Int -> Int -> String
    go [] row col
     | row > size = ""
     | otherwise = replicate (size - col + 1) '.' ++ "\n" ++ go [] (row + 1) 1
    go cs@(coord':coords') row col
     | row > size = ""
     | fst coord' == (row, col) = if col == size then snd coord':'\n':nextRecurse else snd coord':nextRecurse
     | col == size = '.':'\n':go cs (row + 1) 1
     | otherwise = replicate colsRemaining '.' ++ go cs row (col + colsRemaining)
     where 
         nextRecurse = go coords' nextRow nextCol
         colsRemaining
            | row == fst (fst coord') = snd (fst coord') - col
            | otherwise = size - col
         nextRow = if col == size then row + 1 else row
         nextCol = if col == size then 1 else col + 1

prettyPrint2 = prettyPrint2Fast

--------------------------------------------------------------------------------
-- Ex 6: Now that we can check if a piece can be safely placed into a square in
-- the chessboard, it's time to write the first piece of the actual solution.
--
-- Given the size of the chessboard and a stack, the function fixFirst
-- should take the queen on the top of the stack, and if it is in
-- danger, move it right _along the same row_ (in the direction of
-- increasing columns) until it is not in danger.
--
-- If no safe spot is found for the queen on that row, fixFirst should
-- return Nothing.
--
-- Examples:
--   fixFirst 5 [(1,1)] ==> Just [(1,1)]
--   fixFirst 5 [(3,4)] ==> Just [(3,4)]
--   fixFirst 5 [(1,1),(1,5)] ==> Nothing
--   fixFirst 5 [(1,1),(3,3)] ==> Just [(1,2),(3,3)]
--   fixFirst 5 [(1,3),(3,3)] ==> Just [(1,4),(3,3)]
--   fixFirst 5 [(2,1),(3,3)] ==> Just [(2,1),(3,3)]
--   fixFirst 8 [(8,1),(1,1)] ==> Just [(8,2),(1,1)]
--   fixFirst 8 [(4,1),(3,4),(4,6)] ==> Nothing
--   fixFirst 8 [(6,1),(3,4),(4,6)] ==> Just [(6,2),(3,4),(4,6)]
--   fixFirst 8 [(5,1),(3,8),(4,6),(7,5),(6,2),(8,1)] ==> Nothing
--
-- Hint: Remember prettyPrint and prettyPrint2? They might be useful
-- for debugging. For example we can run this to see what's happening
-- in that last example. The whole fifth row is in danger zone.
--
--   putStrLn $ prettyPrint2 8 [(3,8),(4,6),(7,5),(6,2),(8,1)]
--     ###.####
--     ##.#####
--     #######Q
--     #####Q##
--     ########
--     #Q######
--     ####Q###
--     Q#######

fixFirst :: Size -> Stack -> Maybe Stack
fixFirst _ [] = Nothing
fixFirst size q@(queen:queens)
    | snd queen > size = Nothing
    | danger queen queens = fixFirst size ((fst queen, snd queen + 1):queens)
    | otherwise = Just q

--------------------------------------------------------------------------------
-- Ex 7: We need two helper functions for stack management.
--
-- * continue moves on to a new row. It pushes a new candidate to the
--   top of the stack (front of the list). The new candidate should be
--   at the beginning of the next row with respect to the queen
--   previously on top of the stack.
--
-- * backtrack moves back to the previous row. It removes the top
--   element of the stack, and adjusts the new top element so that it
--   is in the next column.
--
-- Examples:
--   continue [(1,1)] ==> [(2,1),(1,1)]
--   continue [(2,3),(1,1)] ==> [(3,1),(2,3),(1,1)]
--   backtrack [(8,1),(7,5),(6,2),(4,6),(3,4)] ==> [(7,6),(6,2),(4,6),(3,4)]
--
-- Hint: Remember nextRow and nextCol? Use them!

continue :: Stack -> Stack
continue [] = []
continue q@(queen:queens) = nextRow queen:q

backtrack :: Stack -> Stack
backtrack [] = []
backtrack [x] = []
backtrack (_:newTop:queens) = nextCol newTop:queens

--------------------------------------------------------------------------------
-- Ex 8: Let's take a step. Our algorithm solves the problem (in a
-- greedy manner) one row at a time, backtracking when needed. The
-- reason why we need backtracking is the following. We can greedily
-- put the queens to (1,1) and (2,3) and end up with no safe spot on
-- the third row:
--
--   Q###
--   ##Q#
--   ####
--   #.##
--
-- However if we backtrack and move the queen from (2,3) to (2,4), we
-- are able to place the third queen:
--
--   Q###
--   ###Q
--   #.##
--   ##.#
--
-- Implement the function step that takes the size of a board and a
-- stack, and tries to fix the position of the queen on the top of the
-- stack (using fixFirst). If a new position is found, the function
-- should call continue to return a stack with a new candidate. If a
-- safe position is not found, the function should call backtrack to
-- return a new stack.
--
-- Examples:
--
--   The first candidate is safe so we continue directly:
--     step 4 [(1,1)] ==> [(2,1),(1,1)]
--
--     Q...     Q...
--     .... ==> Q...
--     ....     ....
--     ....     ....
--
--   The second candidate needs to be adjusted a bit before a third is added:
--     step 4 [(2,1),(1,1)] ==> [(3,1),(2,3),(1,1)]
--
--     Q...     Q...
--     Q... ==> ..Q.
--     ....     Q...
--     ....     ....
--
--   No safe position is found for the third queen so we backtrack:
--     step 4 [(3,1),(2,3),(1,1)] ==> [(2,4),(1,1)]
--
--     Q...     Q...
--     ..Q. ==> ...Q
--     Q...     ....
--     ....     ....
--
--   The new position of the second queen is ok so we move to the third row:
--     step 4 [(2,4),(1,1)] ==> [(3,1),(2,4),(1,1)]
--
--     Q...     Q...
--     ...Q ==> ...Q
--     ....     Q...
--     ....     ....
--
--   More examples:
--     step 8 [(4,2),(3,5),(2,3),(1,1)] ==> [(5,1),(4,2),(3,5),(2,3),(1,1)]
--     step 8 [(5,1),(4,2),(3,5),(2,3),(1,1)] ==> [(6,1),(5,4),(4,2),(3,5),(2,3),(1,1)]
--     step 8 [(6,1),(5,4),(4,2),(3,5),(2,3),(1,1)] ==> [(5,5),(4,2),(3,5),(2,3),(1,1)]

step :: Size -> Stack -> Stack
step size queens = case fixFirst size queens of
    Nothing -> backtrack queens
    Just fixedQueens -> continue fixedQueens

--------------------------------------------------------------------------------
-- Ex 9: Let's solve our puzzle! The function finish takes a partial
-- solution (stack) and repeatedly step until a complete solution is
-- found.
--
-- Reminder: a complete solution has n queens that don't threaten each
-- other. One easy way to know you have a valid solution is when step
-- adds the (n+1)th queen.
--
-- After this, it's just a matter of calling `finish n [(1,1)]` to
-- solve the n queens problem.

finish :: Size -> Stack -> Stack
finish _ [] = []
finish size partialSolution
    | length partialSolution == size + 1 = tail partialSolution
    | otherwise = finish size (step size partialSolution)

solve :: Size -> Stack
solve n = finish n [(1,1)]
