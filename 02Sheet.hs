-- | Functional Programming, course at LMU, summer term 2012
--   Andreas Abel and Steffen Jost
--
-- Exercise sheet 2, 2012-05-03
--
-- Instructions:
--
-- Replace all occurrences of 'undefined' and 'Undefined' by sensible code.
-- Do not change the type of functions you are asked to implement!
--
-- Submit your solutions via UniWorX.

----------------------------------------------------------------------
-- Header
--
-- You can add imports and LANGUAGE pragmas here.
----------------------------------------------------------------------

module Main where

import Control.Applicative ((<$>))
import Control.Monad

import qualified Data.List as List

import Debug.Trace

import System.Directory
import System.Environment
import System.FilePath

----------------------------------------------------------------------
-- Exercise 1  Sudoku-Board as list of lists
----------------------------------------------------------------------

-- | A completely filled Sudoku board is represented as a list of @n*n@ rows
--   which are each a list of @n*n@ numbers between 1 and @n*n@ (inclusively).
type Sudoku = [Row]
type Row    = [Int]

-- | A (completely filled) Sudoku board is valid if all its @rows@, @columns@
--   ans @n×n@-@blocks@ are permutations of @1..n*n@.
checkRow :: Int -> Row -> Bool
checkRow n x = List.sort x == [1 .. n*n]

checkRows :: Int -> Sudoku -> Bool
checkRows n [] = True
checkRows n (x:xs) = (checkRow n x) && checkRows n xs 

boxes :: Int -> Sudoku -> Sudoku
boxes n s = boxes_acc n s []

boxes_acc :: Int -> Sudoku -> [Sudoku] -> Sudoku
boxes_acc n s acc | length (head s) == 0 = concatBox n acc []
                  | otherwise            = boxes_acc n s1 (x:acc)
    where (x, s1) = (map (take n) s, map (drop n) s) 

concatBox :: Int -> [Sudoku] -> [Sudoku] -> Sudoku 
concatBox n []   acc = concat acc
concatBox n (x:xs) acc = concatBox n xs ((merge n x []):acc) 
    where merge n [] acc = acc
          merge n xs acc = merge n (drop n xs) ((concat (take n xs)):acc)

validSudoku :: Int -> Sudoku -> Bool
validSudoku n s = (checkRows n s) && (checkRows n (List.transpose s)) && (checkRows n (boxes n s))

-- TEST validSudoku 2 [[1,2,3,4],[4,3,2,1],[2,1,4,3],[3,4,1,2]]
-- TEST validSudoku 2 [[2,1,3,4],[4,3,2,1],[2,1,4,3],[3,4,1,2]]

----------------------------------------------------------------------
-- Exercise 2 Printing the directory tree
----------------------------------------------------------------------

-- | Print the directory tree recursively, starting with directory @root@.
--   Hidden files are directories are skipped.
--
--   For instance, a directory structure
--
--     @
--     root
--       aDir
--         aFile1
--       bDir
--         cDir
--           cFile1
--       dFile
--
--     @
--
--  is printed as
--
--    @
--       aDir
--       bDir
--         cDir
--    @
--
--  with sensible indentation to express child and sibling relations.
mainDir :: FilePath -> IO ()
mainDir root = do 
    putStr root
    putStr "\n"
    printRoot 1 root

printRoot :: Int -> FilePath -> IO ()
printRoot n root = 
    do b <- doesDirectoryExist root
       when b $
        do content <- getDirectoryContents root
           mapM_ (printDir root) (reverse content)
        where
            printDir root r | r == "." || r == ".." = do return ()
                            | otherwise             = do g <- doesDirectoryExist (root </> r)
                                                         when g $ do {printLine n r; printRoot (n + 1) (root </> r)}

printLine :: Int -> FilePath -> IO ()
printLine 0 r = do putStr r
                   putStr "\n"
printLine n r = do putStr "  "
                   printLine (n - 1) r                                                         

main :: IO ()
main = do
  args <- getArgs
  mainDir $ if null args then "." else head args

----------------------------------------------------------------------
-- Exercise 3  Lazy monadic Boolean operators
----------------------------------------------------------------------

-- Conjunction and Disjunction are lazy

verbose :: Bool -> Bool
verbose b = trace ("encountered " ++ show b) b

tA = [True, False, True, False]
tO = [False, True, False, True]

testA = and $ map verbose tA
testO = or  $ map verbose tO

-- Naive adaption to a monad makes them strict

verboseM :: Bool -> IO Bool
verboseM b = putStrLn ("encountered " ++ show b) >> return b

testMA = and <$> mapM verboseM tA
testMO = or  <$> mapM verboseM tO

-- Implement lazy monadic conjunction and disjunction!

andM :: Monad m => [m Bool] -> m Bool
andM [] = return True
andM (x:xs) = do b <- x
                 if b then (andM xs) else return False 

orM  :: Monad m => [m Bool] -> m Bool
orM [] = return False
orM (x:xs) = do b <- x
                if b then return True else orM xs 

-- The following test cases should only print those Booleans
-- that are necessary to determine the result of the operation!

testLA = andM $ map verboseM tA
testLO = orM  $ map verboseM tO
