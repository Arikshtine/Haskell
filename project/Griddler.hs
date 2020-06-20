
----------------- imports -----------------

import Data.List
import Data.Maybe
import Data.Monoid
import Control.Applicative 
import Control.Monad


------------------ types ------------------

data LinkedList a = None | Node { lHead::a, lTail::(LinkedList a)} deriving (Eq,Show)

type Row a = [a]
type Grid a = [Row a]
type RowConstraints = [[Int]]
type ColConstraints = [[Int]]
type Cell = Maybe Bool
type MarkedCell = Bool


------------------ solver ------------------

-- prints the first solution if the grid was solved. there can be many solution (e.g. chess board griddler).

griddlerSolver :: RowConstraints -> ColConstraints -> IO()
griddlerSolver rows cols 
    | solved grid =  putStr ( showGrid rows cols grid)      -- prints the first solution.
    | otherwise = putStr "Failure :(\n"
    where grid = getSolution rows cols
          solved grid = grid /= []

{--
returns the head of the list of the valid solutions for the grid, (might return []).
gets the partial solution, and use it to guess all possible complete solutions based on it.
filter all solutionts that do not complys with the constraints:
e.g. for a 2X2 grid, row constraints [[1],[1]] , partial solution  [[Nothing,Nothing ],
                                                                    [Nothing ,Nothing ]]
we get all possibilities: [
                        [[True,False],    [[True,False],    [[False,True],    [[False,True],
                        [True,False]],    [False,True]],    [True,False]],    [False,True]]     ]

and then filter them by the cols constraints, and take the first one.
--}

getSolution :: RowConstraints -> ColConstraints -> Grid Bool
getSolution rows cols = head' [sol' | sol <- attemptedSolution, sol' <- guessFullSol rows sol, complyWith cols sol'] -- lazieness
    where attemptedSolution = tryToSolve rows cols
          rowsLen = length cols
          guessFullSol = zipWithM (guessAll rowsLen)


-- an head function that doesn't fails on empty list.

head' :: [Grid Bool]-> Grid Bool 
head' [] = []
head' (x:xs) = x


-- returns True iff the cols constraints are fulfilled in the grid.

complyWith:: ColConstraints -> Grid Bool -> Bool                                   
complyWith cols grid = ( cols == getGridRowsCells (transpose grid) )


{--
counts the filled sequances in every row of the grid.
for the grid - [[True, True, False, True], [True, False, False, True], [True, True, False, False]] 
we will map for every row the function group as follow:
                                            [True, True, False, True] -> [[True,True],[False],[True]]
then, we will filter every group that starts with True:
                                            [[True,True],[False],[True]] -> [[True,True], [True]]
then, we map the function length, to get the length of every marked sequance:
                                            [[True,True], [True]] -> [2, 1]
in the end, we get a matrix of all the marked sequances in the grid.
--}

getGridRowsCells :: Grid Bool -> RowConstraints
getGridRowsCells = map countFilledCellsInRow 


countFilledCellsInRow :: Row Bool -> Row Int
countFilledCellsInRow r = map (length)  $ filter head  (group r) -- length of all [True] groups in a row.


{-- 
returns a singleton list containing a grid which is solved as much as possible.
might return a grid of Nothing.
--}

tryToSolve :: RowConstraints -> ColConstraints -> [Grid Cell]
tryToSolve rows cols = maybeToList (iter applyHeuristics grid)          -- a singleton list
    where m = length rows
          n = length cols
          grid = basicFill m n rows cols
          applyHeuristics x = (overlaps m cols . transpose) x >>= (overlaps n rows . transpose) 


-- partially mark each row, and zips them together, into a partially solved grid.

overlaps :: Int -> RowConstraints -> Grid Cell -> Maybe (Grid Cell) 
overlaps x = zipWithM (simpleBoxes x)  -- apply simsimpleBoxes to every row in the grid, and zip the results


--  iterate with f, until convergence to a static solution.

iter :: (Monad m, Eq a) => (a -> m a) -> a -> m a
iter f x = do
        y <- f x
        if x /= y then iter f y  else return x
 

-------------- first heuristic --------------

{-- 
fill any line that can be completely filled immediately.
e.g. if row constraints are [1,2] and row length is 4, then row is [True, False, True, True].
--}

basicFill :: Int -> Int -> RowConstraints -> ColConstraints -> Grid (Maybe Bool)
basicFill m n rows cols = gridToMaybe $ fillBy cols ( fillBy rows initGrid )
    where initGrid = emptyGrid m n
          fillBy row grid = transpose $ fillRow row grid 
          gridToMaybe = map (map boolToMaybe) 
          boolToMaybe = (\bool -> if bool then (Just True) else Nothing)

          
-- returns an initialized grid

emptyGrid:: Int -> Int -> Grid Bool
emptyGrid m n = replicate m (take n $ cycle[False])  -- laziness - take a part of an infinite list.

          
{-- 
checks if a line can be completely filled, and fill it.
as before, if row is [2,1], and length is 4, then 2-1 (length of row constraints) + 2+1(sum of marked cells) = 4 (row length).
--}

fillRow :: RowConstraints -> Grid Bool ->  Grid Bool
fillRow  [] [] = []
fillRow  (r:rows) (g:grid)
    |(length r - 1 + sum r == length g) = (toBool r) : fillRow rows grid
    |otherwise = g : fillRow rows grid  -- fill next row in the grid.


-- converts a row constraints to a grid row - e.g. [2,1] -> [True, True, False, True].

toBool :: Row Int -> Row Bool
toBool [] = []
toBool [n] = nTrue n 
toBool (n:row) = nTrue n ++ [False] ++ toBool row 


nTrue :: Int -> Row Bool
nTrue n = replicate n True


------------- second heuristic -------------

{-- 
gets all the possible ways to mark the cells, according to the row constraints, 
in a row of length len, that matches the current marked cells.
e.g. in a row of length 5, constraint of [4], we get:
                                                     [0,|0,0,0|, ]
                                                     [ ,|0,0,0|,0]
so we can mark the overlap, [ ,0,0,0, ].
--}

simpleBoxes :: Int -> Row Int -> Row Cell -> Maybe (Row Cell)  
simpleBoxes len constraints row 
    | result == [] = Nothing                                                    
    | otherwise = Just (foldr1 (zipWith markOverlap) $ gridToJust result)  -- foldr1 unites rows, zipWith unites corresponding cells in 2 rows
    where result = guessAll len constraints row   
          gridToJust g = map (map Just) g
          markOverlap = (\a b -> if a == b then a else Nothing)
  

{--
returns all the possible way to mark cells in the row:
for the row [Nothing, Nothing, Nothing, Nothing] of length 4 and row constraints [1,1],
we get all the possibilities - [[True,False,True,False],[True,False,False,True],[False,True,False,True]]
--}

guessAll :: Int -> Row Int -> Row Cell -> Grid Bool
guessAll len [] [] = [[]]
guessAll len row [] = []
guessAll len row (first : rest) = case first of Nothing -> getAll True ++ getAll False           -- guess both options
                                                (Just a) -> getAll a                             -- guess using the current info.
                                                where getAll bool  = guessAll' len row bool rest
 

                                                                                           
guessAll' :: Int -> Row Int -> MarkedCell -> Row Cell -> Grid Bool
guessAll' len row False rest = [False : row' | row' <- guessAll (len - 1) row rest]
guessAll' len [n] True rest = [nTrue n ++ nFalse (len - n)  | fits len n rest True]
guessAll' len (n:row) True rest = [nTrue n ++ False : rest' | fits len n rest False, rest' <- getAll']
    where (_, after) = splitAt (n) rest
          getAll' = guessAll (len - n - 1) row after


nFalse :: Int -> Row Bool
nFalse = map (not) . nTrue 


-- checks that a line complys with the constraints.

fits :: Int -> Int -> Row (Maybe Bool) -> Bool -> Bool
fits len i rest lastSequence
    | not lastSequence = len > i + 1 && marked infront && blank /= Just True   
    | otherwise = len >= i && marked infront && unmarked after
    where (infront, after) = splitAt (i - 1) rest
          blank = head after
          marked = all (/= Just False)
          unmarked = all (/= Just True)


------------------ output ------------------

{--
functor of linked list:
1. fmap id None = id None , fmap id list = id list
2. fmap (f . g) None ==  fmap f . fmap g None == fmap f None == None , fmap (f . g) list == fmap f . fmap g list.
--}
instance Functor LinkedList where
    fmap f None = None                                     
    fmap f (Node val list) = Node (f val) (fmap f list)


{-- 
monoid of linked list - when there are 2 lists, we create a new list where we "merge" the lists
into a single list - e.g. given l1 = Noda 1 {Node 2 None} , l2 = Noda 3 {Node 4 None},
l1 mappend l2 will return Noda 1 {Node 2 {Noda 3 {Node 4 None}}}
1. closed under operation
2. a * ( b * c ) = ( a * b ) * c
3. None * a = a * None = a
--}
instance Monoid (LinkedList a) where      
    mempty = None 
    mappend list None = list
    mappend None list = mappend list None
    mappend (Node val list1) list2 = Node val $ mappend list1 list2    


{--
Applicative of linked list - applying a linked list of function on a linked list of args, 
will return a linked list that contain all results of every function applyed to the args linked list:
for l1 = Noda (1+) {Node (2*) None}, l2 = Noda 1 {Node 2 None} we will get Noda 2 {Node 3 {Noda 2 {Node 4 None}}}
--}
instance Applicative LinkedList where
    pure a = Node a None
    None <*> list = None
    (Node f lTail) <*> list = (fmap f list) `mappend` (lTail <*> list)


{--
monad of linked list - uses applicative and monoid of linked list.
--}
instance Monad LinkedList where
    return = pure
    None >>= f = None
    (Node lHead lTail) >>= f = (f lHead) `mappend` (lTail >>= f)


-- converts a haskell list to linkedlist.

listToLinkedList :: [a] -> LinkedList a 
listToLinkedList [] = None 
listToLinkedList [x] = Node (x) None
listToLinkedList (x:xs) = Node (x) (listToLinkedList (xs))


-- zips 2 linked lists into one linked list.

linkedListZip :: (a->a->a) -> LinkedList a ->LinkedList a -> LinkedList a
linkedListZip f None l = None
linkedListZip f l None = linkedListZip f None l
linkedListZip f list1 list2 = (Node (f (lHead list1) (lHead list2)) (linkedListZip f (lTail list1) (lTail list2)))


-- converts a string linked list to a string.

toString :: LinkedList String -> String 
toString None  = ""
toString list = (lHead list ) ++ (toString (lTail list))


{--
shows the solved grid:
converts the col constraints to a string.
converts the solved grid to a linked list where every node contains a row from the grid.
converts the row constraints to a linked list where every node contains a row.
zip every grid row with it's corresponding constraint, then ++ the next line, then ++ the col constraints.
return a single string representing the grid, ready to be displayed. 
--}

showGrid :: RowConstraints -> ColConstraints -> Grid Bool -> String
showGrid rows cols grid = result
    where colsStr = unlines $ showCols cols
          lGrid = (myUnwords.map (cellToChar)) <$> (listToLinkedList grid)        -- fmap - functor.                   
          lRows = (listToLinkedList rows)>>= showRow                              -- monad - uses monoid + applicative.    
          result = (toString (linkedListZip (++) lGrid lRows ) ) ++ colsStr


{--
adding all the "||0" or "|| " together to a single line:
returns a string that looks like this - "||0||0|| ||0"
--}
myUnwords :: [String] -> String
myUnwords [] = []
myUnwords (s:ss) = s ++ myUnwords ss


-- converts every cell to a representing string.

cellToChar::Bool -> String
cellToChar = (\bool-> if bool == True then "||0" else "|| ")


-- shows a row of constraints - [1,2] -> Node (|| 1 2\n) None

showRow :: Row Int -> LinkedList String                           
showRow row = Node ("|| " ++  unwords (map show row) ++ "\n") None


-- 

showCols :: ColConstraints -> Grid Char 
showCols cols
        | all null cols = []
        | otherwise = concatMap showCol cols : showCols (map next cols)
        where next = (\xs -> if null xs then [] else tail xs)


-- format the column to a string

showCol:: Row Int -> String
showCol [] = "   "
showCol (x:_)
        | x > 9 = " " ++ show x
        | otherwise = "  " ++ show x 


------------------ Tests ------------------

-- test are taken from the website https://www.puzzle-nonograms.com/specfic.php

---- test 1 , 5x5 Nonograms Puzzle ID: 9,447,489

r1 = [[3],[1],[1],[1,3],[1,3]]            
c1 = [[2,2],[1],[1,2],[2],[3]]
test1 = griddlerSolver r1 c1 

---- test 2 , 5x5 Nonograms Puzzle ID: 3,412,818
r2 = [[2],[3],[4],[2],[1,1]]
c2 = [[1],[1],[3],[4],[4]]
test2 = griddlerSolver r2 c2

---- test 3 , 5x5 Nonograms Puzzle ID: 4,387,970
r3 = [[2],[1],[3],[3],[3,1]]
c3 = [[4],[3],[3],[1],[1,1]]    
test3 = griddlerSolver r3 c3

---- test 4 , 10x10 Nonograms Puzzle ID: 4,598,716
r4 = [[1,2],[2],[1,2],[3,4],[6],[2,1,3],[2,3],[3],[5,2],[8]]
c4 = [[1,2],[2],[1,2],[4,2],[2,2],[2,2],[1,3,2],[8,1],[4,4],[1,4]]
test4 = griddlerSolver r4 c4

---- test 5 , 10x10 Nonograms Puzzle ID: 4,069,661
r5 = [[5],[5],[2,2],[1,5],[6],[7],[5],[2,1,1],[4],[4]]   
c5 = [[4,3],[3,3],[2,2],[2,1,2],[2,3],[4],[5],[5],[5],[3,1]]
test5 = griddlerSolver r5 c5

---- test 6 , 10x10 Nonograms Puzzle ID: 5,365,544
r6 = [ [3, 2] , [3, 3] , [5] , [4] , [1, 4] , [1, 4] , [1, 4] , [2, 2] , [3, 2] , [3, 3]]
c6 = [[1, 2] , [1, 3] , [2, 5] , [2] ,[6, 1] , [5, 2] , [8] , [7], [3], [2]]
test6 = griddlerSolver r6 c6

---- test 7 , 15x15 Nonograms Puzzle ID: 7,325,490
r7 = [[5,1,1], [4,2],[1,3,1,1,2],[1,5,2],[4,2],[1,6,1],[4,5],[11],[3,4,2],[2,2],[2,2,1],[3,3,3],[2,3,3],[2,4,1],[2,6]]
c7 = [[2,10],[9],[1,3,1],[2,2],[3,2],[3,3],[4,10],[6,5],[6,4],[6,2],[7,1],[1,4],[1,3],[5,2],[5]]
test7 = griddlerSolver r7 c7

---- test 8 , 15x15 Nonograms Puzzle ID:  3,603,716
r8 = [[10],[1,6,4],[1,4,1],[5,2],[5,4],[5,3],[2,4,1,1],[2,4,1,1],[2,1,3,2],[1,6],[2,1,1,4],[2,2],[4,1,2],[1,2],[4,3,1,1]]
c8 = [[2,5,1],[4,1,1],[1,1,1],[2,5,3],[8,2],[8,2],[5,3,1],[5],[2,3,1],[1,1,3,1,1],[2,6,1],[2,2,2],[6,2,1],[1,1,6],[7]]
test8 = griddlerSolver r8 c8

---- test 9 , 20x20 Nonograms Puzzle ID: 3,438,569
r9 = [[1, 2, 4], [4, 1, 1], [1, 4, 2],[ 4, 1],[ 1, 1],[ 2, 1, 3, 1, 1],[ 3, 5, 5],[ 3, 1, 2, 5],[ 3, 2, 4, 3],[ 2, 3, 2], [2, 13],[ 17, 2],[ 2, 5, 8], [2, 4, 4, 2], [2 ,3, 3, 2], [1, 2, 6, 2], [2, 4, 8], [3, 2, 5], [2, 1, 5], [3, 4, 1]]
c9 =  [[9, 4], [15] ,[1, 4, 1, 1, 1], [2, 3, 1], [1, 2, 4, 1],[ 3, 1, 8], [4, 1, 9], [3, 5], [2, 1, 2, 1], [2, 2, 1], [2, 3, 2, 2, 1], [6, 2, 3, 1], [1, 1, 3, 6], [12], [7, 3], [13], [2, 3, 9], [1, 4, 1, 1], [1, 5, 3], [1, 1, 1, 1, 3]]
test9 = griddlerSolver r9 c9

---- test 10 , 25x25 Nonograms Puzzle ID: 6,856,148
r10 = [[3, 2, 1], [5, 3, 2], [1, 3, 3], [2, 3, 6], [2, 4, 1, 5], [5, 5, 2, 1, 3], [5, 5, 1, 1, 2], [2, 5, 1], [2, 4, 1, 1], [4], [6, 4, 2], [8, 7], [2, 4, 9, 2], [2, 6, 1, 4, 7], [6, 1, 13], [6, 3, 3, 3], [4, 5, 7], [2, 4, 1, 1, 5], [6, 4], [1, 1, 2], [3, 4, 4], [2, 2, 3, 2], [14, 1, 1, 1, 3], [10, 1, 1, 3], [2, 12, 3, 3]]
c10 = [[4, 8], [4, 8, 1, 1], [4, 2, 3, 1, 1], [7, 3, 3, 1], [2, 2, 6, 1], [2, 8, 3], [1, 3, 2, 6], [1, 1, 1, 4, 5], [3, 1, 4, 1, 3], [3, 4, 1, 3], [6, 1, 1, 3], [10, 1, 3, 3], [11, 1, 1, 5], [4, 8, 5], [6, 1, 3], [6, 1], [4, 1, 3], [1, 3, 1, 1], [5, 5, 3, 1, 3], [3, 1, 4, 6, 1], [7, 3, 7, 1], [2, 3, 3, 2], [3, 5, 3], [4, 5, 4], [5, 4, 4]]
test10 = griddlerSolver r10 c10


---- run all tests ----
runTests = do
        putStrLn "\n\n----------- test 1 -----------\n\n"
        test1
        putStrLn "\n\n----------- test 2 -----------\n\n"
        test2
        putStrLn "\n\n----------- test 3 -----------\n\n"
        test3
        putStrLn "\n\n----------- test 4 -----------\n\n"
        test4
        putStrLn "\n\n----------- test 5 -----------\n\n"
        test5
        putStrLn "\n\n----------- test 6 -----------\n\n"
        test6
        putStrLn "\n\n----------- test 7 -----------\n\n"
        test7
        putStrLn "\n\n----------- test 8 -----------\n\n"
        test8
        putStrLn "\n\n----------- test 9 -----------\n\n"
        test9
        putStrLn "\n\n----------- test 10 -----------\n\n"
        test10
       
