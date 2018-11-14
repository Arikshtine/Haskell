import Data.List(group)


---1)findNtConway

countNumbers :: String -> String
countNumbers n = concat ([(show (length l)) ++ take 1(l) | l <- (group (n))])

findNtConway :: Int -> String
findNtConway 1 = "1"
findNtConway n = if n > 1 then countNumbers (findNtConway(n-1)) else "ERROR"  


---2)list
data Mylist a = Emptylist | Add {list::Mylist a, value::a}  deriving (Show, Read, Eq, Ord) 

tolist :: Mylist a -> [a]
tolist Emptylist = []
tolist (Add xs x) = (tolist xs) ++ [x]

fromlist :: [a] -> Mylist a
fromlist [] = Emptylist
fromlist xs = Add (fromlist (init xs)) (last xs)


---3)elem

sum' :: (Num a) => [a] -> a
sum' (x:[]) = x
sum' (x:xs) = x + (sum' xs)

--1.
elem' :: (Eq a) => a-> [a] -> Bool
elem' t [] = False
elem' t (x:xs) = (t==x) || (elem' t xs) 

--2. 
elem'' :: (Eq a) => a-> [a] -> Bool
elem'' t [] = False
elem'' t xs = if (sum' [if t==x then 1 else 0 | x <- xs]) >= 1 then True else False

--3.
elem''' :: (Eq a) => a-> [a] -> Bool
elem''' t [] = False
elem''' t xs = any (==t) xs


---4) sublist

sublst :: [a] -> Int -> Int -> [a]
sublst [] i j = []
sublst xs i j 
       | i < 0 = []
       | j >= length(xs) = []
       | otherwise = drop i (take (j + 1) xs)


---5) rooted tree
sumbool :: [Bool] -> Bool
sumbool [] = False
sumbool (x:xs) = x || sumbool xs

data Tree t = Leaf {element:: t} | Node {element:: t, sons::[Tree t]} deriving (Show, Read, Eq) 

sumtree :: (Num t) => Tree t -> t
sumtree (Leaf t) = t
sumtree t = (element t) + sum(map sumtree $ sons t)

elemtree :: (Eq t) => t -> Tree t -> Bool
elemtree x (Leaf t) = x == t 
elemtree x (Node t sons) = x == t || sumbool(map (elemtree x) sons)






