import Data.List(group, length)
import Control.Monad(join)


--1. findNtConway
countNumbers :: String -> String
countNumbers n = join ([(show (length l)) ++ take 1(l) | l <- (group (n))])

findNtConway :: Int -> String
findNtConway n = if n == 1 then "1" else (countNumbers (findNtConway(n-1)))

--2. Mylist
--data Drlist = a 	


--3. elem: 

--elem' :: a -> [a] -> Bool 
--elem' a b = null [a == x | x <- b]
