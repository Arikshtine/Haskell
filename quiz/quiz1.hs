--1.
data C = R | B deriving (Show, Eq)
data Mtree = Leaf C | Node [Mtree] deriving (Show, Eq)

--2.
t = Node [Node [Leaf R, Leaf B, Leaf B], Node [Leaf R], Node [Leaf R, Leaf R], Node [Leaf B]]

--3.
evelmt :: (Mtree) -> Maybe Char
evelmt (Leaf x) 
    | x == R = Just 'R'
    | x == B = Just 'B'
evelmt t = if m == 0 then Nothing else (if m > 0 then Just 'R' else Just 'B')
    where m = sumt(t)

sumt:: (Mtree) -> Int
sumt (Leaf x)
    |x == R = 1
    |x == B = -1
sumt (Node sons) = if elem 0 xs then 0 else (if k == 0 then 0 else (if k > 0 then 1 else -1))
    where xs =  (map sumt sons) 
          k = (sum xs)

