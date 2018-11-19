


---q2 .  mathematical expressions tree

data BinOp = Plus | Minus | Div | Mult | Sqrt | Pow  deriving (Show, Eq)
data UnOp = Sin | Cos deriving (Show,Eq)

data MathTree t = Empty | NodeBin {opBin::BinOp, lson::(MathTree t), rson::(MathTree t)} | NodeUn {opUn::UnOp, lson::(MathTree t)} | LeafConst t | LeafVar deriving (Show, Eq) 


evel :: (Floating t) => t -> (MathTree t) -> Maybe t
evel _ Empty = Just 0
evel x LeafVar = Just x
evel _ (LeafConst t) = Just t
evel x (NodeUn opUn lson)
    | opUn == Sin = sin <$> (evel x lson)
    | opUn == Cos = cos <$> (evel x lson)
evel x (NodeBin opBin lson rson)
    | opBin == Plus = (+) <$> (evel x lson) <*> (evel x rson)
    | opBin == Minus = (-) <$> (evel x lson) <*> (evel x rson)
    | opBin == Div = (/) <$> (evel x lson) <*> (evel x rson)
    | opBin == Mult = (*) <$> (evel x lson) <*> (evel x rson)
--    | opBin == Sqrt = (**0.5) <$> (evel x lson) <*> (evel x rson)
    | opBin == Pow = (**) <$> (evel x lson) <*> (evel x rson)


 
     









