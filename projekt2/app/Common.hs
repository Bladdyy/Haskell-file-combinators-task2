module Common where
import qualified Data.Map as Map

data Def = Def Name [Pat] Expr
data Expr = Var Name | Expr :$ Expr
type Pat = Name
type Name = String

newtype Prog = Prog {progDefs :: [Def]}

type DefMap = Map.Map Name Def
type ExprMap = Map.Map Name Expr

steps :: Int
steps = 30

instance Show Expr where
    show = ($ "") . subShow
        where
        handleRight (e1' :$ e2') = showString "(" . subShow e1' . showString " "
                                 . handleRight e2' . showString ")"
        handleRight (Var name) = showString name
        subShow (e1 :$ e2) = subShow e1 . showString " " . handleRight e2
        subShow (Var name) = showString name


instance Show Def where
    show (Def name (x:xs) expr) =
        name ++ " " ++ unwords (x:xs) ++ " = " ++ show expr
    show (Def name _ expr) = 
        name ++ " = " ++ show expr


-- Inputs Def list into Map checking for duplicates.
defToMap :: [Def] -> DefMap
defToMap lst = foldr change Map.empty lst
    where
    change (Def name list expr) mapping = 
        if Map.member name mapping 
            then error ("Many definitions of the same combinator named: " ++ name)
            else Map.insert name (Def name list expr) mapping