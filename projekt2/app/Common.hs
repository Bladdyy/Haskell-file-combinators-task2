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
    showsPrec _ (Var name) = showString name
    showsPrec p (e1 :$ e2) =
        showParen (p > 10) $ -- Add parentheses if precedence is greater than 10
            showsPrec 9 e1 . showString " " . showsPrec 11 e2


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
