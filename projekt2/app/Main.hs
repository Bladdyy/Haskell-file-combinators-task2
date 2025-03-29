module Main where
import qualified Data.Map as Map

data Def = Def Name [Pat] Expr
data Expr = Var Name | Expr :$ Expr
type Pat = Name
type Name = String

type DefMap = Map.Map Name Def
type ExprMap = Map.Map Name Expr


defToMap :: [Def] -> DefMap
defToMap lst = foldr change Map.empty lst
    where
    change (Def name list expr) mapping = 
        if Map.member name mapping 
            then error ("Many definitions of the same combinator named: " ++ name)
            else Map.insert name (Def name list expr) mapping


rstep :: Expr -> Expr -> DefMap -> (Expr, Expr)
rstep pref ((Var x):$xs) mapping =  case Map.lookup x mapping of
                                    Nothing -> rstep (pref :$ Var x) xs mapping
                                    Just val -> checkstep pref val xs mapping
    where


    subReduce :: Expr -> ExprMap -> Expr
    subReduce (a :$ (b :$ c)) small_map = subReduce a small_map :$ (subReduce (b :$ c) small_map)
    subReduce (a :$ b) small_map = subReduce a small_map :$ subReduce b small_map
    subReduce (Var name) small_map = case Map.lookup name small_map of
                                     Nothing -> Var name
                                     Just val -> val

    

    reduce :: Expr -> Expr -> ExprMap -> Expr -> (Expr, Expr)                   
    reduce prefs expr' small_mapping rest = (prefs, subReduce expr' small_mapping :$ rest)


    takeParams :: Int -> [Expr] -> Expr -> ([Expr], Expr, Bool)
    takeParams 0 params rest = (reverse params, rest, True)
    takeParams n params (x':$xs') = takeParams (n - 1) (x':params) xs'
    takeParams _ params rest = (params, rest, False)


    checkstep :: Expr -> Def -> Expr -> DefMap -> (Expr, Expr)
    checkstep pref' (Def name pats expr) rest mapping' = 
        let (params, new_rest, out) = takeParams (length pats) [] rest
            in
            if out 
                then reduce pref' expr (Map.fromList (zip pats params)) new_rest
                else rstep (pref' :$ (Var name)) rest mapping'
rstep pref expr _ = (pref, expr)

-- Dopisz case gdy jest samo Var x i potencjalnie (a:$b):$c

main :: IO ()
main = putStrLn "Hello, 222Haskell!"
    