module Main where
import qualified Data.Map as Map

data Def = Def Name [Pat] Expr
data Expr = Var Name | Expr :$ Expr
type Pat = Name
type Name = String

type DefMap = Map.Map Name Def
type ExprMap = Map.Map Name Expr

instance Show Expr where
    show = ($ "") . subShow -- Skąd to ($ "")?
        where
        handleRight (e1' :$ e2') = showString "(" . subShow e1' . showString " "
                                 . handleRight e2' . showString ")"
        handleRight (Var name) = showString name
        subShow (e1 :$ e2) = subShow e1 . showString " " . handleRight e2
        subShow (Var name) = showString name


defToMap :: [Def] -> DefMap
defToMap lst = foldr change Map.empty lst
    where
    change (Def name list expr) mapping = 
        if Map.member name mapping 
            then error ("Many definitions of the same combinator named: " ++ name)
            else Map.insert name (Def name list expr) mapping


rstep :: Maybe Expr -> Maybe Expr -> DefMap -> (Maybe Expr, Maybe Expr)
rstep pref Nothing _ = (pref, Nothing)
rstep pref (Just (e1 :$ e2)) mapping = let (h, rest) = extractHead (e1 :$ e2) 
                                            in 
                                            case Map.lookup h mapping of
                                              Nothing -> case pref of
                                                           Nothing -> rstep (Just (Var h)) (rest) mapping
                                                           Just expr -> rstep (Just (expr :$ Var h)) (rest) mapping
                                              Just val -> checkstep pref val (e1 :$ e2) mapping
    where
    extractHead (Var x) = (x, Nothing)
    extractHead (e1' :$ e2') = let (h', rest') = extractHead e1'
                                in
                                case rest' of
                                  Nothing -> (h', Just e2')
                                  Just val' -> (h', Just (val' :$ e2'))



    subReduce :: Expr -> ExprMap -> Expr
    subReduce (a :$ b) small_map = subReduce a small_map :$ subReduce b small_map
    subReduce (Var name) small_map = case Map.lookup name small_map of
                                     Nothing -> Var name
                                     Just val -> val

    insert ex' (Var _) = ex'
    insert ex' (e1':$ e2') = insert ex' e1' :$ e2'

    reduce :: Maybe Expr -> Expr -> ExprMap -> Maybe Expr -> (Maybe Expr, Maybe Expr)                   
    reduce prefs expr' small_mapping rest = case rest of
                                            Nothing -> (prefs, Just (subReduce expr' small_mapping))
                                            Just expr -> (prefs, Just (insert (subReduce expr' small_mapping) expr))


    takeParams :: Int -> Expr -> [Expr] -> ([Expr], Maybe Expr, Bool)
    
    takeParams n (e1' :$ e2') lst = takeParams n e1' (e2' : lst) 
    takeParams n _ lst = if length lst >= n then (take n lst, revert (drop n lst) (Just (Var "start")), True)
                         else ([], revert lst Nothing, False)
        where
            revert (h:ts) rest = case rest of
                                    Nothing -> revert ts (Just h)
                                    Just expr -> revert ts (Just (expr :$ h))
            revert _ rest = rest

    checkstep :: Maybe Expr -> Def -> Expr -> DefMap -> (Maybe Expr, Maybe Expr)
    checkstep pref' (Def name pats expr) rest mapping' =
        let (params, new_rest, out) = takeParams (length pats) rest []
            in
            if out 
                then reduce pref' expr (Map.fromList (zip pats params)) new_rest
                else case pref of
                       Nothing -> rstep (Just (Var name)) new_rest mapping'
                       Just p  -> rstep (Just (p :$ (Var name))) new_rest mapping'




rstep pref (Just (Var x)) mapping = 
    case Map.lookup x mapping of 
     Nothing -> case pref of
                 Just expr -> (Just (expr :$ Var x), Nothing)
                 Nothing -> (Just (Var x), Nothing)
     Just (Def _ pats expr) -> if null pats then (pref, Just expr)
                                  else case pref of
                                        Just ex -> (Just (ex :$ Var x), Nothing)
                                        Nothing -> (Just (Var x), Nothing)

-- Dopisz case gdy jest samo Var x i potencjalnie (a:$b):$c

instance Show Def where
    show (Def name (x:xs) expr) =
        name ++ " " ++ unwords (x:xs) ++ " = " ++ show expr
    show (Def name _ expr) = 
        name ++ " = " ++ show expr

showDefMap :: DefMap -> String
showDefMap = unlines . map show . Map.elems

showExprMap :: ExprMap -> String
showExprMap = unlines . map (\(k, v) -> k ++ " = " ++ show v) . Map.toList

--
-- (ERROR) - Przykład poniżej: powinno być w nawiasie, błąd wynika stąd, że w takeParams na początku rest nie ma niczego, więc czytając nawias od prawej się wywala wszystko (basically dodaj coś na sam początek restu)
--

main :: IO ()
main = do
    -- let defs = [Def "s" ["x", "y", "z"] (Var "x" :$ Var "z" :$ (Var "y" :$ Var "z")), Def "k" ["x", "y"] (Var "x"), Def "i" [] (Var "s" :$ Var "k" :$ Var "k"), Def "om" ["x"] (Var "x" :$ Var "x"), Def "omega" [] (Var "om" :$ Var "om")]
    -- let smain = Var "s" :$ Var "k" :$ Var "k" :$ Var "X" :$ Var "Y" :$ Var "Y"

    -- let defs = [ Def "zero" ["f", "z"] (Var "z"), Def "one" ["f", "z"] (Var "f" :$ Var "z"), Def "two" [] (Var "suc" :$ Var "one"), Def "tre" [] (Var "suc" :$ Var "two"), Def "suc" ["n", "f", "z"] (Var "f" :$ (Var "n" :$ Var "f" :$ Var "z")), Def "o" ["f", "g", "x"] (Var "f" :$ (Var "g" :$ Var "x")), Def "add" ["m", "n", "f", "x"] (Var "m" :$ Var "f" :$ (Var "n" :$ Var "f" :$ Var "x")), Def "mul" ["m", "n"] (Var "o" :$ Var "m" :$ Var "n"), Def "fyr" [] (Var "add" :$ Var "two" :$ Var "two"), Def "six" [] (Var "mul" :$ Var "two" :$ Var "tre") ]
    -- let smain = Var "six" :$ Var "S" :$ Var "Z"

    -- let defs = [Def "s" ["x", "y", "z"] (Var "x" :$ Var "z" :$ (Var "y" :$ Var "z")), Def "k" ["x", "y"] (Var "x"), Def "i" [] (Var "s" :$ Var "k" :$ Var "k"), Def "om" ["x"] (Var "x" :$ Var "x"), Def "omega" [] (Var "om" :$ Var "om")]
    -- let smain = Var "k" :$ Var "i" :$ Var "omega" :$ Var "Z"
    
    print smain
    let mapping = defToMap defs
    putStrLn (showDefMap mapping)    -- print smain
    print "--------------------------------------------------"
    let (pref, x) = rstep Nothing (Just smain) mapping

    print pref
    case x of 
         Nothing -> print ""
         Just e-> print e
    print "--------------------------------------------------"

    let (pref2, x2) = rstep pref x mapping

    print pref2
    case x2 of 
         Nothing -> print ""
         Just e-> print e
    print "--------------------------------------------------"
    let (pref3, x3) = rstep pref2 x2 mapping

    print pref3
    case x3 of 
         Nothing -> print ""
         Just e-> print e
    print "--------------------------------------------------"
    let (pref4, x4) = rstep pref3 x3 mapping

    print pref4
    case x4 of 
         Nothing -> print ""
         Just e-> print e
    print "--------------------------------------------------"
    let (pref5, x5) = rstep pref4 x4 mapping

    print pref5
    case x5 of 
         Nothing -> print ""
         Just e-> print e
    
    print "--------------------------------------------------"
    let (pref6, x6) = rstep pref5 x5 mapping

    print pref6
    case x6 of 
         Nothing -> print ""
         Just e-> print e
    print "--------------------------------------------------"
    let (pref7, x7) = rstep pref6 x6 mapping

    print pref7
    case x7 of 
         Nothing -> print ""
         Just e-> print e
    print "--------------------------------------------------"
    let (pref8, x8) = rstep pref7 x7 mapping

    print pref8
    case x8 of 
         Nothing -> print ""
         Just e-> print e
    print "--------------------------------------------------"
    let (pref9, x9) = rstep pref8 x8 mapping

    print pref9
    case x9 of 
         Nothing -> print ""
         Just e-> print e
    print "--------------------------------------------------"
    let (pref10, x10) = rstep pref9 x9 mapping

    print pref10
    case x10 of 
         Nothing -> print ""
         Just e-> print e
