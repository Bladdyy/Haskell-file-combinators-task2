module Reducer where
import qualified Data.Map as Map
import Common


-- Performs one step of reduction.
rstep :: Maybe Expr -> Maybe Expr -> DefMap -> (Maybe Expr, Maybe Expr)
rstep pref Nothing _ = (pref, Nothing)


-- Cofaj po wyrażeniu, aż dojedziesz do końca i zapisuj ilość argumetnów;
rstep pref (Just (Var x)) mapping = 
    case Map.lookup x mapping of 
     Nothing -> case pref of
                 Just expr -> (Just (expr :$ Var x), Nothing)
                 Nothing -> (Just (Var x), Nothing)
     Just (Def _ pats expr) -> if null pats then (pref, Just expr)
                                  else case pref of
                                        Just ex -> (Just (ex :$ Var x), Nothing)
                                        Nothing -> (Just (Var x), Nothing)



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



-- Reduces and prints given expression until it is fully reduced or steps are depleted. 
iterateSteps :: Int -> Maybe Expr -> Maybe Expr -> DefMap -> IO ()
iterateSteps 0 _ _ _ = return ()  -- Stop after n steps
iterateSteps n pref rest mapping = do
    let (newPref, newX) = rstep pref rest mapping
    case newX of 
        Nothing -> return ()
        Just x -> do
                  print (buildExpr newPref x)
                  iterateSteps (n - 1) newPref newX mapping
    where
        -- Adds prefix before the rest of expression.
        buildExpr :: Maybe Expr -> Expr -> Expr
        buildExpr Nothing mx = mx
        buildExpr (Just (e1' :$ e2')) x = buildExpr (Just e1') (e2' :$ x)
        buildExpr (Just e1') x = e1' :$ x