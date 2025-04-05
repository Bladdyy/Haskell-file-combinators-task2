module Reducer where
import qualified Data.Map as Map
import Common


-- Performs one step of reduction.
rstep :: Maybe Expr -> Maybe Expr -> DefMap -> (Maybe Expr, Maybe Expr)
rstep pref Nothing _ = (pref, Nothing)
rstep pref (Just expr) mapping = let (new_pref, rest, _, _, _) = findReduce pref expr 0
                                   in
                                   (new_pref, rest)
    where
    -- Finds first reducable part, reduces and passes it. Adds non-reducable part to prefix.
    findReduce :: Maybe Expr -> Expr -> Int -> (Maybe Expr, Maybe Expr, Def, Int, [Expr])
    findReduce pref' (e1 :$ e2) n = 
        let (new_pref, rest, Def nam par ex, to_param, lst) = findReduce pref' e1 (n + 1)
          in
          case to_param of
            -- Looking for reducable part.
            (-1) -> case e2 of
                      (e1' :$ e2') -> findReduce new_pref (e1' :$ e2') n
                      _ -> findReduce new_pref e2 n
            -- Reducable part found and reduced already.
            (-2) -> case rest of
                      Nothing -> (new_pref, Just e2, Def nam par ex, to_param, lst)
                      Just expr' -> (new_pref, Just (expr' :$ e2), Def nam par ex, to_param, lst)
            -- Last argument for reduction. 
            1 -> (pref', Just (reduce ex par (reverse (e2:lst))), Def nam par ex, -2, [])
            -- Middle argument for reduction.
            _ -> (new_pref, rest, Def nam par ex, to_param - 1, e2 : lst)

    findReduce pref' (Var name) n = 
        case Map.lookup name mapping of
          Nothing -> case pref' of
               Nothing -> (Just (Var name), Nothing, Def "a" [] (Var "a"), -1, [])
               Just expr' -> (Just (expr' :$ Var name), Nothing, Def "a" [] (Var "a"), -1, [])
          
          Just (Def _ pats expr') -> 
              -- No arguments needed.
              if length pats == 0 
               then (pref', Just (reduce expr' pats []), (Def name pats expr'), -2, [])
              -- Enough arguments to perform reduction.
              else if length pats <= n 
               then (pref', Nothing, (Def name pats expr'), length pats, [])
              -- Not enough arguments to perform reduction.
              else case pref' of
               Nothing -> (Just (Var name), Nothing, Def "a" [] (Var "a"), -1, [])
               Just expr2 -> (Just (expr2 :$ Var name), Nothing, Def "a" [] (Var "a"), -1, [])


    -- Creates submap for reduced expression.
    reduce :: Expr -> [String] -> [Expr] -> Expr                   
    reduce expr' def_pats params = subReduce expr' (Map.fromList (zip def_pats params))


    -- Performes reduction.
    subReduce :: Expr -> ExprMap -> Expr
    subReduce (a :$ b) small_map = subReduce a small_map :$ subReduce b small_map
    subReduce (Var name) small_map = case Map.lookup name small_map of
                                     Nothing -> Var name
                                     Just val -> val



-- Reduces and prints given expression until it is fully reduced or steps are depleted. 
iterateSteps :: Int -> Maybe Expr -> Maybe Expr -> DefMap -> IO ()
iterateSteps 0 _ _ _ = return ()  -- Stop after n steps
iterateSteps n pref rest mapping = do
    let (new_pref, new_x) = rstep pref rest mapping
    case new_x of 
        Nothing -> return ()
        Just x -> do
                  print (buildExpr new_pref x)
                  iterateSteps (n - 1) new_pref new_x mapping
    where
        -- Adds prefix before the rest of expression.
        buildExpr :: Maybe Expr -> Expr -> Expr
        buildExpr Nothing mx = mx
        buildExpr (Just (e1' :$ e2')) x = buildExpr (Just e1') (e2' :$ x)
        buildExpr (Just e1') x = e1' :$ x
