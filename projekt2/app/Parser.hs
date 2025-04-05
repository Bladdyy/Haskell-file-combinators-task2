module Parser where
import Language.Haskell.Parser
import Language.Haskell.Syntax
import qualified Data.Set as Set
import Common

-- Reads file, parses it and returns Prog made of given file.
fromHsString :: String -> IO (Prog)
fromHsString path = do
                  content <- readFile path
                  return (Prog (fromParseResult content))
    where
        -- Parses content of file and returns list of Def in it.
        fromParseResult :: String -> [Def]
        fromParseResult content' = case parseModule content' of
                                    ParseOk ins -> fromHsModule ins
                                    ParseFailed _ err -> error ("ERROR: Parse error: " ++ err)
        

        -- Extracts list of Def from HsModule.
        fromHsModule :: HsModule -> [Def]
        fromHsModule (HsModule _ _ _ _ funs) = extractFun funs


        -- Changes every HsDecl into coresponding Def.
        extractFun :: [HsDecl] -> [Def]
        extractFun (HsFunBind mat:xs) = let ans = extractFun xs
                                            in 
                                            extractMatch (head mat) : ans
        extractFun ((HsPatBind _ (HsPVar name) out _ ):xs) = let ans = extractFun xs
                                                               in 
                                                               process name Nothing out : ans
        extractFun _ = []


        -- Extracts Def from single HsMatch.
        extractMatch :: HsMatch -> Def
        extractMatch (HsMatch _ name params out _) = process name (Just params) out


        -- Validates and combines parts of Def.
        process :: HsName -> Maybe [HsPat]-> HsRhs -> Def
        process (HsIdent name') (Just params') (HsUnGuardedRhs out') = 
                                            let (pars, _) = validPar params' name'
                                              in
                                              case name' of
                                              "main" -> error "ERROR: Main can't have parameters."
                                              _ -> Def name' pars (extractRight out')
        process (HsIdent name') Nothing (HsUnGuardedRhs out') = Def name' [] (extractRight out')
        process _ _ _ = error "INTERNAL ERROR: Not expected pattern in process."                                            


        -- Validates and parses parameters. 
        validPar :: [HsPat] -> String -> ([String], Set.Set String)
        validPar ((HsPVar (HsIdent par)):xs) name = 
          let (lst, set) = validPar xs name
            in
            case Set.member par set of
              True -> error ("ERROR: Combinator: " ++ name ++ 
                                      " has multiple parameters with the same name: " ++ par ++ ".")
              False -> (par:lst, Set.insert par set)
        validPar _ _ = ([], Set.empty)
        

        -- Extracts expression from HsExp.
        extractRight :: HsExp -> Expr
        extractRight (HsApp left right) = extractRight left :$ extractRight right
        extractRight (HsVar (UnQual (HsIdent name))) = Var name
        extractRight (HsCon (UnQual (HsIdent name))) = Var name
        extractRight (HsParen var) = extractRight var
        extractRight _ = error "INTERNAL ERROR: Not expected pattern in extractRight."
                                                

