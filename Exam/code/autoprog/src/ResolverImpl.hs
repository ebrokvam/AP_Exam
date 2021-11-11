-- Put your Resolver implementation in this file
module ResolverImpl where

import Defs

resolve :: TCEnv -> (TVName -> EM SType) -> PType -> EM SType
resolve tce tve pt = 
  case pt of
    PTVar x -> tve x
    PTApp c x -> 
      case lookup c tce of
        Nothing -> Left $ show c ++ " is not defined in type-constructor environment"
        Just f -> do
          st <- mapM (resolve tce tve) x
          f st

-- T a = a -> a
-- PTApp "(->)"" [PTVar "a", PTVar "a"]
-- STProd (STVar "a") (STVar "a")

-- T a = X a
-- PTApp "X" [PTVar "a"]

-- T a = a
-- PTVar "a"
-- STVar "a"

-- STProd


declare :: [TDecl] -> EM TCEnv
declare [] = Right []
declare ds = do
  decz <- doDeclarations ds
  return $ tce0 ++ decz

doDeclarations :: [TDecl] -> EM TCEnv
doDeclarations (a:as) = undefined
  -- case a of
  --   TDSyn (cn, vs) pt -> do
  --     -- check vs has distinct variables
  --     -- then variables in pt are on lhs
  --     case pt of
  --       PTVar _ ->
  --       PTApp c _ ->
    -- TDRcd (cn, vs) rc fs -> do
    --   tce <- declare as
    --   return $ Right ((cn, \ts -> 
    --                         case ts of 
    --                           res -> return $ STRcd rc res
    --                           _ -> Left $ "bad args for " ++ show cn))

-- T a b = a

tesst :: [SType] -> EM SType
tesst ts = 
  case ts of
    [t] -> return $ STProd t t
    _ -> Left $ "bad args for ick"

test = do 
  tce <- declare [TDSyn ("T", ["a"]) (PTApp "(->)" [PTVar "a", PTVar "a"])] -- T a = a -> a
  tf <- case lookup "T" tce0 of Just tf -> return tf; _ -> Left "no T"
  tf [STVar "a'"]
-- (STArrow (STVar "a'") (STVar "a'"))

-- OUTPUT OF DECLARE 
-- ("T", \ts -> case ts of [t1,t2] -> return $ STArrow t1 t2
--                              _ -> Left "bad args for (->)")]
-- THEN APPLYING FUNCTION
-- Right $ STArrow (STVar "a'") (STVar "a'")


test6 = do
  tce <- declare [TDSyn ("T", ["a"]) (PTVar "a")]
  return $ lookup "T" tce

test1 = do 
  tce <- declare [TDSyn ("T", ["a"]) (PTVar "a")] -- T a = a
  tf <- case lookup "T" tce of Just tf -> return tf; _ -> Left "no T"
  tf [STVar "a'"]

-- OUTPUT OF DECLARE 
-- ("T", \ts -> case ts of [t1] -> return $ STVar t1
--                              _ -> Left "bad args for (->)")]
-- THEN APPLYING FUNCTION
-- Right $ STVar "a'"

test2 = do 
  tce <- declare [TDSyn ("T", ["a"]) (PTApp "(->)" [PTVar "a", PTVar "a"])]
  st <- resolve tce (\x -> return $ STVar x) (PTApp "T" [PTApp "(,)" [PTVar "b", PTVar "b"]])
  return st

test3 = do 
  tce <- declare [TDRcd ("T", ["a"]) "C" [("x", PTVar "a"), ("f", PTApp "(->)" [PTVar "a", PTVar "a"])]] -- T a = a -> a
  tf <- case lookup "T" tce of Just tf -> return tf; _ -> Left "no T"
  tf [STVar "a'"]

test4 = do 
  tce <- declare [TDRcd ("T", ["a"]) "C" [("x", PTVar "a"), ("f", PTApp "(->)" [PTVar "a", PTVar "a"])]]
  st <- resolve tce (\x -> return $ STVar x) (PTApp "T" [PTApp "(,)" [PTVar "b", PTVar "b"]])
  return st

-- [("T", \ts -> 
--   case ts of
--     [t] -> return $ STRcd "C" [("x", t), ("f", STArrow t t)]
--     _ -> Left "bad args for T")]