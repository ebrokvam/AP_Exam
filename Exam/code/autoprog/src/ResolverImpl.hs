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

declare :: [TDecl] -> EM TCEnv
declare [] = Right []
declare ds = do
  decz <- doDeclarations ds
  return $ tce0 ++ decz

doDeclarations :: [TDecl] -> EM TCEnv
doDeclarations [] = Right []
doDeclarations (a: _as) =
  case a of
    TDSyn (_cn, _vs) pt -> do
      -- check vs has distinct variables
      -- check variables in pt are in vs
      case pt of
        PTVar _ -> Left "incomplete"
        PTApp _c _ -> Left "incomplete"
    TDRcd (_cn, _vs) _rc _fs -> Left "incomplete"