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

declare :: [TDecl] -> EM TCEnv
declare [] = Right []
