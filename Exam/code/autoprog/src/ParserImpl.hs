-- Put your Parser implementation in this file
module ParserImpl where

import Defs
-- import either ReadP or Parsec, as relevant


-- REWRITTEN GRAMMAR
-- Type   := TVar Type'
--         | '(' TParen ')' Type'
--         | TCon Typez Type'

-- Type'  := epsilon
--         | "->" Type

-- TParen := Type TParen'
-- TParen':= epsilon
--         | ',' Type

-- Typez  := Type Typez'
-- Typez' := epsilon
--         | Typez


import Text.ParserCombinators.Parsec
import Data.Char

parseStringType :: String -> Either ErrMsg PType
parseStringType input = 
  case parse pType "parse error" input of
    Left err -> Left $ show err
    Right ptype -> Right ptype

parseStringTDeclz :: String -> EM [TDecl]
parseStringTDeclz = undefined

-- pType :: Parser PType
-- pType = do v <- tVar; t' <- pType' $ PTVar v; return t'
--     <|> do symbol "("; t <- pTypeParen; t' <- pType' t; return t'
--     <|> do c <- tCon; t <- pTypez; t' <- pType' $ PTApp c t; return t'

pType :: Parser PType
pType = do v <- tVar; t' <- pType' $ PTVar v; return t'
    <|> do t1 <- between (symbol "(") (symbol ")") pType; t2 <- pTypeParen t1; t' <- pType' t2; return t'
    <|> do c <- tCon; t <- pTypez; t' <- pType' $ PTApp c t; return t'

pType' :: PType -> Parser PType
pType' t1 = do eof; return t1
    <|> do symbol "->"; t2 <- pType; return $ PTApp "(->)" [t1, t2]

pTypeParen :: PType -> Parser PType
pTypeParen t1 = do symbol ","; t2 <- pType; return $ PTApp "(,)" [t1, t2]
    <|> return t1

-- pTypeParen :: Parser PType
-- pTypeParen = do t <- pType; t' <- pTypeParen' t; return t'

-- pTypeParen' :: PType -> Parser PType
-- pTypeParen' t1 = do symbol ","; t2 <- pType; symbol ")"; return $ PTApp "(,)" [t1, t2]
--     <|> do symbol ")"; return t1

pTypez :: Parser [PType]
pTypez = do eof; return []
    <|> do t <- pType; ts <- pTypez; return (t:ts)

tVar :: Parser TVName
tVar = lexeme $ do h <- satisfy isLower; t <- many isName; return $ [h] ++ t

tCon :: Parser TCName
tCon = lexeme $ do h <- satisfy isUpper; t <- many isName; return $ [h] ++ t

isName :: Parser Char
isName = (isKeyword >> fail "reserved keyword")
    <|> satisfy (\c -> isAlphaNum c || c == '_' || c == '\'')

-- TODO: THIS AINT WORKING
isKeyword :: Parser ()
isKeyword = do string "type"; return ()
    <|> do string "newtype"; return ()
    <|> do string "data"; return ()



lexeme :: Parser a -> Parser a
lexeme p = do a <- p; spaces; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

-- is this needed?
-- keyword :: String -> Parser ()
-- keyword s = lexeme $ do s' <- many1 (satisfy isAlphaNum)
--                       if s' == s then return ()
--                       else pfail $ "expected " ++ s