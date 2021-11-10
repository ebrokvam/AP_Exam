-- Put your Parser implementation in this file
module ParserImpl where

import Defs
-- import either ReadP or Parsec, as relevant


-- REWRITTEN GRAMMAR
-- Type   := Type' Assign

-- Type'  := TVar
--         | '(' TParen ')'
--         | TCon Typez

-- Assign := epsilon
--         | "->" Type

-- TParen := Type TParen'
-- TParen':= epsilon
--         | ',' Type

-- Typez  := epsilon
--         | Type' Typez



-- OLD ATTEMP
-- Type   := TVar Type'
--         | '(' TParen ')' Type'
--         | TCon Typez Type'

-- Type'  := epsilon
--         | "->" Type

-- TParen := Type TParen'
-- TParen':= epsilon
--         | ',' Type

-- Typez := epsilon
--         | Type Typez


import Text.ParserCombinators.Parsec
import Data.Char

parseStringType :: String -> Either ErrMsg PType
parseStringType input =
  case parse (do whitespace; pt <- pType; return pt) "parse error" input of
    Left err -> Left $ show err
    Right ptype -> Right ptype

parseStringTDeclz :: String -> EM [TDecl]
parseStringTDeclz = undefined

-- pType :: Parser PType
-- pType = do v <- tVar; t' <- pType' $ PTVar v; return t'
--     <|> do t1 <- between (symbol "(") (symbol ")") pTypeParen; t' <- pType' t1; return t'
--     <|> do c <- tCon; t <- pTypez; t' <- pType' $ PTApp c t; return t'

-- pType' :: PType -> Parser PType
-- pType' t1 = do symbol "->"; t2 <- pType; return $ PTApp "(->)" [t1, t2]
--     <|> return t1

-- pTypeParen :: Parser PType
-- pTypeParen = do t <- pType; t' <- pTypeParen' t; return t'

-- pTypeParen' :: PType -> Parser PType
-- pTypeParen' t1 = do symbol ","; t2 <- pType; return $ PTApp "(,)" [t1, t2]
--     <|> return t1

-- pTypez :: Parser [PType]
-- pTypez = do t <- pType; ts <- pTypez; return (t:ts)
--     <|> return []

pType :: Parser PType
pType = do t <- pType'; t' <- assign t; return t'

pType' :: Parser PType
pType' = do v <- tVar; return $ PTVar v
    <|> do t <- between (symbol "(") (symbol ")") pTypeParen; return t
    <|> do c <- tCon; t <- pTypez; return $ PTApp c t;

assign :: PType -> Parser PType
assign t1 = do symbol "->"; t2 <- pType; return $ PTApp "(->)" [t1, t2]
    <|> return t1

pTypeParen :: Parser PType
pTypeParen = do t <- pType; t' <- pTypeParen' t; return t'

pTypeParen' :: PType -> Parser PType
pTypeParen' t1 = do symbol ","; t2 <- pType; return $ PTApp "(,)" [t1, t2]
    <|> return t1

pTypez :: Parser [PType]
pTypez = do t <- pType'; ts <- pTypez; return (t:ts)
    <|> return []

-- come back to this
-- TODO: name with illegal characters
tVar :: Parser TVName
tVar = lexeme $ do try (do reserved; notFollowedBy isName); fail "reserved keyword"
    <|> do h <- satisfy isLower; t <- many isName; return $ [h] ++ t

tCon :: Parser TCName
tCon = lexeme $ do h <- satisfy isUpper; t <- many isName; return $ [h] ++ t

-- endOfName :: Parser ()
-- endOfName = try (do string "/"; return ())
-- endOfName = try (do string "->"; anyToken; return ())
--     <|> try (do char ')'; return ())
--     <|> try (do char ','; anyToken; return ())
--     <|> try (do spaces; return ())
--     <|> try (do eof; return ())

reserved :: Parser String
reserved = do string "type"
    <|> do string "newtype"
    <|> do string "data"

isName :: Parser Char
isName = satisfy (\c -> isAlphaNum c || c == '_' || c == '\'')

whitespace :: Parser ()
whitespace = do string "{-"; manyTill anyToken (string "-}"); return ()
    <|> do spaces; return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

-- lexeme :: Parser a -> Parser a
-- lexeme p = do a <- p; spaces; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()