module ParserImpl where

import Defs

import Text.ParserCombinators.Parsec
import Data.Char

-- GRAMMAR (After transformation)

-- Type   ::= Type' Assign

-- Type'  ::= TVar
--         | '(' TParen ')'
--         | TCon Typez

-- Assign ::= epsilon
--         | "->" Type

-- TParen ::= Type TParen'
-- TParen'::= epsilon
--         | ',' Type

-- Typez  ::= epsilon
--         | Type' Typez


-- TDeclz ::= epsilon
--         | TDelc TDeclz
--         | ';' TDeclz

-- TDecl  ::= 'type' TDHead '=' Type
--         | 'newtype' TDHead '=' RCon '{' Field "::" Type '}'
--         | 'data' TDHead '=' RCon '{' FDeclz '}'

-- TDHead ::= TCon TVarz

-- TVarz  ::= epsilon
--         | TVar TVarz

-- FDeclz ::= epsilon
--         | FDecls

-- FDecls ::= FDecl FDecls'
-- FDecls'::= epsilon
--         | ',' FDecls

-- FDecl  ::= Fields "::" Type

-- Fields ::= Field Fields'
-- Fields'::= epsilon
--         | ',' Fields

-- TCon   ::= cName
-- TVar   ::= vName
-- RCon   ::= cName
-- Field  ::= vName

parseStringType :: String -> Either ErrMsg PType
parseStringType input =
  case parse (do whitespace; pt <- pType; return pt) "parse error" input of
    Left err -> Left $ show err
    Right ptype -> Right ptype

parseStringTDeclz :: String -> EM [TDecl]
parseStringTDeclz input = 
  case parse (do whitespace; pt <- tDeclz; return pt) "parse error" input of
    Left err -> Left $ show err
    Right tDeclz -> Right tDeclz

-- PType parsers
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

-- TDeclz parsers
tDeclz :: Parser [TDecl]
tDeclz = do eof; return []
    <|> do td <- tDecl; tds <- tDeclz; return (td:tds)
    <|> do symbol ";"; tds <- tDeclz; return tds;

tDecl :: Parser TDecl
tDecl = do symbol "type"; th <- tDHead; symbol "="; pt <- pType; return $ TDSyn th pt
    <|> do symbol "newtype"; th <- tDHead; symbol "="; rc <- rCon; symbol "{"; f <- field; symbol "::"; pt <- pType; symbol "}"; return $ TDRcd th rc [(f, pt)]
    <|> do symbol "data"; th <- tDHead; symbol "="; rc <- rCon; symbol "{"; fdz <- fDeclz; symbol "}"; return $ TDRcd th rc fdz

tDHead :: Parser TDHead
tDHead = do c <- tCon; vz <- tVarz; return (c, vz)

tVarz :: Parser [TVName]
tVarz = do v <- tVar; vz <- tVarz; return (v:vz)
    <|> return []

fDeclz :: Parser [(FName, PType)]
fDeclz = do fds <- fDecls; return fds
    <|> return []

fDecls :: Parser [(FName, PType)]
fDecls = do fd <- fDecl; fds <- fDecls' fd; return fds

fDecls' :: [(FName, PType)] -> Parser [(FName, PType)]
fDecls' fd = do symbol ","; fds <- fDecls; return $ fd ++ fds
    <|> return fd

fDecl :: Parser [(FName, PType)]
fDecl = do fs <- fields; symbol "::"; pt <- pType; return [(f, pt) | f <- fs]

fields :: Parser [FName]
fields = do f <- field; fs' <- fields' f; return fs'

fields' :: FName -> Parser [FName]
fields' f = do symbol ","; fs <- fields; return (f:fs)
    <|> return [f]

-- Terminals
tVar :: Parser TVName
tVar = vName

tCon :: Parser TCName
tCon = cName

rCon :: Parser RCName
rCon = cName

field :: Parser FName
field = vName

-- TODO: names with illegal characters not working
vName :: Parser TVName
vName = lexeme $ do try (do reserved; notFollowedBy isName); fail "reserved keyword"
    <|> do h <- satisfy isLower; t <- many isName; return $ [h] ++ t

-- cName starts with uppercase, hence no checking for keywords
cName :: Parser TCName
cName = lexeme $ do h <- satisfy isUpper; t <- many isName; return $ [h] ++ t

-- Lexeme
whitespace :: Parser ()
whitespace = do string "{-"; manyTill anyToken (string "-}"); return ()
    <|> do spaces; return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

-- Helper parsers
reserved :: Parser String
reserved = do string "type"
    <|> do string "newtype"
    <|> do string "data"

isName :: Parser Char
isName = satisfy (\c -> isAlphaNum c || c == '_' || c == '\'')

-- ATTEMPTED FIX TO ILLEGAL CHARACTERS
-- usage in vName/cName: manyTill isName endOfName

-- endOfName :: Parser ()
-- endOfName = try (do string "/"; return ())
-- endOfName = try (do string "->"; anyToken; return ())
--     <|> try (do char ')'; return ())
--     <|> try (do char ','; anyToken; return ())
--     <|> try (do spaces; return ())
--     <|> try (do eof; return ())