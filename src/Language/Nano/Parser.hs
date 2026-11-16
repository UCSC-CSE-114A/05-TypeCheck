{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Language.Nano.Parser (parseString, parseFile) where

import Language.Nano.Types (Expr(..), Binop(..), Type(..))

import Control.Monad (void, MonadPlus)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Control.Exception (Exception, throw)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Void (Void)
import Text.Megaparsec (Parsec, VisualStream, TraversableStream, ShowErrorComponent, ParseErrorBundle, (<?>), empty, notFollowedBy, try, choice, sepBy, many, some, between, eof, runParser, parse, errorBundlePretty)
import Text.Megaparsec.Char (alphaNumChar, letterChar, spaceChar, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  (void spaceChar)
  (L.skipLineComment "#")
  empty --(L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

keyword :: Text -> Parser Text
keyword k = lexeme (string k <* notFollowedBy alphaNumChar)

rws :: [String]
rws = ["if", "then", "else", "let", "in", "True", "False", "Int", "Bool"]

var :: Parser String
var = lexeme . try $ p >>= check
  where
    p       = (:) <$> letterChar <*> many alphaNumChar <?> "variable"
    check x = if x `elem` rws
                then fail $ "reserved word " ++ show x ++ " cannot be an identifier"
                else return x

list' :: Parser Expr
list' = do _ <- symbol "["
           es <- sepBy expr (symbol ",")
           _ <- symbol "]"
           return $ foldr (EBin Cons) ENil es

if' :: Parser Expr
if' = do _ <- keyword "if"
         cond <- expr
         _ <- keyword "then"
         e1 <- expr
         _ <- keyword "else"
         e2 <- expr
         return $ EIf cond e1 e2

-- chainr1 is deprecated in megaparsec, so this replaces it
chainr1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainr1 p op = makeExprParser p [[InfixR op]]


tyAtom :: Parser Type
tyAtom = choice [ TInt  <$ keyword "Int"
                , TBool <$ keyword "Bool"
                , TIntList <$ (symbol "[" *> keyword "Int" *> symbol "]")
                , between (symbol "(") (symbol ")") typ ]

typ :: Parser Type
typ = chainr1 tyAtom (TFun <$ symbol "->")


param :: Parser (String, Type)
param = between (symbol "(") (symbol ")") ((,) <$> var <* symbol "::" <*> typ)

lam :: Parser Expr
lam = do _ <- symbol "\\"
         (v,t) <- param
         _ <- symbol "->"
         e <- expr
         return $ ELam v t e

let' :: Parser Expr
let' = do _   <- keyword "let"
          v   <- var
          ps  <- many param
          _   <- symbol "::"
          t   <- typ
          _   <- symbol "="
          e1  <- expr
          _   <- keyword "in"
          e2  <- expr
          let body = foldr (\(x, t) e -> ELam x t e) e1 ps
              tf   = foldr (\(_, t) r -> TFun t r) t ps
          return (ELet v tf body e2)


term :: Parser Expr
term = choice
  [
    EInt <$> integer,
    keyword "True" >> return (EBool True),
    keyword "False" >> return (EBool False),
    list',
    if',
    lam,
    let',
    EVar <$> var,
    between (symbol "(") (symbol ")") expr
  ]

binaryL, binaryR, binaryN :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryL name f = InfixL (f <$ symbol name)
binaryR name f = InfixR (f <$ symbol name)
binaryN name f = InfixN (f <$ symbol name)

expr :: Parser Expr
expr = makeExprParser term
  [
    [ binaryL "" EApp ], -- this works out to haskell-style space for function application
    [ binaryL "*" (EBin Mul) ],
    [
      binaryL "+" (EBin Plus),
      binaryL "-" (EBin Minus)
    ],
    [
      binaryL "||" (EBin Or),
      binaryL "&&" (EBin And)
    ],
    [ binaryR ":" (EBin Cons) ],
    [
      binaryN "==" (EBin Eq),
      binaryN "/=" (EBin Ne),
      binaryN "<=" (EBin Le),
      binaryN "<" (EBin Lt)
    ]
  ]

top' :: Parser Expr
top' = choice
  [
    var >> symbol "=" >> expr <* eof,
    expr <* eof
  ]

newtype PrettyError = PrettyError String
instance Show PrettyError where
  show (PrettyError str) = str
instance Exception PrettyError

handleError :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Either (ParseErrorBundle s e) Expr -> Expr
handleError (Left err) = throw . PrettyError $ errorBundlePretty err
handleError (Right ex) = ex

parseString :: String -> Expr
parseString input = handleError $ runParser top' "string" (pack input)

parseFile :: String -> IO Expr
parseFile filename = do contents <- TIO.readFile filename
                        return $ handleError (parse top' filename contents)

-- >>> parseFile "tests/input/3ctest10.no"
-- let f : Int = \(it : (Int) -> Int) -> \(x : Int) -> (((it) x) + 1) in 
--  let incr : (Int) -> Int = \(z : Int) -> (z + 1) in 
--  ((((f) incr)) 10)

