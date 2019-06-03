{-
Authors: Andrei Paskevich (2001 - 2008), Steffen Frerix (2017 - 2018)

Tokenization of input.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module SAD.Parser.Token
  ( Token (tokenPos, tokenText),
    tokenEndPos,
    tokensRange,
    showToken,
    properToken,
    tokenize,
    tokenReports,
    composeTokens,
    isEOF,
    noTokens,
    newTokenize,
    variable)
  where

import Data.Char (isSpace, isAscii, isAlphaNum)

import           Control.Monad (void)
import           Control.Monad.Combinators.Expr -- from parser-combinators
import           Data.Void
import qualified Text.Megaparsec as M
import           Text.Megaparsec (Parsec, (<|>))
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

import SAD.Core.SourcePos
import qualified SAD.Core.Message as Message
import qualified Isabelle.Markup as Markup


data Token =
  Token {
    tokenText :: String,
    tokenPos :: SourcePos,
    precedingWhiteSpace :: Bool,
    tokenProper :: Bool,
    isVar :: Bool,
    isSymbol :: Bool } |
  EOF {tokenPos :: SourcePos}


tokenEndPos :: Token -> SourcePos
tokenEndPos tok@Token{} = advancesPos (tokenPos tok) (tokenText tok)
tokenEndPos tok@EOF {} = tokenPos tok

tokensRange :: [Token] -> SourceRange
tokensRange toks =
  if null toks then noRange
  else makeRange (tokenPos $ head toks, tokenEndPos $ last toks)

showToken :: Token -> String
showToken t@Token{} = tokenText t
showToken EOF{} = "EOF"

properToken :: Token -> Bool
properToken Token {tokenProper} = tokenProper
properToken EOF {} = True

noTokens :: [Token]
noTokens = [EOF noPos]

tokenize :: SourcePos -> String -> [Token]
tokenize start = tokenizeWith start False

-- The boolean indicates if the token is whitespace or not.
tokenizeWith :: SourcePos -> Bool -> String -> [Token]
tokenizeWith pos ws s
  | not (null lexem) =
      tok : tokenizeWith (advancesPos pos lexem) False rest
  where
    (lexem, rest) = span isLexem s
    tok = Token lexem pos ws True False False
-- skip whitespace and tell the next token if there was preceding whitespace.
tokenizeWith pos _ws s
  | not (null white) = tokenizeWith (advancesPos pos white) True rest
  where (white, rest) = span isSpace s
tokenizeWith pos ws s@('%':_) =
  tok : tokenizeWith (advancesPos pos comment) ws rest
  where
    (comment, rest) = break (== '\n') s
    tok = Token comment pos False False False False
tokenizeWith pos ws (c:cs) =
  tok : tokenizeWith (advancePos pos c) False cs
  where
    tok = Token [c] pos ws True False False
tokenizeWith pos _ws [] = [EOF pos]

isLexem :: Char -> Bool
isLexem c =
  isAscii c && isAlphaNum c ||
  c `elem` ['α'..'ω'] ||
  c `elem` ['Α'..'Ω'] ||
  c `elem` ['ℵ'..'ℸ'] ||
  c == '_'

-- markup reports

tokenReports :: Token -> [Message.Report]
tokenReports Token {tokenPos = pos, tokenProper} =
  if tokenProper then []
  else [(pos, Markup.comment1)]
tokenReports _ = []


-- useful functions

composeTokens :: [Token] -> String
composeTokens [] = ""
composeTokens (t:ts) =
  let ws = if precedingWhiteSpace t then " " else ""
  in  ws ++ showToken t ++ composeTokens ts

isEOF :: Token -> Bool
isEOF EOF{} = True
isEOF _     = False




-- Show instances

instance Show Token where
  showsPrec :: Int -> Token -> ShowS
  showsPrec _ (Token s p _ _ _ _) = showString s . shows p
  showsPrec _ EOF{} = showString ""


-- BEGIN EXPERIMENTAL TOKENIZER

type Parser = Parsec Void String

data NewToken =
  Word String |
  Variable FontChoice String |
  Symbol String |
  EndOfFile

-- FontChoice uses similar abbreviations as latex.
-- \mathrm, \mathit, \mathbf, \mathbi
data FontChoice = RM | IT | BF | CAL | BB | FRAK


newTokenize = undefined

variable :: Parser NewToken
variable = lexeme (varRM <|> varIT)

varRM :: Parser NewToken
varRM = undefined

varIT :: Parser NewToken
varIT = do
  string "\\mathit{"
  var <- varName
  string "}"
  return (Variable IT var)

varName :: Parser String
varName = fmap (:[]) (letterChar <|> greekChar)

greekChar :: Parser Char
greekChar = oneOf ['α'..'ω'] <|> oneOf ['Α'..'Ω'] <|> greekAbbrev

greekAbbrev :: Parser Char
greekAbbrev = foldl1 (<|>) (texAbbrev `fmap` lookupGreek)
  where
    lookupGreek = [
      ("alpha",'α'),
      ("beta",'β'),
      ("gamma",'γ')]

texAbbrev :: (String, Char) -> Parser Char
texAbbrev (cmd, c) = do
  string ("\\" <> cmd <> "{")
  return c

spaceConsumer :: Parser ()
spaceConsumer = Lex.space space1 lineComment blockComment
  where
    lineComment  = Lex.skipLineComment "%"
    blockComment = Lex.skipBlockComment "\\begin{commend}" "\\end{comment}"

-- `lexeme` takes a parser `p` and returns a parser that parses the same input as `p`, plus any trailing whitespace, as defined by `spaceConsumer`.
lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceConsumer
