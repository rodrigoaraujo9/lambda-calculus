module Lexer where

import Parser (Token(..))
import Data.Char

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexWord (c:cs)
    | isDigit c = lexNum (c:cs)
    | c == '='  = TEq        : lexer cs
    | c == '+'  = TPlus      : lexer cs
    | c == '-'  = TMinus     : lexer cs
    | c == '*'  = TTimes     : lexer cs
    | c == '/'  = TDivide    : lexer cs
    | c == '('  = TLParen    : lexer cs
    | c == ')'  = TRParen    : lexer cs
    | c == '.'  = TDot       : lexer cs
    | c == '\\' = TBackslash : lexer cs
    | otherwise = error $ "*lexical error* found unexpected character " ++ [c]

lexNum :: String -> [Token]
lexNum cs = TInt (read num) : lexer rest
    where (num, rest) = span isDigit cs

lexWord :: String -> [Token]
lexWord cs = tok : lexer rest
    where
        (word, rest) = span isAlphaNum cs
        tok = case word of
            "let"    -> TLet
            "in"     -> TIn
            "if"     -> TIf
            "iszero" -> TIsZero
            "then"   -> TThen
            "else"   -> TElse
            "fix"    -> TFix
            "lambda" -> TLambda
            _        -> TVar word
