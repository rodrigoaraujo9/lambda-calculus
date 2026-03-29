module Main where

import System.Environment (getArgs)

import Parser
import Lexer

import Compiler

import Run

main :: IO ()
main = do
    [file] <- getArgs
    if reverse (take 4 (reverse file)) /= ".fun"
        then error "give a .fun file"
        else do
            input <- readFile file
            let term = parse (lexer input)
            let code = compile term []
            let result = run code
            print term
            print code
            print result
