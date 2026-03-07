module Main where
import Eval
import Parser
import Lexer

main :: IO ()
main = do
    input <- getContents
    let term = parse (lexer input)
    print term
    print (eval term)
