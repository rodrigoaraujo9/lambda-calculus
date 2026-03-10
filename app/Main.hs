module Main where
import Parser
import Lexer
import Eval
main :: IO ()
main = do
    input <- getContents
    let term = parse (lexer input)
    print term
    -- print (eval term [])
    print (eval_cps term [] id)
