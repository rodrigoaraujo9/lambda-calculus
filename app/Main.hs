module Main where
import MyLib

main :: IO ()
main = do
    input <- getContents
    let term = parse (lexer input)
    print term
    print (eval term)
