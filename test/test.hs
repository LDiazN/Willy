import Lexer
import System.IO
import System.Environment
import Tokens

main = do
    (filename:_) <- getArgs

    content <- readFile filename  
    case tokenizer content of
        Right tks ->
            putStrLn $ displayTokens tks
        Left s ->
            putStrLn s