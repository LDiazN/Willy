import Lexer
import Parser
import System.IO
import System.Environment
import Tokens

main = do
    (filename:_) <- getArgs

    content <- readFile filename  
    case tokenizer content of
        Right tks ->
            print $ parse $ cleanTokens tks
        Left s ->
            putStrLn s