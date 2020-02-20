import Lexer
import Parser
import System.IO
import System.Environment
import Tokens
import Expresions


main = do
    (filename:_) <- getArgs

    content <- readFile filename  

    case tokenizer content of
        Right tks ->
            print $ parseClean $ cleanTokens tks
        Left s ->
            putStrLn s


parseClean :: [TokPos] -> [ProgPart]
parseClean = reverse . parse
    