import Lexer
import Parser
import System.IO
import Control.Monad
import System.Environment
import Tokens
import Expresions
import SymbolTable
import ContextAnalyzer
import qualified Data.Map as M
main = do
    (filename:_) <- getArgs

    content <- readFile filename  

    case tokenizer content of
        Right tks ->

            void (analyzeAST . parseClean . cleanTokens $ tks) 
        Left s ->
            putStrLn s


    
