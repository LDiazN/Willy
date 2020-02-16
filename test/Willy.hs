-- Luis Diaz: 15-10420
-- Nathalia Silvera: 12-
-- Archivo Main para la entrega 1:

import Lexer
import Tokens
import System.IO
import System.Environment
import System.Directory


main = do
    
    --Intenta recibir input:
    inpt <- getArgs

    case inpt of     
        (a:_) -> do
            fileExists <- doesFileExist a
            if fileExists then do
                content <- readFile a
                case tokenizer content of
                    Right tks -> putStr $ displayTokens tks
                    Left  s   -> putStr s
            else do
                putStrLn "Willy Error: El archivo dado no existe."
        ([])  -> do
            putStr "Nombre del archivo a compilar: "
            a <- getLine
            fileExists <- doesFileExist a
            if fileExists then do
                content <- readFile a
                case tokenizer content of
                    Right tks -> putStr $ displayTokens tks
                    Left  s   -> putStr s
            else do
                putStrLn "Willy Error: El archivo dado no existe."


        
    
    

