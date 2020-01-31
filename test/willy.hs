-- Luis Diaz: 15-10420
-- Nathalia Silvera: 12-
-- Archivo Main para la entrega 1:
import Lexer
import Tokens
import System.IO
import System.Environment
import System.Directory


--  Este es el archivo main para la primera entrega, todas las funciones de impresiòn 
--  y procesamiento de tokens estàn en Lexer.x, las definiciones de los token estàn en Tokens.hs

main = do
    --Intenta recibir input:
    inpt <- getArgs

    case inpt of     
        (a:_) -> do -- En caso de que hayas recibido al menos un argumento:
            fileExists <- doesFileExist a   -- Coonsulta si el archivo existe
            if fileExists then do           -- Si existe, compila
                content <- readFile a
                case tokenizer content of
                    Right tks -> putStr $ displayTokens tks
                    Left  s   -> putStr s
            else do                         -- Si no existe, imprime un error
                putStrLn "Willy Error: El archivo dado no existe."
        ([])  -> do -- En caso de que  no hayan pasado ningùn argumento, pregunta por el archivo
            putStrLn "Nombre del archivo a compilar: "
            a <- getLine
            fileExists <- doesFileExist a
            if fileExists then do
                content <- readFile a
                case tokenizer content of
                    Right tks -> putStr $ displayTokens tks
                    Left  s   -> putStr s
            else do
                putStrLn "Willy Error: El archivo dado no existe."


        
    
    

