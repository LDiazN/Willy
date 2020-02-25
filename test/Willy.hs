-- Luis Diaz: 15-10420
-- Nathalia Silvera: 12-
-- Archivo Main Willy*

import qualified Lexer as L
import qualified Parser as P
import qualified Tokens as T
import qualified Expresions as E
import qualified ContextAnalyzer as CA
import qualified SymbolTable as ST
import qualified PrintTools as PT
import System.IO
import System.Environment
import System.Directory
import Control.Monad

main :: IO()
main = do
    
    --Intenta recibir input:
    inpt <- getArgs
    case inpt of     
        (a:_) -> processFile a
        []    -> do
                putStr "Nombre del archivo a compilar: "
                hFlush stdout
                a <- getLine
                processFile a


processFile :: FilePath -> IO()
processFile f = do
    -- Revisa que el archivo exista
    fileExists <- doesFileExist f

    -- Si existe, lo procesa
    if fileExists 
        then do 
        -- Contenido del archivo
        content <- readFile f
        let tks = L.tokenizer content  -- Resultado del lexer
            tks'= case tks of 
                    Right t -> t       -- Si todo salió bien, devuelve los tokes recibidos
                    Left  e -> error e -- Si algo salió mal, devuelve el error del lexer y termina el programa
            ast = P.parseClean $ L.cleanTokens tks' -- El AST asociado al programa willy
            

        -- Revisa que el resultado del lexer sea correcto:
        lexOk <- L.displayTokens tks'

        -- Si el análisis léxico salió bien, usa la función especial para esta entrega
        -- que imprime todo
        when lexOk $ PT.printAll ast
            

        else putStrLn "Willy Error: El archivo dado no existe"

    
    

