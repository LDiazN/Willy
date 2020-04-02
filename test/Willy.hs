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
import qualified Simulator as S
import qualified Interpreter as I
import qualified ProgramState as PS
import System.IO
import System.Environment
import System.Directory
import Control.Monad

main :: IO()
main = do
    
    --Intenta recibir input:
    inpt <- getArgs
    case inpt of     
        [f,t,o]  -> processFile f t o --file, task, options

        []       -> do
                    putStr "Nombre del archivo a compilar: "
                    hFlush stdout
                    a <- getLine
                    --processFile a
                    putStr "deprecated"

        _        -> error "Invalid args"

-- Recibe: 
-- Archivo a abrir -> Nombre de la task a procesar -> opciones
-- Donde opciones puede ser:
--      -m / --manual:  ejecución manual
--      -a / --auto: ejecución automática 
processFile :: FilePath -> String -> String -> IO()
processFile f t o = do
    -- Revisa que el archivo exista
    fileExists <- doesFileExist f

    -- Si existe, lo procesa
    if fileExists 
        then do 
        -- Contenido del archivo
        content <- readFile f
        let tks = L.tokenizer content  -- Resultado del lexer
            tks'= case tks of 
                    Right t -> t       -- Si todo salió bien, devuelve los tokens recibidos
                    Left  e -> error e -- Si algo salió mal, devuelve el error del lexer y termina el programa
            ast = P.parseClean $ L.cleanTokens tks' -- El AST asociado al programa willy
            
        -- Revisa que el resultado del lexer sea correcto:
        lexOk <- L.displayTokens tks'

        
        when lexOk $ do 
            (_, symt) <- CA.analyzeAST ast
            I.runProgram symt t S.printAll
            putStrLn "Testing"
            
            

        else putStrLn "Willy Error: El archivo dado no existe"

    
    

