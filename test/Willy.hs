-- Luis Diaz: 15-10420
-- Nathalia Silvera: 12-10921
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


--Para usar el interpretador de willy:
-- ./Willy [nombre del archivo] [nombre de task] [options]
-- donde los tres son obligatorios y options puede tener los siguientes 
-- valores: 
-- -m/--manual: Realiza la ejecución paso a paso
-- -a/--auto [segundos]*: realiza la ejecución cada [segundos]. Si no se especifica
--                        un valor para [segundos], entonces es ejecución instantánea

main :: IO()
main = do
    
    --Intenta recibir input:
    inpt <- getArgs
    print inpt
    case inpt of     
        [f,t,o]   -> let cbf = case o of --cbf: callback function
                                "-a"        -> S.printAll
                                "--auto"    -> S.printAll
                                "-m"        -> S.printNContinue
                                "--manual"  -> S.printNContinue
                                a           -> error $ "opción inválida: " ++ show a

                     in if o /= "-m" && o /= "-a" && o /= "--manual" && o /= "--auto"
                            then putStrLn $ "Error: opciones inválidas. Opciones válidas: \n" ++
                                            "  -a/--auto [num]\n" ++ 
                                            "  -m/--manual"
                            else processFile f t cbf



        [f,t,o,n] ->  if o/= "-a" && o/="--auto"
                        then putStrLn "Error: demasiados argumentos"
                        else  processFile f t $ S.printNWait ((read . init . init . show $ (read n::Float)*1000000)::Int)

        _         -> putStrLn $ "Error: formato de entrada incorrecto. Para correr un programa de Willy" ++
                                " utilizar: \n" ++
                                "  ./Willy [nombre de archivo] [nombre de task] [opciones]"

-- Recibe: 
-- Archivo a abrir -> Nombre de la task a procesar -> opciones
-- Donde opciones puede ser:
--      -m / --manual:  ejecución manual
--      -a / --auto: ejecución automática 
processFile :: FilePath -> String -> (PS.ProgramState -> IO()) -> IO()
processFile f t cbf = do
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
            when (null . ST.errors $ symt ) $
                void $ I.runProgram symt t cbf
                
            
            

        else putStrLn "Willy Error: El archivo dado no existe"

    
    

