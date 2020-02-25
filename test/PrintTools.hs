module PrintTools where

import qualified Expresions as E
import qualified Tokens as T
import qualified SymbolTable as ST
import qualified ContextAnalyzer as CA
import qualified Data.Map as M
import Parser
import Lexer
import Control.Monad.State
import Control.Monad
import System.IO
import System.Environment

type CurrentContext =  Int

printAll :: E.AST -> IO()
printAll ast = do 
    
    -- Analiza el contexto y la sintaxis del programa
    (_, st@ST.SymbolTable{ST.errors = e}) <- CA.analyzeAST ast

    when (null e) $ do
        putStrLn "[PROGRAMA WILLY*]"
        runStateT (printProg ast) 0
        putStrLn "[TABLA DE SÍMBOLOS]"
        runStateT (printSymTable st) 0
        return ()
        

-- Esta funciòn recibe un AST e imprime un programa asociado formateado 
-- segun las especificaciones dadas
printProg :: E.AST -> StateT CurrentContext IO ()
printProg ast = do
    io $ putStrLn "TAREAS:"

    processAST ast
    
    return ()
    where
        processAST :: E.AST -> StateT CurrentContext IO ()
        processAST = foldr ((>>) . processProgPart) (return())
        

        -- Procesa un bloque de programa. 
        processProgPart :: E.ProgPart -> StateT CurrentContext IO ()
        processProgPart (E.World _ _) = increaseState

        processProgPart (E.Task tn ww inst) = do
            increaseState
            block <- get
            io $ putStrLn $ T.getId' tn ++ " :"            -- Imprime: "nombre_de_task:"
            io $ putStrLn $ "  mundo: " ++ T.getId' ww     -- Imprime: "  mundo: nombre_mundo"
            io $ putStrLn   "  bloque de instrucciones:"-- Imprime: "  bloque de instrucciones"
            io $ putStrLn $ "    Identificador del bloque: " ++ show block 
            -- Imprime todas las instrucciones
            processInstructions 2 inst

-- Imprime la tabla de símbolos pasada como argumento
printSymTable :: CA.ContextState -> StateT CurrentContext IO ()
printSymTable st@ST.SymbolTable{ST.symbolMap = m} = do
    -- Una lista con todos los símbolos de la tabla
    let syms = concat . M.elems $ m

    mapM_ printSym $ reverse syms

    where 
        printType :: ST.SymType -> String
        -- Imprime booleano
        printType ST.BoolVar{} = "Tipo: Booleano\n"

        -- Imprime objeto
        printType ST.ObjType{} = "Tipo: Nombre de objeto\n"

        -- Imprime goal
        printType ST.Goal{}    = "Tipo: Goal\n"

        --Imprime tarea
        printType ST.Task{ST.tBlockId = bid, ST.exprs = E.Task{E.workingWorld = ww}} =
            "Tipo: Tarea\n" ++
            "Identificador del bloque: " ++ show bid ++ "\n" ++
            "Mundo asociado: " ++ T.getId' ww ++ "\n"

        --Imprime mundo
        printType ST.World{ST.wBlockId = bid, ST.walls = w, ST.worldSize = ws} =
            let
                --Pide las filas y columnas del mundo
                E.WorldSize{ E.rows = r, E.cols = c} = head ws 
                --String con el tamaño del mundo formateado
                size =  show (T.getInt' r) ++ " " ++ show (T.getInt' c) --
                --String con todos los muros o null
                swalls = if null w 
                            then "null"
                            else '\n' : concatMap printWall (reverse w)

                -- Función auxiliar que convierte un muro en un string formateado como Germán
                printWall :: E.WorldStmnt -> String
                printWall E.Wall{E.direction = (tkdir,_,_), E.from = (tkfx, tkfy), E.to = (tktx, tkty)} =
                        "- " ++ show tkdir ++ ", from: " ++ show (T.tok tkfx) ++ " " ++ show (T.tok tkfy) ++
                                              ", to: "   ++ show (T.tok tktx) ++ " " ++ show (T.tok tkty) ++ "\n" 

            in 
                "Tipo: Mundo\n" ++
                "Identificador del bloque: " ++ show bid ++ "\n" ++
                "Tamaño: " ++ size ++ "\n" ++
                "Muros: " ++ identString 1 swalls
        -- Imprime una definición de función
        printType ST.DefineFunc{ST.body = b, ST.fBlockId = bid} = 
            "Tipo: Instrucción\n" ++
            "Identificador del bloque: " ++ show bid ++ "\n" ++
            "AST Asocioado: "
        
        -- Imprime un símbolo dado
        printSym :: ST.Symbol -> StateT CurrentContext IO()
        printSym s@ST.Symbol{ST.symId = sid, ST.symType = stype, ST.symContext = scont} = do
            io $ putStrLn $ sid ++ ":"
            io $ putStrLn $ "  Bloque de declaración: " ++ show scont
            io $ putStr $ identString 1 (printType stype)

            when  (ST.isDef stype) $ do
                let inst  = ST.body stype
                    isDef = case inst of 
                                E.DefineFunc{} -> True
                                _              -> False
                if isDef 
                    then io $ putStr $ identString 2 "null"
                    else put 0 >> processInstruction 2 inst
                
                
                
processInstructions :: Int -> [E.TaskStmnt] -> StateT CurrentContext IO ()
processInstructions _ [] = return ()
processInstructions il (i:is) = do
    processInstruction  il i
    processInstructions il is

processInstruction :: Int -> E.TaskStmnt -> StateT CurrentContext IO()
processInstruction il ins = 
    case ins of 
        E.DefineFunc _ fIns         -> increaseState >> contextChecker fIns

        ic@(E.IfCondition _ si fi)-> putOnState (identString il (show ic)) >>
                                    putOnState  (identString (il+1) "Intruccion en exito:") >>
                                    processInstruction (il + 2) si >>
                                    when ( fi /= E.Skip ) 
                                        (putOnState (identString (il+1) "Intruccion en fracaso:")) >>
                                        processInstruction (il + 2) fi 

        rp@(E.Repeat rt inst)     -> increaseState                         >>
                                    putOnState (identString il $ show rp) >>
                                    processInstruction (il + 1) inst

        wl@(E.WhileCond _ inst)   -> increaseState                         >>
                                    putOnState (identString il $ show wl) >>
                                    processInstruction  (il + 1) inst

        be@(E.BeginEnd _ insts)   -> increaseState                         >>
                                    putOnState (identString il $ show be) >>
                                    processInstructions (il + 1) insts
        
        fc@(E.FuncCall _)         -> putOnState (identString il $ show fc )

        _                         -> putOnState (identString il "LLAMADA A INSTRUCCION:") >>
                                    putOnState (identString (il+1) $ show ins)
    
contextChecker :: E.TaskStmnt -> StateT CurrentContext IO ()
contextChecker tsk = case tsk of 
    -- Si es un if, incrementa el estado del contexto, revisa los contextos de la instruccion 
    -- de exito y la de fail
    E.IfCondition _ si fi -> increaseState >> contextChecker si >> contextChecker fi 
    -- Si es un define, aumenta y revisa el contexto de las instrucciones internas
    E.DefineFunc _ ins    -> increaseState >> contextChecker ins 
    E.Repeat _ ins        -> increaseState >> contextChecker ins 
    E.WhileCond _ ins     -> increaseState >> contextChecker ins 
    E.BeginEnd _ (t:ts)   -> increaseState >> foldl (\b a -> b >> contextChecker a) (contextChecker t) ts >> return ()
    _                   -> return ()

-- Aumenta el estado de contexto actual
increaseState :: StateT CurrentContext IO ()
increaseState = do
    currSt <- get
    put (currSt + 1)
    return ()

-- genera un string que representa una identación un un nivel de identación dado
ident :: Int -> String
ident idLevel = replicate (idLevel*2) ' '

-- Identa un string con el nivel de identación dado
identString :: Int -> String -> String
identString il str = unlines $ map (ident il ++) (lines str)

-- Imprime en modo State monad
putOnState :: String -> StateT CurrentContext IO ()
putOnState = io . putStr

io :: IO a -> StateT CurrentContext IO a
io = liftIO