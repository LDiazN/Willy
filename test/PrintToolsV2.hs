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



main :: IO()
main = do 
    (filename:_) <- getArgs

    content <- readFile filename  

    case tokenizer content of
        Right tks -> do
            let ast = parseClean $ cleanTokens tks
            runStateT (printProg ast) 0
            print "Prog ready"
            (_, st) <- CA.analyzeAST ast
            printSymTable st
        Left s ->
            print ""

printProg :: E.AST -> StateT CurrentContext IO ()
printProg ast = do
    io $ putStrLn "TAREAS:"

    processAST ast
    
    return ()
    where
        processAST :: E.AST -> StateT CurrentContext IO ()
        processAST [] = return ()
        processAST (x:xs) = processProgPart x >> processAST xs

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

printSymTable :: CA.ContextState -> IO()
printSymTable st@ST.SymbolTable{ST.symbolMap = m} = do
    -- Una lista con todos los símbolos de la tabla
    print "debug"
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

        printType ST.DefineFunc{ST.body = b, ST.fBlockId = bid} = 
            "Tipo: Intruccion\n" ++
            "Identificador del bloque: " ++ show bid ++ "\n" ++
            "AST Asocioado: "
        
        printType _ = "WIP"
        printSym :: ST.Symbol -> IO()
        printSym s@ST.Symbol{ST.symId = sid, ST.symType = stype, ST.symContext = scont} = do
            putStrLn $ sid ++ ":"
            putStrLn $ "  Bloque de declaración: " ++ show scont
            putStr $ identString 1 (printType stype)

            --when  (ST.isDef stype) $ do processInstruction 1 (ST.body stype)
                

            




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


increaseState :: StateT CurrentContext IO ()
increaseState = do
    currSt <- get
    put (currSt + 1)
    return ()

ident :: Int -> String
ident idLevel = replicate (idLevel*2) ' '

identString :: Int -> String -> String
identString il str = unlines $ map (ident il ++) (lines str)

putOnState :: String -> StateT CurrentContext IO ()
putOnState = io . putStr

io :: IO a -> StateT CurrentContext IO a
io = liftIO