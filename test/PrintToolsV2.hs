import Expresions
import Parser
import Tokens
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
        Right tks ->
            runStateT (code $ parseClean $ cleanTokens tks) 0 >> return ()
        Left s ->
            print ""

code :: AST -> StateT CurrentContext IO ()
code ast = do
    io $ putStrLn "TAREAS:"

    processAST ast
    
    return ()
    where
        ident :: Int -> String
        ident idLevel = replicate (idLevel*2) ' '

        identString :: Int -> String -> String
        identString il str = unlines $ map (ident il ++) (lines str)

        processAST :: AST -> StateT CurrentContext IO ()
        processAST [] = return ()
        processAST (x:xs) = processProgPart x >> processAST xs

        -- Procesa un bloque de programa. 
        processProgPart :: ProgPart -> StateT CurrentContext IO ()
        processProgPart (World _ _) = increaseState

        processProgPart (Task tn ww inst) = do
            increaseState
            block <- get
            io $ putStrLn $ getId' tn ++ " :"            -- Imprime: "nombre_de_task:"
            io $ putStrLn $ "  mundo: " ++ getId' ww     -- Imprime: "  mundo: nombre_mundo"
            io $ putStrLn   "  bloque de instrucciones:"-- Imprime: "  bloque de instrucciones"
            io $ putStrLn $ "    Identificador del bloque: " ++ show block 
            -- Imprime todas las instrucciones
            processInstructions 2 inst
            


        
        processInstructions :: Int -> [TaskStmnt] -> StateT CurrentContext IO ()
        processInstructions _ [] = return ()
        processInstructions il (i:is) = do
            processInstruction  il i
            processInstructions il is

        processInstruction :: Int -> TaskStmnt -> StateT CurrentContext IO()
        processInstruction il ins = 
            case ins of 
                DefineFunc _ fIns        -> increaseState >> contextChecker fIns

                ic@(IfCondition _ si fi) -> putOnState (identString il (show ic)) >>
                                            putOnState  (identString (il+1) "Intruccion en exito:") >>
                                            processInstruction (il + 2) si >>
                                            when ( fi /= Skip ) 
                                                (putOnState (identString (il+1) "Intruccion en fracaso:")) >>
                                                processInstruction (il + 2) fi 

                rp@(Repeat rt inst)      -> increaseState                         >>
                                            putOnState (identString il $ show rp) >>
                                            processInstruction (il + 1) inst

                wl@(WhileCond _ inst)    -> increaseState                         >>
                                            putOnState (identString il $ show wl) >>
                                            processInstruction  (il + 1) inst

                be@(BeginEnd _ insts)    -> increaseState                         >>
                                            putOnState (identString il $ show be) >>
                                            processInstructions (il + 1) insts
                
                fc@(FuncCall _)          -> putOnState (identString il $ show fc )

                _                        -> putOnState (identString il "LLAMADA A INSTRUCCION:") >>
                                            putOnState (identString (il+1) $ show ins)
            
        contextChecker :: TaskStmnt -> StateT CurrentContext IO ()
        contextChecker tsk = case tsk of 
            -- Si es un if, incrementa el estado del contexto, revisa los contextos de la instruccion 
            -- de exito y la de fail
            IfCondition _ si fi -> increaseState >> contextChecker si >> contextChecker fi 
            -- Si es un define, aumenta y revisa el contexto de las instrucciones internas
            DefineFunc _ ins    -> increaseState >> contextChecker ins 
            Repeat _ ins        -> increaseState >> contextChecker ins 
            WhileCond _ ins     -> increaseState >> contextChecker ins 
            BeginEnd _ (t:ts)   -> increaseState >> foldl (\b a -> b >> contextChecker a) (contextChecker t) ts >> return ()
            _                   -> return ()



increaseState :: StateT CurrentContext IO ()
increaseState = do
    currSt <- get
    put (currSt + 1)
    return ()

getId' :: TokPos -> String
getId' = getId . tok
        
putOnState :: String -> StateT CurrentContext IO ()
putOnState = io . putStr

io :: IO a -> StateT CurrentContext IO a
io = liftIO