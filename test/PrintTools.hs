import Expresions
import Parser
import Tokens
import Lexer
import Control.Monad.State
import System.IO
import System.Environment



data PrintState = PrintState { taskPrint :: [String], currentBlockId :: Int}


main :: IO ()
main = do
    (filename:_) <- getArgs

    content <- readFile filename  

    case tokenizer content of
        Right tks ->
            runStateT (code $ parseClean $ cleanTokens tks) (PrintState ["TAREAS:"] 0) >> return ()
        Left s ->
            print ""


code :: AST -> StateT PrintState IO ()
code ast = do

        
    readProgPart ast        
    
    st <- get
    io $ putStrLn $ unlines $ reverse $ taskPrint st




readProgPart :: [ProgPart] ->  StateT PrintState IO ()
readProgPart [] = return ()
readProgPart (( World _ _ ):xs) = do 
    currSt <- get                                           -- Obtiene el estado actual
    put currSt{ currentBlockId = currentBlockId currSt + 1} --  Actualiza aumentando en uno el contador del block id
    readProgPart xs

readProgPart (( Task tn ww insts ):xs) = do
    -- Get state and add the header
    currSt <- get
    put currSt{ currentBlockId = currentBlockId currSt + 1} -- Actualiza el contexto
    put currSt{ taskPrint = taskHeader currSt }             -- Añade la header
    currSt' <- get

    foldl (taskInstructs 0)

    readProgPart xs                                            -- Lee el resto del programa
    where 
        -- Añade la header de un task
        taskHeader :: PrintState -> [String]
        taskHeader ps@(PrintState pl currid) = ("        Identificador de bloque: " ++ show currid):
                                                "    Bloque de instrucciones:" :
                                               ("    mundo: " ++ (getId . tok) ww):

                                                (getId . tok) tn : pl
        -- Imprime una sola instrucción segun la identación dada
        
        
--        taskInstructs id_level ic@(IfCondition be si fi) oinds = 
--            case be of 
--                Operation  oper l r -> ["a"]
--
--            where   ident = replicate (id_level*4) ' '
--
--                    ifinstruct = ident ++ "Intruccion:\n" ++ ident ++ "    " ++ show si
--                    nifinstruct = if fi != Skip 
--                        then  ident ++ "Intruccion Fail:\n" ++ ident ++ "    " ++ show fi
--                        else ""
                    
                    
ident :: Int -> String
ident x = replicate x ' '

-- Used to convert from IO monad to state monad
io :: IO a -> StateT PrintState IO a
io = liftIO

