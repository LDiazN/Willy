module Interpreter where

import qualified SymbolTable as ST
import qualified ProgramState as PS
import qualified Expresions as E
import Control.Monad.State
import Control.Monad
import System.IO
import System.Environment

type RunState a = StateT PS.ProgramState IO a

-- Summary: Given the symbol table returned by the ContextAnalyzer, 
--          A string representing a task to run,
--          A callback function that receives a program State and makes an io operation,
--          run that task applying the callback function to the resulting state
--          at each step
runProgram :: ST.SymbolTable -> String -> (PS.ProgramState -> IO()) -> IO ((),PS.ProgramState)
runProgram st s f = runStateT (runProgram' f) (PS.initProgramState st s)

runProgram' :: (PS.ProgramState -> IO()) -> RunState ()
runProgram' f = do 
    ps@PS.Program{PS.instrs=instrs} <- get

    io $ f ps

    unless (null instrs) $ foldl (\b a -> b >> runInst a) (runInst . head $ instrs) $ tail instrs

    
    where 
        runInst :: E.TaskStmnt -> RunState ()
        runInst E.Move{} = do
            ps <- get
            let newps = PS.walk ps 
            io $ f newps
            put newps

        runInst _ = return()

--Required to wrap an io monad within the state monad
io :: IO a -> RunState a
io = liftIO  
