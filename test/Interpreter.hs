module Interpreter where

import qualified SymbolTable as ST
import qualified ProgramState as PS
import qualified Expresions as E
import qualified Tokens as T
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad
import System.IO
import System.Environment
import Data.Maybe

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
    mapM_ runInst instrs

    newps <- get 
    case PS.programState newps of 

        PS.Error   -> io $ putStrLn "Error durante la ejecución de la tarea."
        
        _       ->  if PS.checkBoolExpr newps . E.finalGoal . PS.finalGoal $ newps
                        then io $ putStrLn "Ejecución exitosa: Willy alcazó el objetivo final."    
                        else io $ putStrLn "Ejecución fracasada: Willy no alcanzó el objetivo final."
        
                            --foldl (\b a -> b >> runInst a) (runInst . head $ instrs) $ tail instrs
    
    where 
        runInst :: E.TaskStmnt -> RunState ()
        -- Run move
        runInst E.Move{} = do
            ps <- get
            unless (PS.programState ps==PS.End) $ do
            let newps = PS.updateWillySensors . PS.walk $ ps 
            io $ f newps
            put newps

        -- Run Turn Left
        runInst E.TurnLeft{} = do
            ps <- get
            unless (PS.programState ps==PS.End) $ do
            let newps = PS.updateWillySensors . PS.turnLeft $ ps 
            io $ f newps
            put newps

        -- Run Turn right
        runInst E.TurnRight{} = do
            ps <- get
            unless (PS.programState ps==PS.End) $ do
            let newps = PS.updateWillySensors . PS.turnRight $ ps 
            io $ f newps
            put newps

        -- Run pick operation:
        runInst E.Pick{E.pickObj = pobj} = do
            ps <- get
            unless (PS.programState ps==PS.End) $ do
            let w = PS.willy ps
                wm = PS.worldMap ps
                oid = T.getId' pobj
                newps = (PS.addToBasket ps 1 oid){PS.worldMap=PS.removeItemFromMap wm currPos oid 1} 
                currPos =  PS.currPos . PS.willy $ ps
                obj = M.lookup currPos . PS.worldMap $ ps
                item = flip PS.getItem oid . PS.itemSet . fromJust $ obj 
                --Aux: tells if the given object its an itemset
                checkObj :: PS.Object -> Bool
                checkObj PS.Items{} = True
                checkObj _ = False

            if isNothing obj  || (not . checkObj . fromJust $ obj) || 
               isNothing item || (PS.amount . fromJust $ item) == 0
                then error . show $ PS.NoSuchObjectCurrPos pobj (PS.currPos w)
                else io (f newps) >> put newps

        -- Run drop
        runInst E.Drop{E.dropObj=dobj} = do
            ps <- get
            unless (PS.programState ps==PS.End) $ do
            let oid  = T.getId' dobj
                w    = PS.willy ps
                wbsk = PS.basket w
                newbsk = PS.removeItems wbsk oid 1
                maybeItem = PS.getItem wbsk oid
                wm = PS.worldMap ps
                newWm = PS.addItemToMap wm (PS.currPos w) (fromJust maybeItem){PS.amount=1}
                newPs = case maybeItem of
                            Nothing -> error . show $ PS.NoSuchObjInBasket dobj
                            Just PS.Item{PS.amount = n} -> if n == 0
                                                    then error . show $ PS.NoSuchObjInBasket dobj
                                                    else ps{ PS.willy = w{PS.basket=newbsk}, 
                                                            PS.worldMap = newWm} 
            io $ f newPs 
            put newPs

        -- Run set var
        runInst E.SetOper{E.varToSet = tkid, E.boolVar=tkb} = do
            ps <- get
            unless (PS.programState ps==PS.End) $ do
            --io $ putStrLn "not in interpreter"
            let 
                b = T.tokToBool . T.tok $ tkb   --Get the boolean val
                id = T.getId' tkid              --Get the string id
                newPs = PS.setBoolVar id b ps   --Assign val
            io $ f newPs
            put newPs

        -- Run clear var
        runInst E.ClearOper{E.varToClear = vtc} = do
            ps <- get
            unless (PS.programState ps==PS.End) $
                runInst E.SetOper{E.varToSet = vtc, E.boolVar=(T.TkFalse,0,0)}

        --Run Flip Oper 
        runInst E.FlipOper{E.varToFlip = vtf} = do
            ps <- get
            unless (PS.programState ps==PS.End) $ do
            let 
                id = T.getId' vtf
                boolVar =  case flip ST.findSymbol id . PS.symbolTable $ ps of
                            Just ST.Symbol{ST.symType=ST.BoolVar{ST.initVal=tkb}} -> not . T.tokToBool . T.tok $ tkb
                            _ -> error . show $ PS.NoSuchBoolVar vtf
                setoper = E.SetOper{E.varToSet = vtf, E.boolVar=(T.boolToTok boolVar,0,0)}
            
            runInst setoper

        --Run if oper
        runInst e@E.IfCondition{ E.ifCondition=bexpr, E.succInstruction = si, E.failInstruction = fi} = do
            ps <- get
            unless (PS.programState ps==PS.End) $ do
                let 
                    st = PS.symbolTable ps
                    ibid = E.ibid e

                put ps{PS.symbolTable = ST.pushBid st ibid} --Push the if context

                if PS.checkBoolExpr ps bexpr
                    then runInst si
                    else runInst fi
                ps' <- get 
                put ps'{PS.symbolTable = ST.popContext . PS.symbolTable $ ps'} --pop the if context


        --Run repeat 
        runInst e@E.Repeat{E.repeatTimes = tkn, E.repInstruction = inst} = do
            ps <- get 
            unless (PS.programState ps==PS.End) $ do
                let n = T.getInt' tkn
                    st = PS.symbolTable ps
                    rbid = E.rbid e

                put ps{PS.symbolTable = ST.pushBid st rbid} --Push repeat context

                replicateM_ n (runInst inst)

                ps' <- get 
                put ps'{PS.symbolTable = ST.popContext . PS.symbolTable $ ps'} --pop the repeat context



        --Run while
        runInst w@E.WhileCond{E.whileCondition = wc, E.whileIntruct = wi} = do
            ps <- get
            unless (PS.programState ps==PS.End) $ do
                let 
                    st = PS.symbolTable ps
                    wbid = E.wbid w

                put ps{PS.symbolTable = ST.pushBid st wbid} --Push while context

                when (PS.checkBoolExpr ps wc) $ runInst wi >> runInst w

                ps' <- get 
                put ps'{PS.symbolTable = ST.popContext . PS.symbolTable $ ps'} --pop the while context

        --Run BeginEnd
        runInst E.BeginEnd{E.beginIntructs = insts} = do
            ps <- get
            unless (PS.programState ps==PS.End) $
                mapM_ runInst insts

        --Run function call
        runInst E.FuncCall{E.funcId = fid} = do
            ps <- get
            unless (PS.programState ps==PS.End) $ do
            let
                id = T.getId' fid
                st = PS.symbolTable ps
                (instrs,context) = 
                    case ST.findSymbol st id of
                        Just ST.Symbol{ST.symType = ST.DefineFunc{ST.body=b,ST.fBlockId=fbid}} -> (b,fbid)
                        Nothing -> error . show $ PS.NoSuchFunc fid
            
            put $ ps{PS.symbolTable = ST.pushBid st context}                --load the function context
            ps' <- get                                                      --Get the resulting state

            runInst instrs                                                  --Run the function body
            ps' <- get                                                      --Get the resulting state
            put $ ps'{PS.symbolTable = ST.popContext . PS.symbolTable $ ps'}--Pop the previously loaded context
            
        
        runInst E.Terminate{} = do
            ps <- get
            unless (PS.programState ps==PS.End) $
                put ps{PS.programState = PS.End}

        runInst _ = return()

        

--Required to wrap an io monad within the state monad
io :: IO a -> RunState a
io = liftIO  
