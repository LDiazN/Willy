module ContextAnalyzer where

import Control.Monad.State
import Control.Monad
import System.IO
import System.Environment
import Data.Maybe
import Data.Typeable
import Data.List
import Data.Function
import qualified Expresions as E
import qualified Tokens as T
import qualified Data.Map as M
import qualified Data.List as L
import qualified SymbolTable as ST

type ContextState = ST.SymbolTable
type RetState  a  = StateT ContextState IO a


--Given an AST, returns the corresponding symbol table
analyzeAST :: E.AST -> IO ((), ContextState) 
analyzeAST ast = runStateT (analyzer ast) (ST.SymbolTable M.empty [0] 1 ST.NoCon []) 

-- Analyze the given ast 
analyzer :: E.AST -> RetState()
analyzer ast = unless (null ast) $ do
            --process all progparts:
            
            mapM_ processProgPart (E.setContext 1 ast)
            
            -- Check for errors
            ST.SymbolTable{ST.errors =  e}  <- get

            -- print Errors if needed
            unless (null e) $ io (mapM_ (putStrLn . (++ "\n")) (reverse e) >> return ())

    where 
        processProgPart :: E.ProgPart -> RetState ()
        processProgPart w@(E.World name ppts) = do
            -- The symbol name
            let id = T.getId . T.tok $ name
            --Check if the world can be inserted
            available <- findSymbol id 
            case available of 
                Nothing -> do
                    -- Since we start a new context, push an empty context table
                    pushEmptyTable
                    st@ST.SymbolTable{ST.contextStack = bid:_} <- get
                    -- Update state to world context:
                    put st{ST.context = ST.WorldCon}

                    -- Create the world type
                    worldType@ST.World{
                                    ST.worldSize = ws, 
                                    ST.capacity = capc, 
                                    ST.startPos = stpos,
                                    ST.finalGoal = fgoal} <-  foldl addWorldIntr' (retStateWrap ST.emptyWorld) ppts
                    -- Return to normal Sate

                    st <- get
                    put st{ST.context = ST.NoCon}
                    -- Check the requirements of the world:
                    -- 1) If no start position declaration, then add the default one (1,1, north)
                    -- 2) If no basket capacity avalable, then add the default (capacity 1)
                    -- 3) If no world size, then add the default (1,1)
                    -- 4) If final goal is null, then add an error
                    let 
                        tkint1  = (T.TkInt 1, 0, 0)
                        tknorth = (T.TkNorth, 0, 0)

                        worldType' = if null ws 
                                        then worldType{ ST.worldSize = [E.WorldSize tkint1 tkint1] }
                                        else worldType
                        worldType''= if null capc
                                        then worldType'{ ST.capacity = [E.BasketCapacity tkint1] }
                                        else worldType'
                        fWorldType = if null stpos
                                        then worldType''{ST.startPos = [E.StartAt (tkint1,tkint1) tknorth ]}
                                        else worldType''
                    
                    -- Check if the final goal is null:
                    when (null fgoal) $ addError (ST.NoFinalGoal id)
                    -- pop the world context
                    popContext
                    
                    -- Add the new created  world
                    void (insertSymbol $ ST.Symbol id fWorldType{ST.wBlockId = bid} 0 (T.pos name))
                    
                Just _ -> 
                        -- When we try to insert a symbol that already exists, the insert
                        -- method will place an error on the error Stack
                        void(insertSymbol $ ST.Symbol id ST.emptyWorld 0 (T.pos name) )
            

        processProgPart t@(E.Task name ww instrs) = do

            ST.SymbolTable{ST.contextCounter = bid} <- get

            let id   = T.getId' name    
                tsymType = ST.Task t bid
                tsym = ST.Symbol id tsymType 0 (T.pos name)
            -- check if the world is a valid world
            valid <- checkTypeExst ST.isWorld ww
            -- search its context
                -- if it is valid, then add the context of the world to the current context
            when valid $ do
                    st@ST.SymbolTable{ ST.contextStack = stk} <- get 
                    Just ST.Symbol{ ST.symType = ST.World{ST.wBlockId = scont} } <- findSymbol (T.getId' ww)
                    put st{ST.contextStack = scont:stk}


            --Push the context for the current task
            pushEmptyTable

            -- Update the context to task context
            st  <- get
            put st{ST.context = ST.TaskCon} 

            -- Check the task:
            checkInstructions instrs

            popContext -- pop task context
            -- Pop the world if needed
            when valid  $ void popContext

            -- Update the context to task context
            st  <- get
            put st{ST.context = ST.NoCon} 
            insertSymbol tsym
            return ()


        -- This function gets a world statement and a world symbol type and 
        -- try to add the given property to the world
        addWorldIntr :: ST.SymType -> E.WorldStmnt -> RetState ST.SymType
        --For wall checking
        addWorldIntr w@ST.World{ST.walls = walls, ST.startPos = stpos} stmnt@(E.Wall dir from to) 
            |  not (checkWallDir dir from to) = addError (ST.InconsisWallDir dir from to) >> return w
            |  isWillyOverWall stpos from to  = 
                let stpos' = if null stpos 
                                then E.StartAt ((T.TkInt 1, 0, 0) ,(T.TkInt 1, 0, 0)) (T.TkNorth, 0, 0)
                                else head stpos
                in
                addError (ST.InconsisWallOverWilly (T.pos dir) from to (E.initPos stpos')) >> return w
            |  otherwise                      = return w{ST.walls = stmnt:walls}

        -- for WorldSize checking:
        addWorldIntr w@ST.World {ST.worldSize = wsize} stmnt@(E.WorldSize rows cols) 
            | not (null wsize)       = addError (ST.RedefWSize rows)   >> return w
            | T.getInt' rows <= 0 || T.getInt' cols <= 0 = addError (ST.InvalidWSize rows cols) >> return w
            | otherwise  = return w{ST.worldSize = [stmnt]}

        -- ObjectType checking
        addWorldIntr w stmnt@(E.ObjectType id color) = do
            let id' = T.getId' id
            insertSymbol $ ST.Symbol  id' (ST.ObjType stmnt) 0 (T.pos id)
            return w

        -- PlaceAt checking
        addWorldIntr w@ST.World{ST.worldSize = wsize} stmnt@(E.PlaceAt tkid amnt (px,py)) = do
            -- Get the symbol being referenced
            sym <- findSymbol $ T.getId' tkid

            --Check if the object it's within the world boundaries
            let wsize'     = if null wsize 
                                then E.WorldSize (T.TkInt 1, 0, 0) (T.TkInt 1, 0, 0)
                                else head wsize

                (wsx, wsy) = (T.getInt' $ E.rows wsize', T.getInt' $ E.cols wsize')

                (pxint, pyint) = (T.getInt' px, T.getInt' py)

                amntInt = T.getInt' amnt
                
            --if the object is placed somewhere out of the world:
            if isNothing sym 
                then addError (ST.UndefRef tkid) >> return w
            else if let stype = (ST.symType . fromJust $ sym) in not (ST.isObjType stype) 
                then addError (ST.InvalidObjType tkid) >> return w
            else if wsx < pxint || wsy < pyint || pxint==0 || pyint==0
                then addError (ST.PlaceOutOfBound (wsx, wsy) (pxint, pyint) tkid) >> return w
            else if amntInt == 0
                then addError (ST.PlaceZeroAt amnt) >> return w
            else return w{ST.placeAt = stmnt:ST.placeAt w}

        -- Place In checking 
        addWorldIntr w@ST.World{ST.placeIn = plin, ST.capacity = cap} stmnt@(E.PlaceIn otid amnt) = do
            -- 1) We have to tell if the symbol exists and if it's an ObjectType symbol
            -- 2) We have to tell if the sum of the symbols is <= than capacity

            let -- cap' is the capacity of the basket
                cap' = if null cap
                            then 1
                            else (T.getInt' . E.capacity . head) cap
                -- Sum is the sum of all place-in-basket including the new one
                sum'  = T.getInt' amnt + sum (map (T.getInt' . E.amountIn) plin)

            sym <- findSymbol (T.getId' otid)
            if isNothing sym 
                then addError (ST.UndefRef otid) >> return w 
            else if let stype = (ST.symType . fromJust $ sym) in not (ST.isObjType stype) 
                then addError (ST.InvalidObjType otid) >> return w
            else if sum' > cap' 
                then addError (ST.CapacityExceeded otid amnt amnt) >> return w
            else if T.getInt' amnt <= 0
                then addError (ST.PlaceZeroObj amnt) >> return w
            else 
                return w{ ST.placeIn = stmnt:plin}
        
        --StartAt checking:
        addWorldIntr w@ST.World{ST.worldSize = wsize, ST.startPos = stpos, ST.walls = walls} stmnt@(E.StartAt (tkpx, tkpy) tkdir) 
            -- We have to check: 
            --  1) startPos must be empty 
            --  2) given position must be between world boundaries
            --  3) given position must be out of a wall
            --  4) given position cannot be (0,0)
            | not (null stpos) = addError (ST.RedefStartPos tkpx) >> return w
            | T.getInt' tkpx > wsx || T.getInt' tkpy > wsy = addError (ST.StartPosOOB tkpx) >> return w
            | T.getInt' tkpx == 0 || T.getInt' tkpy == 0 = do
                let ret = if T.getInt' tkpx==0
                            then tkpx
                            else tkpy 

                addError $ ST.StartPosOOB ret 
                return w
            | or wallsOverWill = addError (ST.WillyOverWall tkpx) >> return w            
            | otherwise = return w{ST.startPos = stmnt:stpos}

            where   (wsx, wsy) = if null wsize 
                                    then (1,1)
                                    else ( T.getInt' . E.rows . head $ wsize, T.getInt' . E.rows . head $ wsize)
                    wallsOverWill = [isWillyOverWall (stmnt:stpos) (E.from i) (E.to i) | i <- walls]  

        -- Basket of capacity checking:
        addWorldIntr w@ST.World{ST.capacity = cap } stmnt@(E.BasketCapacity amnt) 
            -- we have to check:
            --  1) Capacity must be a positive integer
            --  2) No redefinition of capacity
            | T.getInt' amnt <= 0 = addError (ST.NullBaskCapacity amnt)  >> return w
            | not (null cap)      = addError (ST.RedefBaskCapacity amnt) >> return w
            | otherwise           = return w{ST.capacity = stmnt:cap}

        --Boolean definition Checking:
        addWorldIntr w stmnt@(E.BooleanVar bname bval) = do
            -- We have to check:
            -- 1) Name is currently unavailable in the symbol table
            -- The insertSymbol function adds the corresponding error if the symbol already exists
            insertSymbol (ST.Symbol (T.getId' bname) (ST.BoolVar bval) 0 (T.pos bname))
            return w

        --Check goal defiition
        addWorldIntr w stmnt@(E.Goal gname gtest) = do
            -- We have to check:
            -- 1) Name is currently unavailable in the symbol table
            -- 2) Depending on the goal type, we have to make several checks
            -- Check the goal test for errors 
            checkGoalTest gtest w
            -- The insertSymbol function adds the corresponding error if the symbol already exists
            insertSymbol (ST.Symbol (T.getId' gname) (ST.Goal gtest) 0 (T.pos gname))
            return w

        --Check final goal definition:
        addWorldIntr w@ST.World{ST.finalGoal = fgoal} stmnt@(E.FGoal be fgpos) 
            | not (null fgoal)  = addError (ST.RedefFGoal fgpos) >> return w
            | otherwise = do

                b <- checkBoolExpr be
                --io (print $ typeOf bools)

                if b
                    then return w{ST.finalGoal = [stmnt]} 
                    else return w
            
        addWorldIntr w _ = return w

        --Check a Task Statement set 
        checkInstructions :: [E.TaskStmnt] -> RetState ()
        checkInstructions = mapM_ checkInstruction 

        --Check a Task Statement for errors
        checkInstruction :: E.TaskStmnt -> RetState ()
        -- Check a function call
        checkInstruction (E.FuncCall fname) = void (checkTypeExst ST.isDef fname)

        -- Check an if block
        checkInstruction (E.IfCondition ifCond sins fins _) = do
            -- Create a new context for the if block
            pushEmptyTable

            -- check if all the names in the boolExpr are correct
            checkBoolExpr ifCond 
            -- Check the success instruction
            checkInstruction sins
            -- Check the fail instruction
            checkInstruction fins

            -- Pop the if-block context
            popContext
            return ()

        -- Check a while block
        checkInstruction (E.WhileCond wCond inst _) = do
            
            -- Create a new context for the if block
            pushEmptyTable

            -- check if all the names in the boolExpr are correct
            checkBoolExpr wCond 
            -- Check the success instruction
            checkInstruction inst

            -- Pop the if-block context
            popContext
            return ()

        --Check the repeat instruction
        checkInstruction (E.Repeat _ inst _) = do
            
            -- Create a new context for the if block
            pushEmptyTable
 
            -- Check the success instruction
            checkInstruction inst

            -- Pop the if-block context
            popContext
            return ()
        
        --Check the begin instruction:
        checkInstruction (E.BeginEnd _ instrs) = void (checkInstructions instrs)
        
        --Check the define instruction:
        checkInstruction (E.DefineFunc fname finst) = do

            ST.SymbolTable{ST.contextCounter = bid} <- get

            let name  = T.getId' fname
                stype = ST.DefineFunc finst bid
                pos   = T.pos fname

            insertSymbol (ST.Symbol name stype 0 pos)

            pushEmptyTable

            checkInstruction finst

            popContext
            return()
        
        --check pick
        checkInstruction (E.Pick oid) = void $ checkTypeExst ST.isObjType oid

        -- check drop
        checkInstruction (E.Drop oid) = void $ checkTypeExst ST.isObjType oid

        -- check set
        checkInstruction (E.SetOper oid _) = void $ checkTypeExst ST.isBool oid

        -- check Clear
        checkInstruction (E.ClearOper _ oid) = void $ checkTypeExst ST.isBool oid

        --check flip
        checkInstruction (E.FlipOper _ oid) = void $ checkTypeExst ST.isBool oid



        checkInstruction _ = return ()

        -- aux function, given a RetState-wrapped symtype, and a world statement, 
        -- returns a RetState wrapped symtype.
        addWorldIntr' :: RetState ST.SymType -> E.WorldStmnt -> RetState ST.SymType     
        addWorldIntr' w stmnt = do
            w' <- w
            addWorldIntr w' stmnt

        -- Returns true if the wall is consistent with the direction 
        checkWallDir :: T.TokPos -> (T.TokPos, T.TokPos) -> (T.TokPos, T.TokPos) -> Bool
        checkWallDir dir from to =  ( T.tok dir == T.TkNorth && T.tok (fst from) == T.tok (fst to) &&
                                      T.getInt ( T.tok (snd from) ) <= T.getInt ( T.tok (snd to)) )  ||
                                    ( T.tok dir == T.TkSouth && T.tok (fst from) == T.tok (fst to) &&
                                      T.getInt ( T.tok (snd from) ) >= T.getInt ( T.tok (snd to)) )  ||
                                    ( T.tok dir == T.TkEast  && T.tok (snd from) == T.tok (snd to) &&
                                      T.getInt ( T.tok (fst from) ) <= T.getInt (T.tok (fst to) ))  ||
                                    ( T.tok dir == T.TkWest  && T.tok (snd from) == T.tok (snd to) &&
                                      T.getInt ( T.tok (fst from) ) >= T.getInt (T.tok (fst to) ))  

        -- Returns true if willy the StartAt list contains a start position of willy where the given wall 
        -- is over
        isWillyOverWall :: [E.WorldStmnt] -> (T.TokPos,T.TokPos) -> (T.TokPos,T.TokPos) -> Bool
        isWillyOverWall [] _ _ = False
        isWillyOverWall (E.StartAt (x, y) _ : xs) (fx,fy)  (tx,ty) = 
                            (T.tok x == T.tok fx && T.tok fx == T.tok tx &&
                                 ( T.getInt' fy <= T.getInt' y &&  T.getInt' y <= T.getInt' ty ||
                                 T.getInt' ty <= T.getInt' y &&  T.getInt' y <= T.getInt' fy)
                            ) ||
                            (T.tok y == T.tok fy && T.tok fy == T.tok ty &&
                                 ( T.getInt' fx <= T.getInt' x &&  T.getInt' x <= T.getInt' tx ||
                                 T.getInt' tx <= T.getInt' x &&  T.getInt' x <= T.getInt' fx)
                            )
        
        isWillyOverWall _ _ _= False

        -- Given a GoalTest and the world limits checks if its correct. If it is not, then addError
        -- and return False. Otherwise returns True.
        checkGoalTest :: E.GoalTest -> ST.SymType -> RetState Bool
        --Check willyAt
        checkGoalTest gt@E.WillyAt{ E.willyAtPos = tp} w = goalCheckBounds tp w

        --Check WillyObjectsAt
            --To check a WillyObjectsAt test, we have to check
            -- 1) position is within world boundaries
            -- 2) id exists
            -- 3) id is an object type
        checkGoalTest gt@E.WillyObjectsAt{ E.objIdAt = tkid, E.objsPos = tkpos} w = do
            -- 1) check boundaries:
            boundOk <- goalCheckBounds tkpos w

            -- 2) Check id existense and type
            symOk <- checkTypeExst ST.isObjType tkid

            return $ symOk && boundOk

        -- Check WillyBasketObjs
            -- To check objexts in basket, we hace to check:
            -- 1) id exists
            -- 2) id names an object type
        
        checkGoalTest gt@E.WillyBasketObjs{ E.objIdBask = tkid} _ = checkTypeExst ST.isObjType tkid

        --Aux Function: Check if the given position is within the given world boundaries
        goalCheckBounds :: (T.TokPos, T.TokPos) -> ST.SymType -> RetState Bool
        goalCheckBounds tp@(tkpx, tkpy) w@ST.World{ST.worldSize = st} = do
            let (wsx, wsy) = if null st 
                                then (1,1)
                                else (T.getInt' . E.rows . head $ st, T.getInt' . E.cols . head $ st)

                (tkpxInt, tkpyInt) = (T.getInt' tkpx, T.getInt' tkpy)
            
            if tkpxInt == 0 || tkpyInt == 0 || tkpxInt > wsx || tkpyInt > wsy
                then addError (ST.GoalOutOfBound tp (wsx, wsy)) >> return True
                else return False
        

-- Add a new symbol table. In fact, just update the context stack and increase the
-- next-context counter
pushEmptyTable :: RetState ()
pushEmptyTable = do
    st@(ST.SymbolTable _ stk cnt _ _) <- get
    put st{ST.contextStack = cnt : stk, ST.contextCounter = cnt + 1} 

-- Pop a context from the context stack
popContext :: RetState Int
popContext = do
    st@(ST.SymbolTable _ stk _ _ _) <- get
    unless (null stk) $ put st{ ST.contextStack = tail stk } --unless context stack is empty, replace it with tail

    if not (null stk)
        then return $ head stk
        else return $ -1
    

-- Tells if the context stack is empty
emptyContext :: RetState Bool
emptyContext = do
    (ST.SymbolTable _ stk _ _ _) <- get
    return $ null stk

-- Try to find the symbol in the current context
findSymbol :: String -> RetState (Maybe ST.Symbol)
findSymbol name = do
    st@(ST.SymbolTable m stk _ _ _ ) <- get

    case M.lookup name m of
        Nothing     -> return Nothing
        Just syms   -> return $ maybeMaxBy (compare `on` ST.symContext) (filter (available stk) syms)
                                
    where 
        
        available :: [Int] -> ST.Symbol -> Bool
        available xs (ST.Symbol _ _ c _) = foldl (\b a -> c==a || b) False xs

        maybeMaxBy ::  (a -> a -> Ordering) -> [a] -> Maybe a
        maybeMaxBy f [] = Nothing
        maybeMaxBy f l = Just $ maximumBy f l


--Insert a non existing symbol into the symbol table with the current context
insertSymbol :: ST.Symbol -> RetState Bool
insertSymbol sym@(ST.Symbol id stype _ p) = do
    -- Get the current state of the symbol table
    st@(ST.SymbolTable m stk nxt ctxt errs ) <- get

    let 
        currCont = head stk
    -- Check if the symbol already exists:
    exists <- findSymbol id
    
    case exists of
        -- The symbol is in the table in the current context
        Just s@ST.Symbol{ST.symContext = scon} ->
            if scon == currCont
                then put st{ST.errors = show (ST.SymRedef sym):errs} >> return False
                else insertSymbol' sym{ST.symContext = currCont} >> return True

        Nothing -> do
            check <- checkCtxt sym
            if not check
                then put st{ST.errors = show (ST.UnmatchContext sym ctxt):errs} >> return False
                else insertSymbol' sym{ST.symContext = currCont} >> return True
    
    where 
        
        -- This functions checks if the symbol can be inserted 
        -- in the current Program State (defining a world or a task)
        checkCtxt :: ST.Symbol -> RetState Bool
        checkCtxt (ST.Symbol _ stype _ _) = do

            st@(ST.SymbolTable m stk nxt ctxt errs ) <- get

            case ctxt of
                ST.NoCon    ->  return $ ST.isTask stype || ST.isWorld stype 
                ST.WorldCon ->  return $ ST.isBool stype || ST.isGoal stype || ST.isObjType stype
                ST.TaskCon  ->  return $ ST.isDef  stype 

        --This function inserts an already checked symbol into the table
        insertSymbol' :: ST.Symbol -> RetState ()
        insertSymbol' sym' = do
            
            st@(ST.SymbolTable m stk nxt ctxt errs ) <- get
            
            let list = M.lookup (ST.symId sym') m

            case list of
                Nothing -> put st{ST.symbolMap = M.insert id [sym'] m}
                Just xs -> put st{ST.symbolMap = M.insert id (sym':xs) m}


-- Check The existence and type of a TkId in the current context.
-- If it is not available, put an error into the world errors,
-- otherwise if the found id does not match the given function Type,
-- Add an unmatched type error. If there was some error, this function returns
-- False, otherwise returns true.
checkTypeExst :: (ST.SymType -> Bool) -> T.TokPos -> RetState Bool
checkTypeExst f id = do
    sym <- findSymbol (T.getId' id) 

    if isNothing sym
        then addError (ST.UndefRef id) >> return False
    else if let stype = ST.symType (fromJust sym) in not (f stype) 
        then addError (ST.UnmatchedType id) >> return False
    else return True


-- Given a Boolean expresion, tells if the Expresion contains correct symbols.
-- If it doesn't, then add the corresponding errors to the error stack
checkBoolExpr' :: E.BoolExpr -> RetState Bool
checkBoolExpr' be = and <$>  mapM (checkTypeExst isBool') (names be)  
    where 
        -- This function returns all the variable names in a bool expr
        names :: E.BoolExpr -> [T.TokPos]
        names (E.Constant t@(T.TkId _, _, _)) = [t]
        names E.Operation{E.operand1 = op1, E.operand2 = op2} = names op1 ++ names op2
        names (E.NotExpr ne)  = names ne
        names _ = []

        isBool' :: ST.SymType -> Bool
        isBool' s = ST.isBool s || ST.isGoal s

checkBoolExpr :: E.BoolExpr -> RetState Bool
checkBoolExpr be = do
    --Check if every bool id is a correct id
    idsok <- checkBoolExpr' be

    --Check if every Query has an id as argument
    qsok <- checkQuery be

    return $ idsok && qsok

    where 
        --Checks if every query in a bool expresion has an objtype as an argument
        checkQuery :: E.BoolExpr -> RetState Bool
        checkQuery E.Query{E.targetName = tn} = checkTypeExst ST.isObjType tn
        checkQuery E.Operation{E.operand1 = op1, E.operand2 = op2} = do
            q1 <- checkQuery op1
            q1 <- checkQuery op2

            return $ q1 && q1
        checkQuery _ = return True



-- adds the given error to the world errosreturn $ head stk
addError :: ST.Error -> RetState ()
addError err = do
    st@ST.SymbolTable{ ST.errors = oldErrs} <- get
    put st{ST.errors = show err:oldErrs}

-- Wraps a tupe a into a RetState monad
retStateWrap :: a -> RetState a
retStateWrap = return

-- Needed to use IO within State monad context
io :: IO a -> StateT ContextState IO a
io = liftIO  

-- Debug function: print the current state
debugPrintState :: RetState ()
debugPrintState = do
    st <- get
    io $ print st

