module ContextAnalyzer where

import Control.Monad.State
import Control.Monad
import System.IO
import System.Environment
import Data.Maybe
import qualified Expresions as E
import qualified Tokens as T
import qualified Data.Map as M
import qualified Data.List as L
import qualified SymbolTable as ST

type ContextState = ST.SymbolTable
type RetState  a  = StateT ContextState IO a

analyzeAST :: E.AST -> IO ()
analyzeAST ast = runStateT (analyzer ast) (ST.SymbolTable M.empty [] 0 ST.NoCon []) >> return ()

analyzer :: E.AST -> RetState ()
analyzer ast = unless (null ast) $ do
            --process all progparts:
            mapM_ processProgPart ast
            
            -- Check for errors
            (ST.SymbolTable _ _ _ _ e)  <- get

            if null e
                then io $ putStrLn "Everything ok"
                else io (mapM_ (putStrLn . (++ "\n")) e >> return ())

    where 
        processProgPart :: E.ProgPart -> RetState ()
        processProgPart w@(E.World name ppts) = do
            -- The symbol name
            let id = T.getId . T.tok $ name

            -- Since we start a new context, push an empty context table
            pushEmptyTable

            
            --Check if the world can be inserted
            available <- findSymbol id 
            case available of 
                Nothing -> do
                        -- Update state to world context:
                        st <- get
                        put st{ST.context = ST.WorldCon}
                        -- Create the world type
                        worldType <-  foldl addWorldIntr' (retStateWrap ST.emptyWorld) ppts
                        -- Return to normal Sate
                        st <- get
                        put st{ST.context = ST.NoCon}
                        -- Add the new created  world
                        try <- insertSymbol $ ST.Symbol id worldType 0 (T.pos name)
                        return ()

                Just _ -> do
                        -- When we try to insert a symbol that already exists, the insert
                        -- method will place an error on the error Stack
                        insertSymbol $ ST.Symbol id ST.emptyWorld 0 (T.pos name) 
                        return ()



            -- If it is possible to add the world, check if it is correct
            --when try $

            return ()

        processProgPart t@(E.Task name ww instrs) = do
            return()

        
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
            | T.getInt' rows <= 0 && T.getInt' cols <= 0 = addError (ST.InvalidWSize rows cols) >> return w
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
                
            --if the object is placed somewhere out of the world:
            if isNothing sym 
                then addError (ST.UndefRef tkid) >> return w
            else if let stype = (ST.symType . fromJust $ sym) in not (ST.isObjType stype) 
                then addError (ST.InvalidObjType tkid) >> return w
            else if wsx < pxint || wsy < pyint
                then addError (ST.PlaceOutOfBound (wsx, wsy) (pxint, pyint) tkid) >> return w
            else return w

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
            | not (null stpos) = addError (ST.RedefStartPos tkpx) >> return w
            | T.getInt' tkpx > wsx || T.getInt' tkpy > wsy = addError (ST.StartPosOOB tkpx) >> return w
            | or wallsOverWill = addError (ST.WillyOverWall tkpx) >> return w            
            | otherwise = return w{ST.startPos = stmnt:stpos}

            where   (wsx, wsy) = if null wsize 
                                    then (1,1)
                                    else ( T.getInt' . E.rows . head $ wsize, T.getInt' . E.rows . head $ wsize)
                    wallsOverWill = [isWillyOverWall (stmnt:stpos) (E.from i) (E.to i) | i <- walls]  

        -- Basket of capacity checking:
        addWorldIntr w@ST.World{ST.capacity = cap } stmnt@(E.BasketCapacity amnt) 
            -- we have tu check:
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
            -- The insertSymbol function adds the corresponding error if the symbol already exists
            insertSymbol (ST.Symbol (T.getId' gname) (ST.Goal gtest) 0 (T.pos gname))
            return w

        --Check final goal definition:
        addWorldIntr w@ST.World{ST.finalGoal = fgoal} stmnt@(E.FGoal be fgpos) 
            | not (null fgoal)  = addError (ST.RedefFGoal fgpos) >> return w
            | otherwise = return w{ST.finalGoal = [stmnt]}
            
         
        addWorldIntr w _ = return w


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
    return $ head stk

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
        Just syms   -> return $ head' $ filter (available stk) syms
                                
    where 
        
        available :: [Int] -> ST.Symbol -> Bool
        available xs (ST.Symbol _ _ c _) = foldl (\b a -> c==a || b) False xs

        head' :: [a] -> Maybe a
        head' [] = Nothing
        head' (x:xs) = Just x

--Insert a non existing symbol into the symbol table with the current context
insertSymbol :: ST.Symbol -> RetState Bool
insertSymbol sym@(ST.Symbol id stype _ p) = do
    -- Get the current state of the symbol table
    st@(ST.SymbolTable m stk nxt ctxt errs ) <- get

    -- Check if the symbol already exists:
    exists <- findSymbol id

    case exists of
        -- The symbol is in the table in the current context
        Just _  -> put st{ST.errors = show (ST.SymRedef sym):errs} >> return False

        Nothing -> do
                    check <- checkCtxt sym
                    if not check
                        then put st{ST.errors = show (ST.UnmatchContext sym ctxt):errs} >> return False
                        else insertSymbol' sym{ST.symContext = head stk} >> return True

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

-- adds the given error to the world erros
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