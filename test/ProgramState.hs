-- This module implements the Willy Program State. 
-- These are the data structures needed to run a Willy program
-- The Willy interpreter its build upon this structures
module ProgramState where

import qualified SymbolTable as ST
import qualified Expresions as E
import qualified Tokens as T
import qualified Data.Map as M
import Data.Maybe


data ProcessState = Running | Error | End deriving(Show, Eq)    --Possible program status

data Item = Item{   symbol  :: ST.Symbol, -- ObjType symbol  
                    amount :: Int -- Amount of this object
                    } deriving(Show)

type ItemSet = M.Map String Item  --Set of objects

data Object = Wall 
            | Items{itemSet :: ItemSet} 
            deriving(Show)

type WorldMap = M.Map (Int,Int) Object

data Orientation = North | South | West | East deriving(Show, Eq)

data Willy = Willy{ currPos     :: (Int, Int),      -- (x,y)
                    looking     :: Orientation,     -- Where is willy looking at
                    basket      :: ItemSet,         -- Map from objectType id to Object
                    frontClear  :: Bool,
                    leftClear   :: Bool,
                    rightClear  :: Bool
                    } deriving(Show)

data ProgramState = Program{
                            worldMap        :: WorldMap,         -- Map from world positions to objects
                            programState    :: ProcessState,     -- Current program status
                            willy           :: Willy,            -- Willy object 
                            basketCapacity  :: Int,              -- Total Willy capacity
                            finalGoal       :: E.WorldStmnt,     -- A boolean expresion defining a final goal
                            symbolTable     :: ST.SymbolTable,   -- The symbol table related to this program
                            taskId          :: String,           -- Name of the task currently loaded
                            worldId         :: String,           -- Name of the world currently loaded
                            instrs          :: [E.TaskStmnt],    -- Instructions to be executed
                            worldSize       :: (Int, Int)
                            } deriving(Show)


data RuntimeError = NoSuchTask{ errTaskName :: String }
                  | NoSuchWorld{ errWorldName :: String }
                  | NoSuchObjectType{errObjTypeId :: String}
                  | WallInFront{ errWallPos :: (Int,Int) }
                  | BasketCapacityExceeded {errCap :: Int, errAdd :: Int, errUsedSpace :: Int}
                  | InvalidObjectId{errObjId :: String }
                  | NoSuchObjectCurrPos{errObjTk :: T.TokPos, errCurrPos :: (Int,Int)}
                  | NoSuchObjInBasket {errObjTkBask :: T.TokPos}
                  | NoSuchBoolVar{errBoolVar :: T.TokPos}
                  | NoSuchFunc{errFuncId :: T.TokPos}

---------------------------------------
---- < Helper program operations > ----
---------------------------------------
-- Summary: Given the symbol table, and a taskid, returns a ProgramState
--          with such data
initProgramState :: ST.SymbolTable -> String -> ProgramState
initProgramState st id = updateWillySensors Program{
                            worldMap        = mww,
                            programState    = Running,
                            willy           = w,
                            basketCapacity  = T.getInt' . E.capacity . head . ST.capacity . ST.symType $ world,
                            finalGoal       = head . ST.finalGoal . ST.symType $ world,
                            symbolTable     = newSt,
                            taskId          = ST.symId task,
                            worldId         = worldName,
                            instrs          = E.instructions . ST.exprs . ST.symType $ task,
                            worldSize       = wsize
                            } 
    where
        task = case ST.findSymbol st id of
                    Nothing     -> error . show $ NoSuchTask id
                    Just sym    -> sym

        worldName = T.getId' . E.workingWorld . ST.exprs . ST.symType $ task

        world = case ST.findSymbol st worldName of
                    Nothing  -> error . show $ NoSuchWorld worldName
                    Just sym -> sym

        newSt = ST.loadTask st $ ST.symId task

        os = foldl addToObjMap M.empty . ST.placeAt . ST.symType $ world -- map w/ objects
        mww = addWallsToMap os . ST.walls . ST.symType $ world -- map W/ walls

        w = Willy{
                    currPos = ( T.getInt' . fst $ stpos , T.getInt' . snd $ stpos ),
                    looking = tokToOrientation . T.tok . E.initDirection . head . ST.startPos . ST.symType $ world,
                    basket  = wBask,
                    frontClear = True,
                    leftClear  = True,
                    rightClear = True
                }

        wBask = foldl addItemsToBask emptySet (ST.placeIn . ST.symType $ world)

        stpos = case ST.startPos . ST.symType $ world of
                    (s:[]) -> E.initPos s
                    _      -> error "Runtime error: Programa inválido. Definición incorrecta de posición inicial"

        wsize' =  head . ST.worldSize . ST.symType $ world
        wsize  = ( T.getInt' . E.cols $ wsize', T.getInt' . E.rows $ wsize' )
        --Aux
        addToObjMap :: WorldMap -> E.WorldStmnt -> WorldMap
        addToObjMap m placeAt@E.PlaceAt{} = let 
            x      =   T.getInt' . fst . E.place $ placeAt 
            y      =   T.getInt' . snd . E.place $ placeAt 
            objid  =   T.getId'  . E.objectTypeIdAt $ placeAt
            symobj = case ST.findSymbol newSt objid of 
                        Nothing  -> error . show $ NoSuchObjectType objid
                        Just sym -> sym
            n      =    T.getInt' . E.amountAt $ placeAt
            
            in addItemToMap m (x,y) Item{symbol = symobj, amount = n}
        addToObjMap m _ = m

        addWallsToMap :: WorldMap -> [E.WorldStmnt] -> WorldMap
        addWallsToMap  = foldl addWallToMap 
        
        addItemsToBask :: ItemSet -> E.WorldStmnt -> ItemSet
        addItemsToBask is E.PlaceIn{E.objectTypeIdIn = tkid, E.amountIn=tkn} = 
            let 
                id = T.getId' tkid
                n = T.getInt' tkn
                maybeSym = ST.findSymbol newSt id
            in 
                case maybeSym of
                    Nothing  -> error "Bug: Un símbolo que pasó el análisis estático no se consigue"
                    Just sym -> addItems is sym n 
-- Summary: Walk one unit
walk :: ProgramState -> ProgramState
walk ps = 
    let lookingAt = looking w
        (x,y) = currPos w
        w = willy ps
        (wsx,wsy) = worldSize ps
        nextPos@(npx,npy) = case lookingAt of
                    North -> (x,y+1)
                    South -> (x,y-1)
                    West  -> (x-1,y)
                    East  -> (x+1,y)
        wm = worldMap ps
        withinBounds = npx >= 1 && npy >= 1 && npx <= wsx && npy <= wsy

    in if withinBounds && positionFree wm nextPos
        then ps{willy=w{currPos = nextPos}} 
        else error . show $ WallInFront nextPos
-------------------------------------
----- End of program operations -----
-------------------------------------

-------------------------------------
-- < Helper ItemSet operations > --
-------------------------------------
--Summary: Given an item set, an id s, an int n, remove n of s 
--         from the ItemSet and return the resulting ItemSet.
--         If the amount is bigger than the number of  currently stored
--         objects, then delete that object from the set
removeItems :: ItemSet -> String -> Int -> ItemSet
removeItems os s n 
    | n == 0 = os
    | isNothing found = os
    | amount result <= n = M.delete s os
    | otherwise = M.insert s result{amount = amount result - n} os
    where   found = M.lookup s os
            result = fromJust found

--Summary: Given an object set, an object symbol s, an int n, add n of s 
--         to the ItemSet and return the resulting ItemSet.
addItems :: ItemSet -> ST.Symbol -> Int -> ItemSet
addItems os s n  
    | n <= 0 = os
    | not (ST.isObjType . ST.symType $ s) = error $ "Error adding object to ItemSet: expected an object-type symbol. Given: " ++ show s
    | otherwise = case getItem os (ST.symId s) of
                    Nothing -> M.insert (ST.symId s) Item{ symbol=s, amount=n } os
                    Just Item{amount=n'} -> M.insert (ST.symId s) Item{ symbol=s, amount=n+n'} os

-- Summary add item from an item object
addItems' :: ItemSet -> Item -> ItemSet
addItems' os Item{symbol = s, amount = n} = addItems os s n

--Summary: Given an object set, an object id s, look up an object with id s 
-- (in fact this is just an alias for the map lookup)
getItem :: ItemSet -> String -> Maybe Item
getItem os s = M.lookup s os

--Summary: Given an object set, get all the objects stored
getItems :: ItemSet -> [Item]
getItems = M.elems

--Summary: Return an empty object set (an alias)
emptySet :: ItemSet
emptySet = M.empty

--Summary: get the total number of items in willy's basket
elemsInItemSet :: ItemSet -> Int
elemsInItemSet  = foldl (\acc it -> acc + amount it) 0 . M.elems 

--Summary: try to add n of id to the willy basket
addToBasket :: ProgramState -> Int -> String -> ProgramState
addToBasket ps n id 
    | n + usedSpace > bsize = error . show $ BasketCapacityExceeded bsize n usedSpace
    | isNothing sym  || (not . ST.isObjType . ST.symType . fromJust $ sym) = error . show $ InvalidObjectId id
    | otherwise = ps{willy=newW}
    where 
        usedSpace = elemsInItemSet . basket . willy $ ps
        bsize     = basketCapacity ps
        st        = symbolTable ps 
        sym       = ST.findSymbol st id 
        w         = willy ps
        newW      = w{basket = addItems (basket w) (fromJust sym) n}
        
-------------------------------------
------ < World Map Operations > -----
-------------------------------------

-- Summary: Add the given object to the map in the given position.
--          If the object to add is an itemSet, this functions returns the given
--          map.
addToWorldMap :: WorldMap -> (Int,Int) -> Object -> WorldMap
addToWorldMap wm p Items{itemSet = is} = foldl (\m i -> addItemToMap m p i) wm $ M.elems is
addToWorldMap wm p o = case M.lookup p wm of
                        Nothing -> M.insert p o wm
                        _       -> wm

-- Summary: Add the given item to the map in the given position. If that position contains
--          something different to an item set, ignore the given item
addItemToMap :: WorldMap -> (Int, Int) -> Item -> WorldMap
addItemToMap wm p i  = case M.lookup p wm of
                        Nothing                -> M.insert p Items{itemSet = addItems' emptySet i} wm
                        Just Items{itemSet=s}  -> M.insert p Items{itemSet = addItems' s i} wm
                        _                      -> wm

-- Summary: remove n of the given item by its id in the map in the given position. 
--          If that position contains something different to an item set
--          ignore the given item
removeItemFromMap :: WorldMap -> (Int, Int) -> String -> Int -> WorldMap
removeItemFromMap wm p s n = newWM
    where   newItSet = case M.lookup p wm of
                        Just Items{itemSet=is} -> removeItems is s n
                        _   -> emptySet
            newWM  = if elemsInItemSet newItSet == 0
                        then M.delete p wm
                        else M.insert p Items{itemSet=newItSet} wm

-- Summary: Add a wall to the map
addWallToMap :: WorldMap -> E.WorldStmnt -> WorldMap
addWallToMap wm w@E.Wall{} = foldl (\ m p -> addToWorldMap m p Wall) wm pairs
    where 
        from@(fx,fy) = ( T.getInt' . fst . E.from $ w, T.getInt' . snd . E.from $ w)
        to@(tx,ty)   = ( T.getInt' . fst . E.to $ w, T.getInt' . snd . E.to $ w)
        xs = [min fx tx .. max fx tx]
        ys = [min fy ty .. max fy ty]
        pairs = [(i,j) | i <- xs, j <- ys]
addWallToMap _ w = error $ "Error adding wall to map. This is not a wall: " ++ show w

-- Summary: tells if the given position is valid to walk in
positionFree :: WorldMap -> (Int,Int) -> Bool
positionFree wm p = case M.lookup p wm of 
                Just Wall -> False
                _         -> True
-------------------------------------
---  End of world map  operations ---
-------------------------------------

-------------------------------------
-------- < Misc  Operations > -------
-------------------------------------

-- Summary: Convert from a token to a orientation:
tokToOrientation :: T.Token -> Orientation
tokToOrientation T.TkNorth = North
tokToOrientation T.TkSouth = South
tokToOrientation T.TkWest  = West
tokToOrientation T.TkEast  = East
tokToOrientation t = error $ "Error en tokToOrientation: Esto no es un token de orientación: " ++ show t

-- Summary: updates the robot sensors
updateWillySensors :: ProgramState -> ProgramState
updateWillySensors ps = 
    let w = willy ps
        (x,y) = currPos w
        dir = looking w
        wm = worldMap ps
        (wsx, wsy) = worldSize ps
        isFree :: (Int,Int) -> Bool
        isFree p@(x',y') = positionFree wm p && (x' <= wsx && y' <= wsy && x' >= 1 && y' >= 1)

        (left,front,right) = case dir of 
                                North -> (isFree (x-1,y), isFree (x,y+1), isFree (x+1,y))
                                South -> (isFree (x+1,y), isFree (x,y-1), isFree (x-1,y))
                                West  -> (isFree (x,y-1), isFree (x-1,y), isFree (x,y+1))
                                East  -> (isFree (x,y+1), isFree (x+1,y), isFree (x,y-1))
    in ps{willy = w{frontClear=front, leftClear=left, rightClear=right}}

-- Summary: test a world boolean goal
checkGoal :: ProgramState -> E.GoalTest -> Bool
checkGoal ps E.WillyAt{E.willyAtPos=p} = 
    let pint = ( T.getInt' . fst $ p, T.getInt' . snd $ p)
        wpos = currPos . willy $ ps
    in pint == wpos

checkGoal ps E.WillyBasketObjs{E.objIdBask = oib, E.objAmountInBask = oaib} = 
    let obname = T.getId' oib
        obamnt = T.getInt' oaib
        bsk = basket . willy $ ps
    in case M.lookup obname bsk of
        Nothing                 -> obamnt == 0
        Just Item{amount=n'}    -> n'==obamnt

checkGoal ps E.WillyObjectsAt{E.objIdAt=oia, E.objAmountAt=oaa, E.objsPos=opos} = 
    let amnt = T.getInt' oaa
        oid  = T.getId' oia
        p    = ( T.getInt' . fst $ opos,T.getInt' . snd $ opos )
        wm   = worldMap ps
    in case M.lookup p wm of
        Nothing                  -> amnt == 0
        Just Items{itemSet = is} -> case M.lookup oid is of
                                        Nothing        -> amnt == 0
                                        Just Item{amount=n} -> amnt==n
        _                        -> False

-- Summary: evaluates a BoolExpr
checkBoolExpr :: ProgramState -> E.BoolExpr -> Bool
checkBoolExpr ps E.Constant{E.consVal = (T.TkTrue, _,_)} = True
checkBoolExpr ps E.Constant{E.consVal = (T.TkFalse, _,_)} = False
checkBoolExpr ps E.Constant{E.consVal = (T.TkFrontClear, _,_)} = frontClear . willy $ ps
checkBoolExpr ps E.Constant{E.consVal = (T.TkLeftClear, _,_)} = leftClear . willy $ ps
checkBoolExpr ps E.Constant{E.consVal = (T.TkRightClear, _,_)} = rightClear . willy $ ps
checkBoolExpr ps E.Constant{E.consVal = (T.TkLookingNorth, _,_)} = (looking . willy $ ps)==North
checkBoolExpr ps E.Constant{E.consVal = (T.TkLookingSouth, _,_)} = (looking . willy $ ps)==South
checkBoolExpr ps E.Constant{E.consVal = (T.TkLookingEast, _,_)} = (looking . willy $ ps)==East
checkBoolExpr ps E.Constant{E.consVal = (T.TkLookingWest, _,_)} = (looking . willy $ ps)==West
checkBoolExpr ps E.Constant{E.consVal = (T.TkId s ,_,_)} = 
    let symtable = symbolTable ps
        sym = ST.findSymbol symtable s

        --aux: tells if a tkbool is true, false, or invalid
        checkBoolVar :: T.TokPos -> Bool
        checkBoolVar (T.TkTrue,_,_) = True
        checkBoolVar (T.TkFalse,_,_) = False
        checkBoolVar (t, _, _) = error $ "Error en checkBoolExpr: esta no es una constante booleana válida" ++ show t
    in
        case sym of 
            Nothing -> error $ "Error al evaluar bool expr: el símbolo dado no existe en este contexto" ++ s
            Just ST.Symbol{ST.symType = ST.BoolVar{ST.initVal=b}} -> checkBoolVar b
            Just ST.Symbol{ST.symType = ST.Goal{ST.goal = g}} -> checkGoal ps g
            Just err  -> error $ "Error evaluando bool expr: el símbolo dado no es booleano: " ++ show err
        

checkBoolExpr ps E.Query{E.queryType = qt, E.targetName = tn} = 
    let qtype = T.tok qt
        tid     = T.getId' tn
        bskt    = basket . willy $ ps
        wm      = worldMap ps
        wpos    = currPos . willy $ ps

        --In case of carrying type
        checkCarry :: String -> Bool
        checkCarry s = case getItem bskt s of
                        Nothing -> False
                        Just Item{amount=n} -> n/=0
        --In case of found type:
        checkFound :: String -> Bool
        checkFound s = case M.lookup wpos wm of
                        Nothing -> False
                        Just Items{itemSet = is} -> case getItem is s of
                                                    Nothing -> False
                                                    Just Item{amount=n} -> n/=0
    in case qtype of
        T.TkFound       -> checkFound tid
        T.TkCarrying    -> checkCarry tid
checkBoolExpr ps E.NotExpr{E.notExpr = ne} = not . checkBoolExpr ps $ ne
checkBoolExpr ps E.Operation{E.operator = op, E.operand1 = oprn1, E.operand2 = oprn2} =
    let lbool = checkBoolExpr ps oprn1
        rbool = checkBoolExpr ps oprn2

    in case T.tok op of
        T.TkAnd -> lbool && rbool
        T.TkOr  -> lbool || rbool
        e       -> error $ "Error checking a boolean expr: This is not an operator: " ++ show e

-- Summary: turn willy to look at the given direction
lookAt :: ProgramState -> Orientation -> ProgramState
lookAt ps or = let w = willy ps in ps{willy=w{looking=or}}
lookAt' :: ProgramState -> T.Token -> ProgramState
lookAt' ps = lookAt ps . tokToOrientation 

-- Summary: rotate willy to the left
turnLeft :: ProgramState -> ProgramState
turnLeft ps = case looking . willy $ ps of
                North -> lookAt ps West
                South -> lookAt ps East
                East  -> lookAt ps North
                West  -> lookAt ps South

-- Summary: rotate willy to the right
turnRight :: ProgramState -> ProgramState
turnRight ps = case looking . willy $ ps of
                North -> lookAt ps East
                South -> lookAt ps West
                East  -> lookAt ps South
                West  -> lookAt ps North

--Summary: Set to The given boolean value the variable named by the given 
--         id (or explodes if it is not a bool var) in the current programState
setBoolVar :: String -> Bool -> ProgramState -> ProgramState
setBoolVar id b ps = ps{symbolTable = newSt}
    where
        st = symbolTable ps 
        newSt = ST.setVal st id b 


-------------------------------------
-------- < Error Messages >  --------
-------------------------------------

-- This function returns a formated string with a position in file
posToString :: (Int, Int) -> String
posToString pos = "linea: " ++ (show . fst ) pos ++ 
                  ", columna: " ++ (show . snd ) pos

instance Show RuntimeError where
    show NoSuchTask{ errTaskName = s }      = "Willy runtime error: No existe una tarea con el nombre dado: " ++ s

    show NoSuchWorld{ errWorldName = s }    = "Willy runtime error: No existe un mundo con el nombre dado: " ++ s

    show NoSuchObjectType{errObjTypeId = s} = "Willy runtime error: No existe un objeto con el nombre dado: " ++ s

    show WallInFront{ errWallPos = p }      = "Willy runtime error: Willy intentó caminar hacia un muro en: " ++ show p

    show BasketCapacityExceeded{errCap = c, 
                                errAdd = a,
                                errUsedSpace = u
                                } = "Willy runtime error: Añadiendo muchos elementos a la cesta." ++ 
                                    "\n capacidad de la cesta: " ++ show c ++
                                    "\n Capacidad utilizada: " ++ show u ++
                                    "\n Cantidad añadida: " ++ show a

    show InvalidObjectId{errObjId = s} = "Willy runtime error: El nombre dado no coincide con ningún objeto " ++
                                         "válido en este contexto." ++
                                         "\n nombre: " ++ s

    show NoSuchObjectCurrPos{errObjTk = tk, 
                             errCurrPos = c} ="Willy runtime error: No existe este objeto en la casilla actual." ++
                                              "\n Objeto: " ++ T.getId' tk ++
                                              "\n Casilla Actual: " ++ show c ++
                                              "\n Cerca de " ++ (posToString . T.pos $ tk)

    show NoSuchObjInBasket{
                        errObjTkBask=objtk} = "Willy runtime error: No existe este objeto en la cesta actualmente." ++
                                              "\n Objeto: " ++ T.getId' objtk ++
                                              "\n Cerca de " ++ (posToString . T.pos $ objtk)

    show NoSuchBoolVar{
                        errBoolVar=objtk} = "Willy runtime error: Esto no es una variable booleana válida." ++
                                              "\n nombre: " ++ T.getId' objtk ++
                                              "\n Cerca de " ++ (posToString . T.pos $ objtk)

    show NoSuchFunc{errFuncId=objtk} = "Willy runtime error: Esto no es una función válida en este contexto." ++
                                       "\n nombre: " ++ T.getId' objtk ++
                                       "\n Cerca de " ++ (posToString . T.pos $ objtk)

    
