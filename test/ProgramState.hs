-- This module implements the Willy Program State. 
-- These are the data structures needed to run a Willy program
-- The Willy interpreter its build upon this structures
module ProgramState where

import qualified SymbolTable as ST
import qualified Expresions as E
import qualified Tokens as T
import qualified Data.Map as M
import Data.Maybe


data ProcessState = Running | Success | Error deriving(Show, Eq)    --Possible program status

data Item = Item{   symbol  :: ST.Symbol, -- ObjType symbol  
                    amount :: Int -- Amount of this object
                    } deriving(Show)

type ItemSet = M.Map String Item  --Set of objects

data Object = WillyPos | Wall | Items{itemSet :: ItemSet} deriving(Show)

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
                            worldMap        :: WorldMap,                    -- Map from world positions to objects
                            programState    :: ProcessState,                -- Current program status
                            willy           :: Willy,                       -- Willy object 
                            basketCapacity  :: Int,                         -- Total Willy capacity
                            finalGoal       :: E.WorldStmnt,                      -- A boolean expresion defining a final goal
                            symbolTable     :: ST.SymbolTable,
                            taskId          :: String,
                            worldId         :: String     
                            } deriving(Show)


data RuntimeError = NoSuchTask{ errTaskName :: String }
                  | NoSuchWorld{ errWorldName :: String }
                  | NoSuchObjectType{errObjTypeId :: String}

---------------------------------------
---- < Helper program operations > ----
---------------------------------------
-- Summary: Given the symbol table, and a taskid, returns a ProgramState
--          with such data
initProgramState :: ST.SymbolTable -> String -> ProgramState
initProgramState st id = Program{
                            worldMap        = os,
                            programState    = Running,
                            willy           = w,
                            basketCapacity  = T.getInt' . E.capacity . head . ST.capacity . ST.symType $ world,
                            finalGoal       = head . ST.finalGoal . ST.symType $ world,
                            symbolTable     = newSt,
                            taskId          = ST.symId task,
                            worldId         = worldName
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

        os = foldl addToObjMap M.empty . ST.placeAt . ST.symType $ world

        w = Willy{
                    currPos = ( T.getInt' . fst $ stpos , T.getInt' . snd $ stpos ),
                    looking = tokToOrientation . T.tok . E.initDirection . head . ST.startPos . ST.symType $ world,
                    basket  = emptySet,
                    frontClear = True,
                    leftClear  = True,
                    rightClear = True
                }

        stpos = case ST.startPos . ST.symType $ world of
                    (s:[]) -> E.initPos s
                    _      -> error "Runtime error: Programa inválido. Definición incorrecta de posición inicial"


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
    | amount result < n = M.delete s os
    | otherwise = M.insert s result{amount = amount result - n} os
    where   found = M.lookup s os
            result = fromJust found

--Summary: Given an object set, an object symbol s, an int n, add n of s 
--         to the ItemSet and return the resulting ItemSet.
addItems :: ItemSet -> ST.Symbol -> Int -> ItemSet
addItems os s n  
    | n <= 0 = os
    | not (ST.isObjType . ST.symType $ s) = error $ "Error adding object to ItemSet: expected an object-type symbol. Given: " ++ show s
    | otherwise = M.insert (ST.symId s) Item{ symbol=s, amount=n } os

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


-------------------------------------
------ < Wolrd Map Operations > -----
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
tokToOrientation t = error $ "Error en tokToOrientation: Esto no es un token de orientación" ++ show t


-------------------------------------
-------- < Error Messages >  --------
-------------------------------------
instance Show RuntimeError where
    show NoSuchTask{ errTaskName = s }      = "Willy runtime error: No existe una tarea con el nombre dado: " ++ s
    show NoSuchWorld{ errWorldName = s }    = "Willy runtime error: No existe un mundo con el nombre dado: " ++ s
    show NoSuchObjectType{errObjTypeId = s} = "Willy runtime error: No existe un mundo con el nombre dado: " ++ s