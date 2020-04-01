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
                    ammount :: Int -- Amount of this object
                    } deriving(Show)

type ItemSet = M.Map String Item  --Set of objects

data Object = WillyPos | Wall | Items{itemSet :: ItemSet} deriving(Show)

type WorldMap = M.Map (Int,Int) Object

data Orientation = North | South | West | East deriving(Show, Eq)

data Willy = Willy{ currPos     :: (Int, Int),      -- (x,y)
                    looking     :: Orientation,     -- Where is willy looking at
                    basket      :: ItemSet,       -- Map from objectType id to Object
                    frontClear  :: Bool,
                    leftClear   :: Bool,
                    rightClear  :: Bool
                    } deriving(Show)

data ProgramState = Program{
                            wMap            :: WorldMap,                    -- Map from world positions to objects
                            willyStartPos   :: E.WorldStmnt,                --StartAt
                            programState    :: ProcessState,                -- Current program status
                            willy           :: Willy,                       -- Willy object 
                            vars            :: M.Map String ST.Symbol,      -- map from symbol id to bool var symbol
                            basketCapacity  :: Int,                         -- Total Willy capacity
                            finalGoal       :: ST.Symbol,                      -- A boolean expresion defining a final goal
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
                            items           = os,
                            willyStartPos   = wsp,
                            programState    = ps,
                            willy           = w,
                            vars            = vs,
                            basketCapacity  = bc,
                            finalGoal       = fg,
                            context         = c,
                            taskId          = tid,
                            worldId         = wid,
                            symbolTable     = newSt
                            } 
    where
        task = case ST.findSymbol st id of
                    Nothing     -> error . show $ NoSuchTask id
                    Just sym    -> sym

        worldName = T.getId' . E.workingWorld . ST.exprs . ST.symType $ task

        world = case findSymbol st worldName of
                    Nothing  -> error . show $ NoSuchWorld worldName
                    Just sym -> sym

        newSt = ST.loadTask st $ symId task

        os = foldl addToObjMap M.empty . ST.placeAt . ST.symType $ world
        --Aux
        addToObjMap :: M.Map (Int, Int) ItemSet -> E.WorldStmnt -> M.Map (Int, Int) ItemSet
        addToObjMap m placeAt@E.PlaceAt{} = let 
            x      =   T.getInt' . fst . E.place $ placeAt 
            y      =   T.getInt' . snd . E.place $ placeAt 
            objid  =   T.getId'  . E.objectTypeIdAt $ placeAt
            symobj = case findSymbol newSt objid of 
                        Nothing  -> error . show $ NoSuchObjectType objid
                        Just sym -> sym
            n      =   T.getInt' . E.ammountAt $ symobj
            
            in case M.lookup (x,y) m of
                Nothing   -> M.insert (x,y) (addItems emptySet symobj n) m
                Just objs -> M.insert (x,y) (addItems objs symobj n) m
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
    | ammount result < n = M.delete s os
    | otherwise = M.insert s result{ammount = ammount result - n} os
    where   found = M.lookup s os
            result = fromJust found

--Summary: Given an object set, an object symbol s, an int n, add n of s 
--         to the ItemSet and return the resulting ItemSet.
addItems :: ItemSet -> ST.Symbol -> Int -> ItemSet
addItems os s n  
    | n <= 0 = os
    | not (ST.isObjType . ST.symType $ s) = error $ "Error adding object to ItemSet: expected an object-type symbol. Given: " ++ show s
    | otherwise = M.insert (ST.symId s) Item{ symbol=s, ammount=n } os

-- Summary add item from an item object
addItems' :: ItemSet -> Item -> ItemSet
addItems' os Item{symbol = s, ammount = n} = addItems os s n

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
---  End of object set operations ---
-------------------------------------

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
-------- < Error Messages >  --------
-------------------------------------
instance Show RuntimeError where
    show NoSuchTask{ errTaskName = s }      = "Willy runtime error: No existe una tarea con el nombre dado: " ++ s
    show NoSuchWorld{ errWorldName = s }    = "Willy runtime error: No existe un mundo con el nombre dado: " ++ s
    show NoSuchObjectType{errObjTypeId = s} = "Willy runtime error: No existe un mundo con el nombre dado: " ++ s