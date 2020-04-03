-- This module contains the data structure of the SymbolTable. 
-- Since the symbol table needs a state to work, all the operations
-- the table supports relies on the ContextAnalyzer module
module SymbolTable where

import qualified Expresions as E
import qualified Tokens as T
import qualified Data.Map as M
import Data.Typeable
import Data.List
import Data.Function
import Data.Maybe
-- The possible symbols:
data SymType = BoolVar  {initVal :: T.TokPos}
             | ObjType  {objColor :: E.WorldStmnt}
             | DefineFunc { body :: E.TaskStmnt , fBlockId :: Int}
             | Goal { goal :: E.GoalTest }
             | World{ 
                    worldSize :: [E.WorldStmnt], 
                    walls :: [E.WorldStmnt], 
                    startPos :: [E.WorldStmnt], 
                    capacity :: [E.WorldStmnt], 
                    finalGoal :: [E.WorldStmnt], 
                    placeIn :: [E.WorldStmnt],
                    placeAt :: [E.WorldStmnt],
                    wBlockId :: Int
                    } 
             | Task{ exprs :: E.ProgPart, tBlockId :: Int }

             deriving(Show,Eq)

-- Symbol type: Useful information about a symbol
data Symbol = Symbol {
                symId      :: String,   --Symbol id
                symType    :: SymType,  --Aditional data depending on the symboltype
                symContext :: Int,      --declaration context
                symPos     :: (Int,Int) --Symbol position in the file
            }

            deriving(Show, Eq)

-- SymbolTable type: useful information for context check
data SymbolTable = SymbolTable{
    symbolMap      :: M.Map String [Symbol],
    contextStack   :: [Int],
    contextCounter :: Int,
    context        :: Context,
    errors         :: [String]
} deriving(Show)

-- Enumerator with all the possible contexts
data Context = NoCon | WorldCon | TaskCon

--Possible errors and its messages
data Error = SymRedef{ redefinedSym :: Symbol }
           | UnmatchContext{ unmatchedSym :: Symbol, currContext :: Context }
           | InconsisWallDir{wallDir :: T.TokPos, wallFromDir :: (T.TokPos,T.TokPos), wallToDir :: (T.TokPos,T.TokPos)}
           | InconsisWallOverWilly{wallPos :: (Int,Int), wallFromWill :: (T.TokPos,T.TokPos), wallToWill :: (T.TokPos,T.TokPos), willPos :: (T.TokPos,T.TokPos)}
           | InvalidWSize{ invRows :: T.TokPos, invCols :: T.TokPos}
           | RedefWSize{ redefWSPos :: T.TokPos }
           | PlaceOutOfBound{ worldbound :: (Int,Int), objPos :: (Int,Int), plcPos::T.TokPos}
           | PlaceZeroAt{placeAtZeroPos :: T.TokPos}
           | UndefRef{ undefId :: T.TokPos }
           | InvalidObjType { invOTId :: T.TokPos }
           | CapacityExceeded { excObId :: T.TokPos, excAmnt :: T.TokPos, excPos :: T.TokPos }
           | UnmatchedType { givId ::  T.TokPos}
           | PlaceZeroObj{ zerrPos :: T.TokPos }
           | RedefStartPos{ redefStPos :: T.TokPos}
           | StartPosOOB{ oobPos :: T.TokPos }
           | WillyOverWall{ wowPos :: T.TokPos }
           | RedefBaskCapacity{ redefBskPos :: T.TokPos }
           | NullBaskCapacity{ nullbskPos :: T.TokPos }
           | GoalOutOfBound{ gGivenPos :: (T.TokPos, T.TokPos), gWorldSize :: (Int, Int) }
           | RedefFGoal{ fgPos :: T.TokPos }
           | NoFinalGoal{ fgworldName :: String }


instance Show Context where
    show NoCon = "Sin contexto"
    show WorldCon = "Contexto World"
    show TaskCon = "Contexto Task"

-- The possible error messages
instance Show Error where
    show (SymRedef m) = "Willy Context Error: Redefinicion de " ++ symId m ++ 
                        "\n   En la línea: " ++ (show . fst . symPos) m ++
                        ", Columna: " ++ (show . snd . symPos) m
    
    show (UnmatchContext sym ctxt) = "Willy Context Error: No se puede definir un símbolo de este tipo en este contexto: " ++
                                     show ctxt ++
                                     "\n   En la línea: " ++ (show . fst . symPos) sym ++
                                     ", Columna: " ++ (show . snd . symPos) sym ++ ", Símbolo: " ++ symId sym
    
    show (InconsisWallDir wd (fx,fy) (tx,ty)) = "Willy Context Error: Declaración de muro inconsistente. \n" ++
                                        "   Direccion incompatible con límites.\n   Direccion: " ++ show (T.tok wd) ++
                                        "\n   Desde: " ++ show (T.tok fx, T.tok fy) ++ 
                                        "\n   Hasta: " ++ show (T.tok tx, T.tok ty) ++
                                        "\n Cerca de linea: " ++ (show . fst . T.pos) fx ++
                                        ", columna: " ++ (show . snd . T.pos) fx

    show (InconsisWallOverWilly wpos (fx,fy) (tx,ty) (wx,wy)) =
                                "Willy Context Error: Declaración de muro inconsistente." ++
                                "\n    Muro sobre posición de Willy: " ++ show (T.tok wx, T.tok wy) ++
                                ", muro: " ++ 
                                "\n   Desde: " ++ show (T.tok fx, T.tok fy) ++ 
                                "\n   Hasta: " ++ show (T.tok tx, T.tok ty) ++
                                "\nError cerca de " ++ posToString wpos

    show (InvalidWSize rows cols) = "Willy Context Error: Declaración de tamaño inválida." ++
                                    "\n   Tamaño del mundo debe ser no nulo. Se encontró:  " ++                                      
                                    show (T.tok rows, T.tok cols) ++
                                    "\n   Cerca de " ++ posToString (T.pos rows)
    
    show (RedefWSize pos) = "Willy Context Error: Declaración de tamaño inválida." ++
                            "\n    Redefinicion del tamaño del mundo cerca de " ++
                            posToString (T.pos pos)
                            
    show (PlaceOutOfBound wsize objpos plpos) = "Willy Context Error: Posicionando objetos fuera de límite."++  
                                                "\n    Tamaño del mundo: " ++ show wsize ++
                                                "\n    Posición del objeto: " ++ show objpos ++
                                                "\nCerca de línea " ++ posToString (T.pos plpos)

    show (PlaceZeroAt pos) = "Willy Context Error: No se pueden posicionar 0 objetos."++  
                             "\nCerca de línea " ++ posToString (T.pos pos)

    show (UndefRef tkid) = "Willy Context Error: Referencia a nombre sin definir."++
                           "\n   Nombre: " ++ T.getId' tkid ++
                           "\nEn " ++ posToString ( T.pos tkid)

    show (InvalidObjType tkid) = "Willy Context Error: El nombre dado no referencia un tipo de objeto."++
                                 "\n    Nombre: " ++ T.getId' tkid ++
                                 "\nEn: " ++ posToString ( T.pos tkid)

    show (CapacityExceeded oid amt pos) = "Willy Context Error: Excedida capacidad de la cesta." ++
                                          "\n   Proveniente de colocar " ++ show (T.getInt' amt) ++
                                          " de " ++ T.getId' oid ++
                                          "\nCerca de " ++ posToString (T.pos pos)

    show (UnmatchedType gid) = "Willy Context Error: El identificador dado no referencia el tipo requerido." ++
                                "\n   Identificador: " ++ show (T.getId' gid) ++
                                "\nEn la " ++ posToString (T.pos gid)

    show (PlaceZeroObj pos)  = "Willy Context Error: No se pueden añadir 0 elementos a la cesta." ++
                                "\nEn la" ++ posToString (T.pos pos)

    show (RedefStartPos pos) = "Willy Context Error: Redefinición de posición inicial." ++
                               "\nCerca de " ++ posToString (T.pos  pos)

    show (StartPosOOB pos) = "Willy Context Error: Posición inicial fuera de los límited del mundo." ++
                             "\nCerca de " ++ posToString (T.pos pos)

    show (WillyOverWall pos) = "Willy Context Error: Willy posicionado sobre un muro." ++
                               "\nCerca de " ++ posToString (T.pos pos)

    show (RedefBaskCapacity pos) = "Willy Context Error: Redefinicion de capacidad de la cesta." ++
                                   "\nCerca de " ++ posToString (T.pos pos)

    show (NullBaskCapacity pos)  = "Willy Context Error: Capacidad de la cesta inválida." ++
                                   "\n    Tamaño dado: " ++ show (T.getInt' pos) ++
                                   "\nCerca de " ++ posToString (T.pos pos)

    show (GoalOutOfBound givpos wsize) = "Willy Context Error: Error en definición de objetivo." ++
                                         "\n    La posición dada no se encuentra dentro de los límites del mundo." ++
                                         "\n    Posición dada: " ++ show (T.getInt' . fst $ givpos, T.getInt' . snd $ givpos) ++ 
                                         "\n    Tamaño del mundo: " ++ show wsize ++
                                         "\nCerca de " ++ posToString (T.pos $ fst givpos)

    show (RedefFGoal pos) = "Willy Context Error: Redefinición de objetivo final" ++
                            "\nEn " ++ posToString (T.pos pos)

    show (NoFinalGoal name) = "Willy Context Error: Sin definición de objetivo final." ++
                              "\n   En el mundo nombrado por: \"" ++ name ++ "\"" 


-- This function returns a formated string with a position in file
posToString :: (Int, Int) -> String
posToString pos = "linea: " ++ (show . fst ) pos ++ 
                  ", columna: " ++ (show . snd ) pos

--Aux SymbolTable functions
-- Given an id and a SymbolTable, returns nothing or the symbol related to such id
findSymbol :: SymbolTable -> String  -> Maybe Symbol
findSymbol st@SymbolTable{contextStack = stk, symbolMap = m} name = case M.lookup name m of
                                                                        Nothing     -> Nothing
                                                                        Just syms   -> maybeMaxBy (compare `on` symContext) (filter (available stk) syms)
    where         
        available :: [Int] -> Symbol -> Bool
        available xs (Symbol _ _ c _) = foldl (\b a -> c==a || b) False xs

        maybeMaxBy ::  (a -> a -> Ordering) -> [a] -> Maybe a
        maybeMaxBy f [] = Nothing
        maybeMaxBy f l = Just $ maximumBy f l

-- Set the current val of a boolean variable to the given one
setVal :: SymbolTable -> String -> Bool -> SymbolTable
setVal st id b = st{symbolMap=newSymMap}
    where
        newVal = (T.boolToTok b, 0,0)

        symMap = symbolMap st

        sym = case findSymbol st id of
                    Just s@Symbol{symType=BoolVar{}} -> s

                    _ -> error $ "Error: esta variable no está disponible en este contexto o no es booleana." ++
                                "\n Variable: " ++ id

        newSym = sym{symType = BoolVar{initVal=newVal}}

        newList = case M.lookup id symMap of
                    Nothing -> error $ "Error: Este símbolo no existe en la tabla: " ++ id
                    Just l  -> replace sym newSym l

        newSymMap = M.insert id newList symMap

        --Aux: replace first occurence of an item in the list
        replace :: (Eq a) => a -> a -> [a] -> [a]
        replace _ _ [] = []
        replace a1 a2 (x:xs) 
            | a1==x     = a2:xs
            | otherwise = a1:replace a1 a2 xs

-- Given a symbolTable, an id, returns the symbolTable with the context related to 
-- this symbol loaded. If the symbol does not exists, returns the same symbol table
loadTask :: SymbolTable -> String -> SymbolTable
loadTask st@SymbolTable{contextStack = stk} id = st{contextStack = newstk}
    where 
        newstk = case findSymbol st id of
                    Nothing -> stk
                    Just sym -> getTbid sym:getTaskWorldbid sym:stk
        getTbid :: Symbol -> Int
        getTbid sym
            | not . isTask . symType $ sym = error $ "The given id is not a valid task: " ++ (show . symId $ sym)
            | otherwise = tBlockId . symType $ sym

        getTaskWorldbid :: Symbol -> Int
        getTaskWorldbid sym
            | not . isTask . symType $ sym = error $ "The given id is not a valid task: " ++ (show . symId $ sym)
            | otherwise = wBlockId . symType . fromJust . findSymbol st . T.getId' . E.workingWorld . exprs . symType $ sym

--Given a symbol table and a block id, push this id as a new context
pushBid :: SymbolTable -> Int -> SymbolTable
pushBid st bid = newst
    where stk = contextStack st
          newStk = bid:stk
          newst = st{contextStack = newStk}

--Given a symbol table, pop the current context
popContext :: SymbolTable -> SymbolTable
popContext st = st{contextStack = tail . contextStack $ st}


-- aux SymTypes functions:
--The following functions can tell is the given 
-- symtype match an specific symtype member
isBool :: SymType -> Bool
isBool BoolVar{} = True
isBool _         = False

isDef :: SymType -> Bool
isDef DefineFunc{} = True
isDef _            = False

isGoal :: SymType -> Bool
isGoal Goal{} = True
isGoal _      = False

isWorld :: SymType -> Bool
isWorld World{} = True
isWorld _       = False

isTask :: SymType -> Bool
isTask Task{} = True
isTask _      = False

isObjType :: SymType -> Bool
isObjType ObjType{} = True
isObjType _ = False

-- Empty constructor for symtypes
emptyWorld :: SymType
emptyWorld = World [] [] [] [] [] [] [] 0
