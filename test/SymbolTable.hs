module SymbolTable where
import qualified Expresions as E
import qualified Tokens as T
import qualified Data.Map as M

-- The possible symbols:
data SymType = BoolVar  {initVal :: T.TokPos}
             | ObjType  {objColor :: E.WorldStmnt}
             | DefineFunc { body :: E.TaskStmnt }
             | Goal { goal :: E.GoalTest }
             | World{ worldSize :: [E.WorldStmnt], walls :: [E.WorldStmnt], startPos :: [E.WorldStmnt], capacity :: [E.WorldStmnt], finalGoal :: [E.WorldStmnt], placeIn :: [E.WorldStmnt]} 
             | Task{ exprs :: E.ProgPart }

             deriving(Show,Eq)

-- Symbol type: Useful information about a symbol
data Symbol = Symbol {
    symId      :: String,
    symType    :: SymType,
    symContext :: Int,
    symPos     :: (Int,Int)
}

-- SymbolTable type: useful information for context check
data SymbolTable = SymbolTable{
    symbolMap      :: M.Map String [Symbol],
    contextStack   :: [Int],
    contextCounter :: Int,
    context        :: Context,
    errors         :: [String]
}

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
           | UndefRef{ undefId :: T.TokPos }
           | InvalidObjType { invOTId :: T.TokPos }
           | CapacityExceeded { excObId :: T.TokPos, excAmnt :: T.TokPos, excPos :: T.TokPos }
           | UnmatchedType { givId ::  T.TokPos}
           | PlaceZeroObj{ zerrPos :: T.TokPos }
           | RedefStartPos{ redefStPos :: T.TokPos}
           | StartPosOOB{ oobPos :: T.TokPos }
           | WillyOverWall{ wowPos :: T.TokPos }

instance Show Context where
    show NoCon = "Sin contexto"
    show WorldCon = "Contexto World"
    show TaskCon = "Contexto Task"

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

posToString :: (Int, Int) -> String
posToString pos = "linea: " ++ (show . fst ) pos ++ 
                  ", columna: " ++ (show . snd ) pos

-- aux symTypes functions
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
emptyWorld = World [] [] [] [] [] []
