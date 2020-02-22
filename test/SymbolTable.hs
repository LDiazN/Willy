module SymbolTable where
import qualified Expresions as E
import qualified Tokens as T
import qualified Data.Map as M

-- The possible symbols:
data SymType = BoolVar  {initVal :: T.TokPos}
             | DefineFunc { body :: E.TaskStmnt }
             | Goal { goal :: E.GoalTest }
             | World{ worldSize :: [E.WorldStmnt], walls :: [E.WorldStmnt], startPos :: [E.WorldStmnt], capacity :: [E.WorldStmnt], finalGoal :: [E.WorldStmnt]} 
             | Task{ exprs :: E.ProgPart }

             deriving(Show,Eq)

-- Symbol type: Useful information about a symbol
data Symbol = Symbol {
    symId      :: String,
    symType    :: SymType,
    symContext :: Int
}
--
data SymbolTable = SymbolTable{
    symbolMap      :: M.Map String [Symbol],
    contextStack   :: [Int],
    contextCounter :: Int,
    context        :: Context,
    errors         :: [String]
}


data Context = NoCon | WorldCon | TaskCon
