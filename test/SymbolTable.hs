module SymbolTable where
import qualified Expresions as E
import qualified Tokens as T


-- The possible symbols:
data SymType = BoolVar | DefineFunc | Goal | World | Task

-- Symbol type

data Symbol = Symbol {
    symId     :: String,
    symPos    :: (Int, Int),
    symType   :: SymType,
    symContext:: Int
}

