module ContextAnalyzer where

import Control.Monad.State
import Control.Monad
import System.IO
import System.Environment
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
analyzer ast = do
    unless (null ast) $ io $ putStrLn "Hello Context"

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
        available xs (ST.Symbol _ _ c) = foldl (\b a -> c==a || b) False xs

        head' :: [a] -> Maybe a
        head' [] = Nothing
        head' (x:xs) = Just x

--Insert a non existing symbol into the symbol table with the current context
--insertSymbol :: ST.Symbol -> RetState ()
--insertSymbol sym@ST.Symbol(id stype _) = do
--    st@(ST.SymbolTable m stk _ _ )


-- Needed to use IO within State monad context
io :: IO a -> StateT ContextState IO a
io = liftIO