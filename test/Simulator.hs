-- This module exports functions to show and debug a willy 
-- program 
module Simulator where

import qualified Tokens as T
import qualified Expresions as E
import qualified SymbolTable as ST
import qualified ProgramState as PS
import qualified Interpreter as I
import qualified Data.Map as M
import Control.Concurrent

import Data.List
-- Summary: Simple function that shows a program state
printState :: PS.ProgramState -> IO()
printState = print

-- Summary: Display the map
printWorldMap :: PS.ProgramState -> IO()
printWorldMap ps =
    let wmap = PS.worldMap ps
        (sx, sy) = PS.worldSize ps
        (wx, wy) = PS.currPos . PS.willy $ ps
        lookingAt = PS.looking . PS.willy $ ps
        mapGrid = [ [(i,j)| i<-[1..sx] ] | j <- reverse [1..sy]]
        mapRows = [ map (posToChar wmap) list | list <- mapGrid ]

        posToChar :: PS.WorldMap -> (Int,Int) -> Char
        posToChar wm p 
            | (wx, wy) == p = if lookingAt == PS.North
                                then '^'
                              else if lookingAt == PS.South
                                then 'v'
                              else if lookingAt == PS.East
                                then '>'
                              else '<'

            | otherwise = case M.lookup p wm of
                            Nothing                 -> '+'
                            (Just PS.Wall)          -> '|'
                            (Just PS.Items{})       -> 'i'

        interpose :: a -> [a] -> [a]
        interpose _ [] = []
        interpose _ [a] = [a]
        interpose y (x:xs) = x:y:interpose y xs

    in putStrLn . unlines $ [ interpose ' ' r | r <-  mapRows]

-- Summary: Print the sensors status
printSensors :: PS.ProgramState -> IO ()                                                                            
printSensors ps = 
    let
        w = PS.willy ps
        (left, front, right) = (PS.leftClear w, PS.frontClear w, PS.rightClear w)

    in putStrLn $ "left-clear: " ++ show left ++
                  " | front-clear: " ++ show front ++
                  " | right-clear: " ++ show right

-- Summary: Print the willy basket
printBasket :: PS.ProgramState -> IO()
printBasket ps = 
    let
        header = "[CESTA DE WILLY]"
        baskElems = M.elems . PS.basket . PS.willy $ ps
        --Aux: return a formated item
        
        elemList = if null baskElems
                        then "  <cesta vacía>"
                        else unlines . map printItem $ baskElems
    in putStrLn header >>  putStrLn elemList 

-- Summary: Print the objects in the world
printItems :: PS.ProgramState -> IO()
printItems ps = 
    let 
        wmIts = filter (\(k,v) -> isItem v) . M.toList . PS.worldMap $ ps

        posToLists = map (\(k, its) -> (k,M.toList . PS.itemSet $ its)) wmIts
        posToLists' = map (\(p, its) -> (p, map (\ (a,b)-> (a,PS.amount b)) its)) posToLists
        --aux: Tells if an object in the map it's an item set:
        isItem :: PS.Object -> Bool 
        isItem PS.Items{} = True
        isItem _ = False

        --aux: print formatted the content of a (pos,[item])
        printPosItemL :: ((Int,Int), [(String, Int)] ) -> String
        printPosItemL (p, its) = "  -" ++ show p ++ ":\n"
                                ++ (unlines . map (\(a,b) -> "    "++a++": "++show b) $ its)

    in putStrLn "[OBJETOS EN EL MUNDO]" >> ( putStr . unlines $ map printPosItemL  posToLists')


-- Summary: print the state of the boolean variables:
printVars :: PS.ProgramState -> IO()
printVars ps = putStr str
    where
        boolVars =  filter (ST.isBool . ST.symType) . flat . M.elems . ST.symbolMap . PS.symbolTable $ ps
        availVars =  filter avail boolVars
        ctxt = ST.contextStack . PS.symbolTable $ ps
        --Aux: tells if a symbol is available in the current context
        avail :: ST.Symbol -> Bool 
        avail ST.Symbol{ST.symContext=sc} = case find (==sc) ctxt of
                                                    Nothing -> False
                                                    _       -> True
        printVar :: ST.Symbol -> String
        printVar ST.Symbol{ST.symId=id, ST.symType=ST.BoolVar{ST.initVal=b}} = 
            "  -" ++ id ++ ": " ++ (show . T.tok $ b)

        str = "[VARIABLES]\n" ++ (unlines . map printVar $ availVars)

        flat :: [[a]] -> [a]
        flat [] = []
        flat [[]] = []
        flat ((x:xs):xs') = x:flat (xs:xs')
        flat ([]:xs') = flat xs'

--aux function:
printItem :: PS.Item -> String
printItem PS.Item{PS.symbol=sym,PS.amount=n} = "  -" ++ ST.symId sym ++
                                               ": " ++ show n

-- Summary: Print all the program data
printAll :: PS.ProgramState -> IO()
printAll ps = do 
    printSensors ps 
    printBasket ps 
    printItems ps
    printVars ps
    printWorldMap ps
    putStrLn "------------------------------"

-- Summary: Print the program state data after n seconds
printNWait :: Int -> PS.ProgramState -> IO()
printNWait n ps = printAll ps >> threadDelay n

--For a smooth control, print with the given function and then wait
printNWait' :: (PS.ProgramState -> IO()) -> Int -> PS.ProgramState -> IO()
printNWait' f n ps = f ps >> threadDelay n

-- Summary: Print and ask for continue
printNContinue :: PS.ProgramState -> IO()
printNContinue ps = do 
    printAll ps 
    putStrLn "presiona enter para continuar" 
    getLine 
    return ()
