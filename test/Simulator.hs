-- This module exports functions to show an debug a willy 
-- program 
module Simulator where

import qualified Tokens as T
import qualified Expresions as E
import qualified SymbolTable as ST
import qualified ProgramState as PS
import qualified Interpreter as I
import qualified Data.Map as M
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
        (left, front, right) = (PS.leftClear w, PS.frontClear w, PS.leftClear w)

    in putStrLn $ "left-clear: " ++ show left ++
                  " | front-clear: " ++ show front ++
                  " | right-clear: " ++ show right

-- Summary: Print all the program data
printAll :: PS.ProgramState -> IO()
printAll ps = printWorldMap ps >> printSensors ps
