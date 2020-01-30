import Lexer
import System.IO
import System.Environment
import Tokens

main = do
    (filename:_) <- getArgs

    content <- readFile filename
--    case runAlex content scanRes of
--        Right tks -> do
--            print tks
--        Left s    -> do
--            putStrLn s
--    case runAlex content scanRes of
--        Right tks -> do
--            print tks
--        Left s    -> do
--            putStrLn s    

    case tokenizer content of
        Right tks ->
            putStrLn $ displayTokens tks
        Left s ->
            putStrLn s

 
--
--  runAlex content loop
displayTokens :: [TokPos] -> String
displayTokens toks = 
    let errs = filterErrors toks    -- The errors in the tokens
        closingBraces =             -- This int is used to tell if all the long comments are closed
            foldl (\ count rem -> case rem of
                                    (TkCommOpen,_,_)  -> count + 1
                                    (TkCommClose,_,_) -> count - 1
                                    a                 -> count
            ) 0 toks
        lines = reverse $ lineOrder toks      -- tokens arranged by lines
        
    in
    if null errs  && closingBraces == 0 then
        foldl (\ s l -> s ++ printLine l)  "" lines  
    else
        displayErrors errs closingBraces
        

-- displayErrors:
--  Try to show an error message for each undefined token in the given input
--  Param:
--      toks: undefined token list
--      closed: number of unclosed comments
--  Return:
--      A string with each error mssg
displayErrors :: [TokPos] -> Int -> String
displayErrors toks closed = foldl ( \ s t -> errorLog t ++ s) "" toks ++ 
                            if closed /= 0 then "Willy* Lexer Error: Unexpected EOF inside of comment\n" 
                            else ""
                            


--Param: 
--  toks: list of tokpos with general tokens
--Return :
--  A list of token errors only
filterErrors :: [TokPos] -> [TokPos] 
filterErrors toks = foldl (\ l t -> 
    case t of
        err@(TkUndef s,_,_) -> (err:l)
        (_,_,_)             -> l
    )
    [] toks


-- Generates a formated string from a list of tokens
printLine :: [TokPos] -> String
printLine (t@(_, _, c):xs) = [' ' | i <- [1..c-1] ] ++ showTokPos t ++  foldr ( \ t s -> showTokPos t ++ s) "" xs
-- From a list of tokens, returns a list of list of tokens, which represents
-- the tokens in each line of the input file
lineOrder :: [TokPos] -> [[TokPos]]
lineOrder toks = map reverse $ foldl aux [[]] toks

-- Aux function to help ordering the tokens 
aux :: [[TokPos]] -> TokPos -> [[TokPos]]
aux [[]] t@(TkEndl, _, _) = [[],[t]]
aux [[]] t = [[t]]
aux (x:xs) t = case t of
                endl@(TkEndl, _, _) -> ([]:(t:x):xs)
                tok@(_,_,_)         -> ((tok:x):xs)

-- Retuns a string showing a TokPos object
showTokPos :: TokPos -> String
showTokPos (TkEndl,_,_) = "\n"
showTokPos (TkCommClose,_,_) = ""
showTokPos (TkCommOpen,_,_) = ""
showTokPos (TkInLineComm,_,_) = ""
showTokPos (tok, r, c)  = "[" ++ show tok ++ ", " ++ "r: " ++ show r ++ ", c: " ++ show c ++ "]"

-- errorLog: takes an undefined token and returns an error log string
errorLog :: TokPos -> String
errorLog (TkUndef s, l, c) = "Willy* Lexer Error: Undefined token \"" ++ s ++ "\" \n                    at Line: "
                            ++ show l ++ ", Column: " ++ show c ++ "\n"
errorLog _ = ""
-- Error log
--tokenizer inpt = 
--  --tokenizer content:
--  let loop = do
--
--    token <- alexMonadScan
--
--    if token == TkEOF then
--      return []
--
--    else
--      do
--        tokens <- loop
--        return (token : tokens)
--  in
--  runAlex inpt loop
--