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
    let loop = do
        token <- alexMonadScan
        if token == (TkEOF,0,0) then
            return [token]
        else
            do
                tokens <- loop 
                return (token : tokens)
    
    case runAlex content loop of
        Right tks ->
            print tks
        Left s ->
            putStrLn s

 
--
--  runAlex content loop
    



--tokenizer :: String -> Either String [Token]
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