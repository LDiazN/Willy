import Control.Monad.State

--main :: IO ()
--main = runStateT code [3..] >> return ()
----
---- layer an infinite list of uniques over the IO monad
----
--
--code :: StateT [Integer] IO ()
--code = do
--    x <- pop
--    io $ print x
--    y <- pop
--    io $ print y
--    z <- pop 
--    io $ print z
--    return ()
--
----
---- pop the next unique off the stack
----
--pop :: StateT [Integer] IO Integer
--pop = do
--    (x:xs) <- get
--    put xs
--    return x
--
--io :: IO a -> StateT [Integer] IO a
--io = liftIO

data S = S{ l :: [Int] , x :: Int } deriving(Show)

main :: IO ()
main = runStateT code (S [] 0) >> return ()

code :: StateT S IO ()
code = do
    xs <- getS
    io $ print xs
    setS [1..5]
    xs' <- getS
    io $ print  xs' 

    s <- getState
    io $ print s
    put $ s{x = 10}
    s' <- getState
    io $ print s'
    return ()

getS :: StateT S IO [Int]
getS = do 
    (S xs x) <- get 
    return xs

setS :: [Int] -> StateT S IO ()
setS xs = do
    (S _ x) <- get
    put $ S xs x
    return ()

io :: IO a -> StateT S IO a
io = liftIO

getState :: StateT S IO S
getState = do 
    s <- get
    return s

setState :: S -> StateT S IO ()
setState ns = do 
    put ns
    return ()

