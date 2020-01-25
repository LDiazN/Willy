import Lexer
import System.IO
import System.Environment

main = do
  (filename:_) <- getArgs
  content <- readFile filename

  print $ show (alexScanTokens content :: [Token]) ++ "."