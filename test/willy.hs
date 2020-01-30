-- Luis Diaz: 15-10420
-- Nathalia Silvera: 12-
-- Archivo Main para la entrega 1:
import Lexer
import Tokens
import System.IO
import System.Environment

main = do
    --Intenta recibir input:
    inpt <- getArgs

    let filename = case inpt of
        []    -> do
                putStr "Archivo a interpretar: "
                f <- getLine 
                return f
        (f:_) -> f
    
    putStrLn filename