-- Luis Diaz: 15-10420
-- Nathalia Silviera: 12-10921 
-- Analizador lexicográfico para el lenguaje "Willy*"
{
    module Lexer where

}

%wrapper "posn"
tokens :-
    $white+ ;
    begin\-world { \ p s -> TkBeginWorld p}



-- Token types:

{
data Token = 
    TkBeginWorld AlexPosn 

    deriving (Eq, Show)
}