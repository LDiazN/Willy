-- Luis Diaz: 15-10420
-- Nathalia Silviera: 12-10921 
-- Analizador lexicográfico para el lenguaje "Willy*"
{
    module Lexer where

}

%wrapper "posn"

$alpha = [a-zA-z]
$num = [0-9]
$alphaNum = [a-zA-z0-9]
$anything = ~[]
$white = [\ \t\n\f\v\r]
tokens :-
    $white+                     ;

    begin\-world                { \ p s -> TkBeginWorld p}

    end\-world                  { \ p s -> TkEndWorld p}
    
    World                       { \p s -> TkWorld p}

    Wall                        { \ p s -> TkWall p}

    north                       { \ p s -> TkNorth p}
    south                       { \ p s -> TkSouth p}
    east                        { \ p s -> TkEast p}
    west                        { \ p s -> TkWest p}

    from                        { \ p s -> TkFrom p}
    
    $num+                       { \ p s -> TkInt p $ read s }

    to                          { \ p s -> TkTo p}

    [$alpha\_][$alphaNum\_]*    { \ p s -> TkId p s}

    --$anything+                  {\ p s -> TkUndef p s}




-- Token types:

{
data Token = 
    TkBeginWorld AlexPosn    |
    TkEndWorld AlexPosn      |
   
   
    TkWorld AlexPosn         |
   
    TkWall  AlexPosn         |
   
    TkNorth AlexPosn         |
    TkSouth AlexPosn         |
    TkEast  AlexPosn         |
    TkWest  AlexPosn         |
        
    TkFrom  AlexPosn         |
    TkTo    AlexPosn         |

    TkInt   AlexPosn Integer |

    TkId AlexPosn String     |

    TkUndef AlexPosn String 


    deriving (Eq, Show)
}