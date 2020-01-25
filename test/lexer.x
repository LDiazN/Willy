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

    Object\-type                { \ p s -> TkObjectType p}
    
    World                       { \ p s -> TkWorld p}

    Wall                        { \ p s -> TkWall p}

    north                       { \ p s -> TkNorth p}
    south                       { \ p s -> TkSouth p}
    east                        { \ p s -> TkEast p}
    west                        { \ p s -> TkWest p}

    from                        { \ p s -> TkFrom p}
    

    to                          { \ p s -> TkTo p}

    of                          { \ p s -> TkOf p}

    color                       { \ p s -> TkColor p}

    red                         { \ p s -> TkColorRed p}
    blue                        { \ p s -> TkColorBlue p}
    magenta                     { \ p s -> TkColorMagenta p}
    cyan                        { \ p s -> TkColorCyan p}
    green                       { \ p s -> TkColorGreen p}
    yellow                      { \ p s -> TkColorYellow p}
    
    Place                       { \ p s -> TkPlace p}

    basket                      { \ p s -> TkBasket p}
    Basket                      { \ p s -> TkBasketOfCapacity p}

    with                        { \ p s -> TkWith p}
    initial                     { \ p s -> TkInitial p}
    value                       { \ p s -> TkValue p}

    Boolean                     { \ p s -> TkBoolean p}
    true                        { \ p s -> TkTrue p}
    false                       { \ p s -> TkFalse p}

    willy                       { \ p s -> TkWilly p }

    at                          { \ p s -> TkAt p}
    in                          { \ p s -> TkIn p}

    Start                       { \ p s -> TkStart p}

    heading                     { \ p s -> TkHeading p}
    capacity                    { \ p s -> TkCapacity p}

    Goal                        { \ p s -> TkGoalIs p}
    goal                        { \ p s -> TkGoal p}
    is                          { \ p s -> TkIs p}
    Final                       { \ p s -> TkFinal p}

    objects                     { \ p s -> TkObjects p}

    and                         { \ p s -> TkAnd p}
    or                          { \ p s -> TkOr p}
    not                         { \ p s -> TkNot p}

    $num+                       { \ p s -> TkInt p $ read s }
    
    [$alpha\_][$alphaNum\_]*    { \ p s -> TkId p s}




    --$anything+                  {\ p s -> TkUndef p s}



-- Token types:

{
data Token = 
    TkBeginWorld            AlexPosn         |
    TkEndWorld              AlexPosn         |
   
   
    TkWorld                 AlexPosn         |
   
    TkWall                  AlexPosn         |
   
    TkNorth                 AlexPosn         |
    TkSouth                 AlexPosn         |
    TkEast                  AlexPosn         |
    TkWest                  AlexPosn         |

    TkFrom                  AlexPosn         |
    TkTo                    AlexPosn         |

    TkObjectType            AlexPosn         |
    TkOf                    AlexPosn         |

    TkColor                 AlexPosn         |
    TkColorRed              AlexPosn         |
    TkColorBlue             AlexPosn         |
    TkColorMagenta          AlexPosn         |
    TkColorCyan             AlexPosn         |
    TkColorGreen            AlexPosn         |
    TkColorYellow           AlexPosn         |

    TkPlace                 AlexPosn         |
    TkAt                    AlexPosn         |
    TkIn                    AlexPosn         |

    TkBasket                AlexPosn         |
    TkBasketOfCapacity      AlexPosn         |
    TkCapacity              AlexPosn         |

    TkBoolean               AlexPosn         |
    TkTrue                  AlexPosn         |
    TkFalse                 AlexPosn         |
    
    TkWith                  AlexPosn         |
    TkInitial               AlexPosn         |
    TkValue                 AlexPosn         |

    TkHeading               AlexPosn         |
    TkStart                 AlexPosn         |
    
    TkGoalIs                AlexPosn         |
    TkGoal                  AlexPosn         |
    TkIs                    AlexPosn         |
    TkFinal                 AlexPosn         |

    TkObjects               AlexPosn         |
    TkWilly                 AlexPosn         |

    TkAnd                   AlexPosn         |
    TkOr                    AlexPosn         |
    TkNot                   AlexPosn         |

    TkInt                   AlexPosn Integer |

    TkId                    AlexPosn String  |

    TkUndef                 AlexPosn String 


    deriving (Eq, Show)
}