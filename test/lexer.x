-- Luis Diaz: 15-10420
-- Nathalia Silviera: 12-10921 
-- Analizador lexicográfico para el lenguaje "Willy*"
{
    module Lexer where
    import Tokens

}

%wrapper "posn"

$alpha = [a-zA-z]
$num = [0-9]
$alphaNum = [a-zA-z0-9]
$anything = ~$white
$white = [\ \t\n\f\v\r]

--For comments:
$notCurly = [.\n] # \}
@chars            = \} $notCurly*
@commClose = \}\}

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

    begin\-work                 { \ p s -> TkBeginWork p}
    end\-work                   { \ p s -> TkEndWork p}
    on                          { \ p s -> TkOn p}

    if                          { \ p s -> TkIf p}
    then                        { \ p s -> TkThen p}
    else                        { \ p s -> TkElse p}

    repeat                      { \ p s -> TkRepeat p}
    times                       { \ p s -> TkTimes p}
    
    while                       { \ p s -> TkWhile p}
    do                          { \ p s -> TkDo p}

    begin                       { \ p s -> TkBegin p}
    end                         { \ p s -> TkEnd p}
    
    define                      { \ p s -> TkDefine p}
    as                          { \ p s -> TkAs p}

    move                        { \ p s -> TkMove p}
    turn\-left                  { \ p s -> TkTurnLeft p}
    turn\-right                 { \ p s -> TkTurnRight p}
    pick                        { \ p s -> TkPick p}
    drop                        { \ p s -> TkDrop p}
    set                         { \ p s -> TkSet p}
    clear                       { \ p s -> TkClear p}
    flip                        { \ p s -> TkFlip p}
    terminate                   { \ p s -> TkTerminate p}

    front\-clear                { \ p s -> TkFrontClear p}
    left\-clear                 { \ p s -> TkLeftClear p}
    right\-clear                { \ p s -> TkRightClear p}
    looking\-north              { \ p s -> TkLookingNorth p}
    looking\-east               { \ p s -> TkLookingEast p}
    looking\-south              { \ p s -> TkLookingSouth p}
    looking\-west               { \ p s -> TkLookingWest p}

    found                       { \ p s -> TkFound p}
    carrying                    { \ p s -> TkCarrying p}


    \(                          { \ p s -> TkParOpen p}
    \)                          { \ p s -> TkParClose p}

    \-\- ~[]* \n                { \ p s -> TkInLineComm p}

    \{\{ ( ($notCurly)* | \}($notCurly)+ ) \}\}  { \ p s -> TkLongComm p}

    
    

    $num+                       { \ p s -> TkInt p $ read s }
    
    [$alpha\_][$alphaNum\_]*    { \ p s -> TkId p s}




    $anything+                  {\ p s -> TkUndef p s}



-- Token types:

    


}