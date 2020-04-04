{
module Parser where
import Tokens
import Expresions
}

-- Happy definitions below:

%name parse
%tokentype { TokPos }
%error { parseError }

-- Precedence directives
%nonassoc then
%nonassoc else
%left or
%left and
%nonassoc not

-- Possible tokens:
%token
    ------------------------
    -- World instructions --
    ------------------------

    -- KeyWords -- 
    beginworld              { (TkBeginWorld, _, _) }
    endworld                { (TkEndWorld, _, _) }
    World                   { (TkWorld, _, _) }
    Wall                    { (TkWall, _, _) }
    north                   { (TkNorth, _, _) }
    south                   { (TkSouth, _, _) }
    east                    { (TkEast, _, _) }
    west                    { (TkWest ,_, _) }
    from                    { (TkFrom, _, _) }
    to                      { (TkTo, _, _) }
    name                    { (TkId _, _, _)  }
    ObjectType              { (TkObjectType, _, _) }
    of                      { (TkOf, _, _) }
    color                   { (TkColor, _, _) }
    Place                   { (TkPlace, _, _) }
    at                      { (TkAt, _, _) }
    in                      { (TkIn, _, _) }
    basket                  { (TkBasket, _, _) }
    Start                   { (TkStart, _, _) }
    heading                 { (TkHeading, _, _) }
    Basket                  { (TkBasketOfCapacity, _, _) }
    capacity                { (TkCapacity, _, _) }
    Boolean                 { (TkBoolean, _, _) }
    with                    { (TkWith, _, _) }
    initial                 { (TkInitial, _, _) }
    value                   { (TkValue, _, _) }
    Goal                    { (TkGoalIs, _, _) }
    goal                    { (TkGoal, _, _) }
    is                      { (TkIs, _, _) }
    willy                   { (TkWilly, _, _) }
    objects                 { (TkObjects, _, _) }
    Final                   { (TkFinal, _, _) }


    --------------------------
    -- Program Instructions --
    --------------------------

    -- Keywords --
    beginTask               { (TkBeginTask, _, _) }
    endTask                 { (TkEndTask, _, _) }
    on                      { (TkOn, _, _) }
    terminate               { (TkTerminate, _, _) }
    if                      { (TkIf, _, _) }    
    then                    { (TkThen, _, _) }
    else                    { (TkElse, _, _) }
    repeat                  { (TkRepeat, _, _) }
    times                   { (TkTimes, _, _) }
    while                   { (TkWhile, _, _) }
    do                      { (TkDo, _, _) }
    define                  { (TkDefine, _, _) }
    as                      { (TkAs, _, _) }
    begin                   { (TkBegin, _, _) }
    end                     { (TkEnd, _, _) }
    -- Primitive instructions --
    move                    { (TkMove, _ , _) }
    turnLeft                { (TkTurnLeft, _, _) }
    turnRight               { (TkTurnRight, _, _) }
    pick                    { (TkPick, _, _) }
    drop                    { (TkDrop, _, _) }
    set                     { (TkSet, _, _) }
    clear                   { (TkClear, _, _) }
    flip                    { (TkFlip, _, _) }
    frontClear              { (TkFrontClear, _, _) }
    leftClear               { (TkLeftClear, _, _) }
    rightClear              { (TkRightClear, _, _) }
    lookingNorth            { (TkLookingNorth, _, _) }                  
    lookingEast             { (TkLookingEast, _, _) }                   
    lookingSouth            { (TkLookingSouth, _, _) }                  
    lookingWest             { (TkLookingWest, _, _) }                   
    found                   { (TkFound, _, _) }
    carrying                { (TkCarrying, _, _) }



    --------------------------
    ------- Generics ---------
    --------------------------

    -- Constants --
    red                     { (TkColorRed, _, _) }                      
    blue                    { (TkColorBlue, _, _) }                     
    magenta                 { (TkColorMagenta, _, _) }                  
    cyan                    { (TkColorCyan, _, _) }                     
    green                   { (TkColorGreen, _, _) }                    
    yellow                  { (TkColorYellow, _, _) }                   
    int                     { (TkInt _,_,_)  }
    true                    { (TkTrue, _, _) }
    false                   { (TkFalse, _, _) }


    -- Special Symbols --
    ';'                     { (TkSemiColon, _, _) }
    '('                     { (TkParOpen, _, _) }
    ')'                     { (TkParClose, _, _) }

    -- Operators --
    and                     { (TkAnd, _, _) } 
    or                      { (TkOr, _, _) }
    not                     { (TkNot, _, _) }
%%

-----------------------
-- Production Rules: --
-----------------------

    -- Program definition --

    willy_prog  :: { [ProgPart] }
    willy_prog  : prog_part                                   { [$1] }
                | willy_prog prog_part                        { $2 : $1 }

    prog_part   :: { ProgPart }
    prog_part   : beginworld name  world_stmts endworld       { World $2 $ reverse $3 }
                | beginworld name sc world_stmts endworld     { World $2 $ reverse $4 }
                | beginworld name endworld                    { World $2 [] }
                | beginworld name sc endworld                 { World $2 [] }
                | beginTask name on name sc task_stmts endTask{ Task $2 $4 $ reverse $6 }
                | beginTask name on name task_stmts endTask   { Task $2 $4 $ reverse $5 }
                | beginTask name on name sc endTask           { Task $2 $4 [] }
                | beginTask name on name endTask              { Task $2 $4 [] }


    -- World Creation statements --

    world_stmts :: { [WorldStmnt] }
    world_stmts : world_stmts world_stmt                      { $2:$1 }
                | world_stmts ';'                             { $1 }
                | world_stmt                                  { [$1] }            

    world_stmt  :: { WorldStmnt }
    world_stmt  : Wall direction from pos to pos ';'          { Wall $2 $4 $6 }
                | World int int ';'                           { WorldSize $2 $3 }
                | ObjectType name of color colorVal ';'       { ObjectType $2 $5 }
                | Place int of name at pos ';'                { PlaceAt $4 $2 $6 }
                | Place int of name in basket ';'             { PlaceIn $4 $2 }
                | Start at pos heading direction ';'          { StartAt $3 $5 }
                | Basket of capacity int ';'                  { BasketCapacity $4 }
                | Boolean name with initial value boolVal ';' { BooleanVar $2 $6}
                | Goal name is goalTest ';'                   { Goal $2 $4 }
                | Final goal is wboolExpr ';'                 { FGoal $4 $1}


    goalTest    :: { GoalTest }
    goalTest    : willy is at pos                             { WillyAt $4 }
                | int name objects in Basket                  { WillyBasketObjs $2 $1 }
                | int name objects at pos                     { WillyObjectsAt $2 $1 $5 }

    -- Task Creation Statements --
    
    task_stmts  :: { [TaskStmnt] }
    task_stmts  : task_stmts task_stmt                        { $2:$1 }
                | task_stmts ';'                              { $1 } 
                | task_stmt                                   { [$1] }

    task_stmt   :: { TaskStmnt }
                -- Control --
    task_stmt   : if boolExpr then task_stmt                  { IfCondition $2 $4 Skip 0 }
                | if boolExpr then task_stmt else task_stmt   { IfCondition $2 $4 $6 0}
                | repeat int times task_stmt                  { Repeat $2 $4 0}
                | while boolExpr do task_stmt                 { WhileCond $2 $4 0}
                | begin end                                   { BeginEnd $1 [] }
                | begin task_stmts end                        { BeginEnd $1 $ reverse $2 }
                | define name as task_stmt                    { DefineFunc $2 $4 }
                -- Primitive instructions --
                | move ';'                                    { Move $1 }
                | turnLeft ';'                                { TurnLeft $1 }
                | turnRight ';'                               { TurnRight $1 }
                | pick name ';'                               { Pick $2 }
                | drop name ';'                               { Drop $2 }
                | set name ';'                                { SetOper $2 (TkTrue, 0, 0) }
                | set name to boolVal ';'                     { SetOper $2 $4 }
                | clear name ';'                              { ClearOper $1 $2 }
                | flip name ';'                               { FlipOper $1 $2 }                    
                | terminate ';'                               { Terminate $ pos $1 }
                | name ';'                                    { FuncCall $1 }


    query       :: { TokPos }
    query       : found                                       { $1 }
                | carrying                                    { $1 } 

    -- Expresions --
    wboolExpr   :: {BoolExpr}                                 
    wboolExpr   : name                                        { Constant $1 }
                | wboolExpr and wboolExpr                     { Operation $2 $1 $3 }
                | wboolExpr or wboolExpr                      { Operation $2 $1 $3 }
                | not wboolExpr                               { NotExpr $2 }
                | '(' wboolExpr ')'                           { $2 }
                
                

    boolExpr    :: {BoolExpr}                                
    boolExpr    : true                                        { Constant $1 }
                | false                                       { Constant $1 }
                | name                                        { Constant $1 }
                | frontClear                                  { Constant $1 }
                | rightClear                                  { Constant $1 }
                | leftClear                                   { Constant $1 }
                | lookingNorth                                { Constant $1 }
                | lookingSouth                                { Constant $1 }
                | lookingEast                                 { Constant $1 }
                | lookingWest                                 { Constant $1 }
                | query '(' name ')'                          { Query $1 $3 }
                | boolExpr and boolExpr                       { Operation $2 $1 $3 }
                | boolExpr or boolExpr                        { Operation $2 $1 $3 }
                | not boolExpr                                { NotExpr $2 }
                | '(' boolExpr ')'                            { $2 }


    -- Constants --

    direction   :: { TokPos }
    direction   : north                                       { $1 }
                | south                                       { $1 }
                | east                                        { $1 }
                | west                                        { $1 }

    colorVal    :: { TokPos }
    colorVal    : red                                         { $1 } 
                | blue                                        { $1 } 
                | magenta                                     { $1 }     
                | cyan                                        { $1 } 
                | green                                       { $1 } 
                | yellow                                      { $1 }     

    boolVal     :: { TokPos }                                 
    boolVal     : true                                        { $1 } 
                | false                                       { $1 } 

    pos         :: { (TokPos, TokPos) }
    pos         : int int                                     { ($1, $2) }

    sc          : ';'                                         { [] }
                | sc ';'                                      { $1 }
    
------------------
-- Haskell code --
------------------
{
    -- < Required by happy > --
parseError :: [TokPos] -> a
parseError (e:es) = error $ "Unexpected token: " ++ show (tok e)  ++ 
                            "\n At line: "  ++ show ( fst (pos e) )  ++ 
                            ", Column: "  ++ show ( snd (pos e) )
parseError []     = error "Unexpected EOF"


    -- < Parser functions > --
parseClean :: [TokPos] -> [ProgPart]
parseClean = reverse . parse

}