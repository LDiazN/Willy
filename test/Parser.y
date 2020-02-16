{
module Parser where
import Tokens
import Expresions
}

-- Happy definitions below:

%name parse
%tokentype { TokPos }
%error { parseError }


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
    is                      { (TkIs, _, _) }
    willy                   { (TkWilly, _, _) }
    objects                 { (TkObjects, _, _) }

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


    --------------------------
    -- Program Instructions --
    --------------------------
%%

-----------------------
-- Production Rules: --
-----------------------

    -- Program definition --

    willy_prog  :: { [ProgPart] }
    willy_prog  : prog_part                                   { [$1] }
                | willy_prog prog_part                        { $2 : $1 }

    prog_part   :: { ProgPart }
    prog_part   : beginworld name world_stmts endworld        { World $2 $3 }
                | beginworld name endworld                    { World $2 [] }


    -- World Creation statements --

    world_stmts :: { [WorldStmnt] }
    world_stmts : world_stmts ';' world_stmt                  { $3:$1 }
                | world_stmts ';'                             { $1 }
                | world_stmt                                  { [$1] }            

    world_stmt  :: { WorldStmnt }
    world_stmt  : Wall direction from pos to pos              { Wall $2 $4 $6 }
                | World int int                               { WorldSize $2 $3 }
                | ObjectType name of color colorVal           { ObjectType $2 $5 }
                | Place int of name at int int                { PlaceAt $4 $2 ($6, $7) }
                | Place int of name in basket                 { PlaceIn $4 $2 }
                | Start at int int heading direction          { StartAt ($3, $4) $5 }
                | Basket of capacity int                      { BasketCapacity $4 }
                | Boolean name with initial value boolVal     { BooleanVar $2 $6}
                | Goal name is goalTest                       { Goal $2 $4 }
    
    goalTest    :: { GoalTest }
    goalTest    : willy is at pos                             { WillyAt $4 }
                | int name objects in Basket                  { WillyBasketObjs $2 $1 }
                | int name objects at pos                     { WillyObjectsAt $2 $1 $5 }


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
    boolVal     : true                                         { $1 } 
                | false                                        { $1 } 

    pos         :: { (TokPos, TokPos) }
    pos         : int int                                     { ($1, $2) }

------------------
-- Haskell code --
------------------
{
    -- < Required by happy > --
parseError :: [TokPos] -> a
parseError (e:es) = error $ "Unexpected token: " ++ show (tok e)  ++ "\n At line: "  ++ show ( fst (pos e) )  ++ ", Column: "  ++ show ( snd (pos e) )
parseError []     = error "Unknown Error"
}