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

    beginworld              { (TkBeginWorld,_,_) }
    endworld                { (TkEndWorld,_,_) }
    World                   { (TkWorld,_,_) }
    Wall                    { (TkWall,_,_) }
    north                   { (TkNorth,_,_) }
    south                   { (TkSouth,_,_) }
    east                    { (TkEast,_,_) }
    west                    { (TkWest,_,_) }
    from                    { (TkFrom,_,_) }
    to                      { (TkTo,_,_) }
    int                     { (TkInt _,_,_)  }
    name                    { (TkId _, _, _)  }
    ';'                     { (TkSemiColon, _, _) }

    --------------------------
    -- Program Instructions --
    --------------------------
%%

-----------------------
-- Production Rules: --
-----------------------

    willy_prog  :: { [ProgPart] }
    willy_prog  : prog_part                                   { [$1] }
                | willy_prog prog_part                        { $2 : $1 }

    prog_part   :: { ProgPart }
    prog_part   : beginworld name world_stmts endworld        { World $2 $3 }
                | beginworld name endworld                    { World $2 [] }

    world_stmts :: { [WorldStmnt] }
    world_stmts : world_stmts ';' world_stmt                  { $3:$1 }
                | world_stmts ';'                             { $1 }
                | world_stmt                                  { [$1] }
                

    world_stmt  :: { WorldStmnt }
    world_stmt  : Wall direction from pos to pos              { Wall $2 $4 $6 }
                | 
    direction   :: { TokPos }
    direction   : north                                       { $1 }
                | south                                       { $1 }
                | east                                        { $1 }
                | west                                        { $1 }

    pos         :: { (TokPos, TokPos) }
    pos         : int int                                     { ($1, $2) }


------------------
-- Haskell code --
------------------
{
    -- < Required by happy > --
parseError :: [TokPos] -> a
parseError tks = error $ show tks
}