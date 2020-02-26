-- Luis Diaz: 15-10420
-- Nathalia Silviera: 12-10921 
-- Analizador lexicográfico para el lenguaje "Willy*"

{
module Lexer where
import Tokens
}

%wrapper "monad"

$alpha = [a-zA-z]
$num = [0-9]
$alphaNum = [a-zA-Z0-9]
$anything = ~$white
$white = [\ \t\n\f\v\r]

--For comments:
$notCurly = [.\n] # \}
@chars            = \} $notCurly*
@commClose = \}\}

tokens :-
    <0> \n                          {storeToken TkEndl}
    <0> [$white # \n]+               ;

    <0> begin\-world                {storeToken TkBeginWorld}

    <0> end\-world                  {storeToken TkEndWorld}

    <0> Object\-type                {storeToken TkObjectType}
    
    <0> World                       {storeToken TkWorld}

    <0> Wall                        {storeToken TkWall}

    <0> north                       {storeToken TkNorth}
    <0> south                       {storeToken TkSouth}
    <0> east                        {storeToken TkEast}
    <0> west                        {storeToken TkWest}

    <0> from                        {storeToken TkFrom}
    

    <0> to                          {storeToken TkTo}

    <0> of                          {storeToken TkOf}

    <0> color                       {storeToken TkColor}

    <0> red                         {storeToken TkColorRed}
    <0> blue                        {storeToken TkColorBlue}
    <0> magenta                     {storeToken TkColorMagenta}
    <0> cyan                        {storeToken TkColorCyan}
    <0> green                       {storeToken TkColorGreen}
    <0> yellow                      {storeToken TkColorYellow}
    
    <0> Place                       {storeToken TkPlace}

    <0> basket                      {storeToken TkBasket}
    <0> Basket                      {storeToken TkBasketOfCapacity}

    <0> with                        {storeToken TkWith}
    <0> initial                     {storeToken TkInitial}
    <0> value                       {storeToken TkValue}

    <0> Boolean                     {storeToken TkBoolean}
    <0> true                        {storeToken TkTrue}
    <0> false                       {storeToken TkFalse}

    <0> willy                       {storeToken TkWilly }

    <0> at                          {storeToken TkAt}
    <0> in                          {storeToken TkIn}

    <0> Start                       {storeToken TkStart}

    <0> heading                     {storeToken TkHeading}
    <0> capacity                    {storeToken TkCapacity}

    <0> Goal                        {storeToken TkGoalIs}
    <0> goal                        {storeToken TkGoal}
    <0> is                          {storeToken TkIs}
    <0> Final                       {storeToken TkFinal}

    <0> objects                     {storeToken TkObjects}

    <0> and                         {storeToken TkAnd}
    <0> or                          {storeToken TkOr}
    <0> not                         {storeToken TkNot}

    <0> begin\-task                 {storeToken TkBeginTask}
    <0> end\-task                   {storeToken TkEndTask}
    <0> on                          {storeToken TkOn}

    <0> if                          {storeToken TkIf}
    <0> then                        {storeToken TkThen}
    <0> else                        {storeToken TkElse}

    <0> repeat                      {storeToken TkRepeat}
    <0> times                       {storeToken TkTimes}
    
    <0> while                       {storeToken TkWhile}
    <0> do                          {storeToken TkDo}

    <0> begin                       {storeToken TkBegin}
    <0> end                         {storeToken TkEnd}
    
    <0> define                      {storeToken TkDefine}
    <0> as                          {storeToken TkAs}

    <0> move                        {storeToken TkMove}
    <0> turn\-left                  {storeToken TkTurnLeft}
    <0> turn\-right                 {storeToken TkTurnRight}
    <0> pick                        {storeToken TkPick}
    <0> drop                        {storeToken TkDrop}
    <0> set                         {storeToken TkSet}
    <0> clear                       {storeToken TkClear}
    <0> flip                        {storeToken TkFlip}
    <0> terminate                   {storeToken TkTerminate}

    <0> front\-clear                {storeToken TkFrontClear}
    <0> left\-clear                 {storeToken TkLeftClear}
    <0> right\-clear                {storeToken TkRightClear}
    <0> looking\-north              {storeToken TkLookingNorth}
    <0> looking\-east               {storeToken TkLookingEast}
    <0> looking\-south              {storeToken TkLookingSouth}
    <0> looking\-west               {storeToken TkLookingWest}

    <0> found                       {storeToken TkFound}
    <0> carrying                    {storeToken TkCarrying}


    <0> \(                          {storeToken TkParOpen}
    <0> \)                          {storeToken TkParClose}
    <0> \;                          {storeToken TkSemiColon}

    <0> \-\- ~[]* \n                {storeToken TkInLineComm}

    <0> \{\{                        { storeToken TkCommOpen `andBegin` commSt }
    <0> \}\}                        { storeTokenError }
    <commSt> \n                     { storeToken TkEndl}
    <commSt> \}\}                   { storeToken TkCommClose `andBegin` initSt }
    <commSt> \{\{                   { storeTokenError }
    <commSt> $white                 { skip }
    <commSt> .                      { skip }

    
    

    <0> [$num # $white]+             {storeTokenInt} --numeric
    
    <0> $num+ $alphaNum+            { storeTokenError }
    <0> [$alpha\_][$alphaNum\_]*     {storeTokenId}




    .                                 {storeTokenError}



{

-- states:
initSt :: Int
initSt = 0

-- EOF Definition required by alex

alexEOF :: Alex TokPos
alexEOF = return (TkEOF, 0, 0)

-- Store token functions
storeToken :: Token -> AlexInput -> Int -> Alex TokPos
storeToken token (AlexPn _ r c, _, _, _) _ = return (token,r,c)

storeTokenError :: AlexInput -> Int -> Alex TokPos
storeTokenError (AlexPn _ r c, _, _, s) l = return (TkUndef $ take l s,r,c)

storeTokenInt :: AlexInput -> Int -> Alex TokPos
storeTokenInt (AlexPn _ r c, _, _, s) l = return $ (TkInt $ read $ take l s, r,c)

storeTokenId :: AlexInput -> Int -> Alex TokPos
storeTokenId (AlexPn _ r c, _, _, s) l = return (TkId $ take l s ,r,c)

-- Tokenizer:   returns either a list of token or an error message from a string program

tokenizer :: String -> Either String [TokPos]
tokenizer inpt = do 
    let loop = do
        token <- alexMonadScan
        if token == (TkEOF,0,0) then
            return [token]
        else
            do
                tokens <- loop 
                return (token : tokens)
    
    runAlex inpt loop 
-- Printing functions and token processing:

-- displayTokens
-- param: 
--  toks: A list of tokens given by the tokenizer function
-- Return:
--  A string with the description of the given program tokens
displayTokens :: [TokPos] -> IO Bool
displayTokens toks = 
    let errs = filterErrors toks    -- The errors in the tokens
        closingBraces =             -- This int is used to tell if all the long comments are closed
            foldl (\ count rem -> case rem of
                                    (TkCommOpen,_,_)  -> count + 1
                                    (TkCommClose,_,_) -> count - 1
                                    a                 -> count
            ) 0 toks
        lines = reverse $ lineOrder toks      -- tokens arranged by lines
        
    in
    if null errs  && closingBraces == 0 then do
        putStrLn "Análisis Léxico ok"  
        return True

    else
        do 
            putStrLn (displayErrors errs closingBraces )
            return False
        
        

-- displayErrors:
--  Try to show an error message for each undefined token in the given input
--  Param:
--      toks: undefined token list
--      closed: number of unclosed comments
--  Return:
--      A string with each error mssg
displayErrors :: [TokPos] -> Int -> String
displayErrors toks closed = foldl ( \ s t -> errorLog t ++ s) "" toks ++ 
                            if closed /= 0 then "Willy* Lexer Error: Unexpected EOF inside of comment\n" 
                            else ""
                            

-- filterErrors:
--Param: 
--  toks: list of tokpos with general tokens
--Return :
--  A list of token errors 
filterErrors :: [TokPos] -> [TokPos] 
filterErrors toks = foldl (\ l t -> 
    case t of
        err@(TkUndef s,_,_) -> (err:l)
        (_,_,_)             -> l
    )
    [] toks

-- Remove all aux tokens from a list of tokens
cleanTokens :: [TokPos] -> [TokPos]
cleanTokens toks = filter (\ tk -> case tk of 
                        (TkEndl, _, _)    -> False
                        (TkCommOpen, _, _)    -> False
                        (TkCommClose, _, _)    -> False
                        (TkInLineComm, _, _)    -> False
                        (TkEOF, _, _)     -> False
                        (TkUndef _, _, _) -> False
                        (_, _, _)         -> True
    ) toks


-- Generates a formated string from a list of tokens
printLine :: [TokPos] -> String
printLine (t@(_, _, c):xs) = [' ' | i <- [1..c-1] ] ++ showTokPos t ++  foldr ( \ t s -> showTokPos t ++ s) "" xs
-- From a list of tokens, returns a list of list of tokens, which represents
-- the tokens in each line of the input file
lineOrder :: [TokPos] -> [[TokPos]]
lineOrder toks = map reverse $ foldl aux [[]] toks

-- Aux function to help ordering the tokens 
aux :: [[TokPos]] -> TokPos -> [[TokPos]]
aux [[]] t@(TkEndl, _, _) = [[],[t]]
aux [[]] t = [[t]]
aux (x:xs) t = case t of
                endl@(TkEndl, _, _) -> ([]:(t:x):xs)
                tok@(_,_,_)         -> ((tok:x):xs)

-- Retuns a string showing a TokPos object
showTokPos :: TokPos -> String
showTokPos (TkEndl,_,_) = "\n"
showTokPos (TkCommClose,_,_) = ""
showTokPos (TkCommOpen,_,_) = ""
showTokPos (TkInLineComm,_,_) = ""
showTokPos (TkEOF,_,_) = ""
showTokPos (tok, r, c)  = "[" ++ show tok ++ ", " ++ "l: " ++ show r ++ ", c: " ++ show c ++ "]"

-- errorLog: takes an undefined token and returns an error log string
errorLog :: TokPos -> String
errorLog (TkUndef s, l, c) = "Willy* Lexer Error: Undefined token \"" ++ s ++ "\" \n                    at Line: "
                            ++ show l ++ ", Column: " ++ show c ++ "\n"
errorLog _ = ""
}