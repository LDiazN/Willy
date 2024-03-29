-- Luis Diaz: 15-10420
-- Nathalia Silviera: 12-10921 
-- Analizador lexicográfico para el lenguaje "Willy*"
{
module Lexer where
import Tokens

}

%wrapper "monadUserState"

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

    <0> \-\- ~[]* \n                {storeToken TkInLineComm}

    --<0> \{\{ ( ($notCurly)* | \}($notCurly)+ ) \}\}  {storeToken TkLongComm}

    <0> \{\{                         { storeToken TkCommOpen `andBegin` commSt }
    <0> \}\}                         { storeTokenError }
    <commSt> \}\}                    { storeTokenError `andBegin` initSt }
    <commSt> \{\{                    { storeTokenError }
    <commSt> \n                      { storeToken TkEndl}
    <commSt> [^\ \n]                   { skip }


    
    

    --$num+                       {storeToken TkInt $ read }
    
    --[$alpha\_][$alphaNum\_]*    {storeToken TkId s}




    --$anything+                  {storeToken TkUndef}



{

-- states:
initSt :: Int
initSt = 0

-- EOF Definition required by alex

alexEOF :: Alex TokPos
alexEOF = return (TkEOF, 0, 0)

-- user state object definition required by alex:

data AlexUserState = AlexUserState
    {
        lexerErrors    :: [String],
        lexerTokens    :: [TokPos]
    }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    {
        lexerErrors = [],
        lexerTokens = []
    }

--     Setters & Getters

--  lexerErrors:
getLexerErrors :: Alex [String]
getLexerErrors = Alex $ \ st@AlexState {alex_ust = ust} -> Right (st, lexerErrors ust)

setLexerErrors :: [String] -> Alex ()
setLexerErrors ss = Alex $ \ st -> Right ( st{ alex_ust = (alex_ust st){lexerErrors = ss}} , ())


--  lexerTokens:
getLexerTokens :: Alex [TokPos]
getLexerTokens = Alex $ \ st@AlexState {alex_ust = ust} -> Right (st, lexerTokens ust)

setLexerTokens :: [TokPos] -> Alex ()
setLexerTokens ts = Alex $ \ st -> Right ( st{ alex_ust = (alex_ust st){lexerTokens = ts}} , ())

--   Store tokens:
storeToken :: Token -> AlexInput -> Int -> Alex TokPos
storeToken tok  (AlexPn _ r c, _, _, _) _ = return (tok, r, c)

storeTokenId :: AlexInput -> Int -> Alex TokPos
storeTokenId (AlexPn _ r c, _, _, s) _ = return (TkId s, r, c)

storeTokenInt :: AlexInput -> Int -> Alex TokPos
storeTokenInt (AlexPn _ r c, _, _, s) _ = return (TkInt (read s :: Int), r, c)

-- Store Error: In order to store an error, we need to store the error message in the 
--              user state monad, and then return the Tkundefined token
storeTokenError :: AlexInput -> Int -> Alex TokPos
storeTokenError (AlexPn _ r c, _, _, s) _ = do
    let error = "Willy Lexer error in line: " ++ show r ++ ", column: " ++ show c ++ "\n"   ++
                "       Undefined token: " ++ s 
    errors <- getLexerErrors
    _      <- setLexerErrors (error:errors)
    return $ (TkUndef s,r,c)

}