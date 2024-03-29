module Expresions where
import Tokens

type AST = [ProgPart]

data ProgPart = World{ worldName :: TokPos, properties :: [WorldStmnt]}     
              | Task { taskName :: TokPos, workingWorld :: TokPos, instructions :: [TaskStmnt] } 

              deriving(Eq, Show)

data WorldStmnt = Wall{direction :: TokPos, from :: (TokPos, TokPos), to :: (TokPos, TokPos)}
                | WorldSize{ rows :: TokPos, cols :: TokPos }
                | ObjectType{ objectName :: TokPos, color :: TokPos }
                | PlaceAt{ objectTypeIdAt :: TokPos, amountAt :: TokPos, place :: (TokPos, TokPos) }
                | PlaceIn{ objectTypeIdIn :: TokPos, amountIn :: TokPos }
                | StartAt{ initPos :: (TokPos, TokPos), initDirection :: TokPos }
                | BasketCapacity{ capacity :: TokPos }
                | BooleanVar{ boolName :: TokPos, boolVal :: TokPos}
                | Goal{ goalName :: TokPos, goalTest :: GoalTest }
                | FGoal{ finalGoal :: BoolExpr, fGoalPos :: TokPos }

                deriving(Eq, Show)
                
data TaskStmnt = IfCondition{   ifCondition :: BoolExpr, 
                                succInstruction :: TaskStmnt, 
                                failInstruction :: TaskStmnt,
                                ibid :: Int
                            } 
               | Repeat     { repeatTimes :: TokPos, repInstruction :: TaskStmnt, rbid :: Int }                                  
               | WhileCond  { whileCondition :: BoolExpr, whileIntruct :: TaskStmnt, wbid :: Int }                              
               | BeginEnd   { beginPos :: TokPos, beginIntructs :: [TaskStmnt] }                                    
               | DefineFunc { funcName :: TokPos, funcInstruct :: TaskStmnt }                                       
               | Move       { tokenMove :: TokPos }
               | TurnLeft   { tokenTLeft :: TokPos }
               | TurnRight  { tokenTRight :: TokPos }
               | Pick       { pickObj :: TokPos }  
               | Drop       { dropObj :: TokPos }
               | SetOper    { varToSet :: TokPos, boolVar :: TokPos }
               | ClearOper  { clearPos :: TokPos, varToClear :: TokPos }
               | FlipOper   { flipPos :: TokPos, varToFlip :: TokPos}
               | Terminate  { terminatePos :: (Int, Int)}
               | FuncCall   {funcId :: TokPos}
               | Skip

               deriving (Eq)

data GoalTest = WillyAt{ willyAtPos :: (TokPos, TokPos) }   
              | WillyBasketObjs{ objIdBask :: TokPos, objAmountInBask :: TokPos }
              | WillyObjectsAt{ objIdAt :: TokPos, objAmountAt :: TokPos, objsPos :: (TokPos, TokPos) }

              deriving(Eq,Show)

data BoolExpr = Constant{ consVal :: TokPos } --A Tkid or a constant boolean like true, false, front-clear... 
              | Query { queryType :: TokPos, targetName :: TokPos } --query type is a TokPos TkFound or TkCarrying, target is a Tkid
              | NotExpr{ notExpr :: BoolExpr }
              | Operation{ operator :: TokPos, operand1 :: BoolExpr, operand2 :: BoolExpr }

              deriving(Eq)

-- Show functions:
instance Show BoolExpr where
    show (Constant tok) = case tok of
        (TkId s, _, _)        -> "[Var]" ++ s
        (tk, _, _)            -> show tk
    show (Query qt tn ) = show (tok qt) ++ "(" ++ (getId . tok)  tn ++ ")"
    show (NotExpr ne)   = "not" ++ show ne
    show (Operation oper op1 op2) = show (tok oper) ++ ":\n  lado izquierdo: " ++ show op1 ++ "\n  lado derecho: " ++ show op2

instance Show TaskStmnt where
    show (Move _) = "Move"
    show (TurnLeft _)  = "Turn Left"
    show (TurnRight _) = "Turn Right"
    show (Terminate _) = "Terminate"
    show (Pick (t, _, _)) = "Pick " ++ show t
    show (Drop (t, _, _)) = "Drop " ++ show t
    show (SetOper (t, _, _) (b, _, _)) = "Set " ++ show t ++ show b
    show (ClearOper _ (b, _, _)) = "Set " ++ show b
    show (FlipOper _ (b, _, _)) = "Flip " ++ show b
    show (Repeat rt _ _) = "REPEAT " ++ (show . getInt . tok $ rt) ++ " TIMES:"
    show (WhileCond be _ _) = "WHILE LOOP:\n"  ++
                            "  Condicion:\n" ++
                            unlines ( map ("    " ++ ) (lines $ show be) )
    show (IfCondition be si fi _) = "CONDICIONAL:\n" ++
                                  "  condicion:\n" ++
                                   unlines ( map ("    " ++ ) (lines $ show be) )
    show (FuncCall  fn) = "LLAMADA A FUNCION:\n  " ++ (getId . tok $ fn)
    show (BeginEnd _ b) = show b
    
    show _ = "what"

--------------------------
-- < MODULE FUNCTIONS > --
--------------------------

-- Summary: given an initial context, and an AST,
--          return this AST with the bid setted according to 
--          the context
setContext :: Int -> AST -> AST
setContext _ [] = []
setContext c (x@World{}:xs) = x:setContext (c+1) xs
setContext c (x@Task{instructions = ins}:xs) = 
    let (newc,newinst) = setContext' (c+1) ins
    in x{instructions = newinst}:setContext newc xs



setContext' :: Int -> [TaskStmnt] -> (Int,[TaskStmnt])
setContext' c [] = (c,[])
setContext' c (x:xs) = 
    let
        (c1,newx) = setContextTask c x
        (newc,newxs) = setContext' c1 xs
    in (newc, newx:newxs)


--Returns the new context and the new stmnt
setContextTask :: Int -> TaskStmnt -> (Int,TaskStmnt)
setContextTask c ts@IfCondition{succInstruction=si,failInstruction=fi} = 
    let 
        (c1, newsi) = setContextTask (c+1) si
        (c2, newfi) = setContextTask c1 fi
        newts = ts{ibid=c, succInstruction=newsi, failInstruction=newfi}
    in (c2,newts)

setContextTask c ts@WhileCond{whileIntruct = wi} = 
    let 
        (c1, newwi) = setContextTask (c+1) wi
        newts = ts{wbid=c, whileIntruct=newwi}
    in (c1,newts)

setContextTask c ts@Repeat{repInstruction = ri} = 
    let 
        (c1, newri) = setContextTask (c+1) ri
        newts = ts{rbid=c, repInstruction=newri}
    in (c1,newts)

setContextTask c ts@BeginEnd{beginIntructs = bi} =
    let 
        (c1, newbi) = setContext' c bi
        newts = ts{beginIntructs=newbi}
    in (c1,newts)

setContextTask c ts = (c,ts)