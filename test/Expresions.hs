module Expresions where
import Tokens

data ProgPart = World{ worldName :: TokPos, properties :: [WorldStmnt]}     
              | Task { taskName :: TokPos,workingWorld :: TokPos, instructions :: [TaskStmnt] }                                                
              deriving(Show)


data WorldStmnt = Wall{direction :: TokPos, from :: (TokPos, TokPos), to :: (TokPos, TokPos)}
                | WorldSize{ rows :: TokPos, cols :: TokPos }
                | ObjectType{ objectName :: TokPos, color :: TokPos }
                | PlaceAt{ objectTypeIdAt :: TokPos, amountAt :: TokPos, place :: (TokPos, TokPos) }
                | PlaceIn{ objectTypeIdIn :: TokPos, amountIn :: TokPos }
                | StartAt{ initPos :: (TokPos, TokPos), initDirection :: TokPos }
                | BasketCapacity{ capacity :: TokPos }
                | BooleanVar{ boolName :: TokPos, boolVal :: TokPos}
                | Goal{ goalName :: TokPos, goalTest :: GoalTest }
                | FGoal{ finalGoal :: BoolExpr }

                deriving(Show)
                
data TaskStmnt = IfCondition{ ifCondition :: BoolExpr, succInstruction :: TaskStmnt, failInstruction :: TaskStmnt }
               | Move { tokenMove :: TokPos }
               | Repeat { repeatTimes :: TokPos, repInstruction :: TaskStmnt }
               | WhileCond { whileCondition :: BoolExpr, whileIntruct :: TaskStmnt }
               | BeginEnd  { beginPos :: TokPos, beginIntructs :: [TaskStmnt] }
               | DefineFunc { funcName :: TokPos, funcInstruct :: TaskStmnt }
               | TurnLeft { tokenTLeft :: TokPos }
               | TurnRight { tokenTRight :: TokPos }
               | Pick { pickObj :: TokPos }
               | Drop { dropObj :: TokPos }
               | SetOper {varToSet :: TokPos, boolVar :: TokPos}
               | ClearOper {clearPos :: TokPos, varToClear :: TokPos}
               | FlipOper { flipPos :: TokPos, varToFlip :: TokPos}
               | Terminate { terminatePos :: (Int, Int)}
               | Skip
                deriving(Show)


data GoalTest = WillyAt{ willyAtPos :: (TokPos, TokPos) }   
              | WillyBasketObjs{ objIdBask :: TokPos, objAmountInBask :: TokPos }
              | WillyObjectsAt{ objIdAt :: TokPos, objAmountAt :: TokPos, objsPos :: (TokPos, TokPos) }

              deriving(Show)

data BoolExpr = Constant{ consVal :: TokPos } --A Tkid or a constant boolean  
              | Query { queryType :: TokPos, targetName :: TokPos } --query type is a TokPos TkFound or TkCarrying, target is a Tkid
              | NotExpr{ notExpr :: BoolExpr }
              | Operation{ operator :: TokPos, operand1 :: BoolExpr, operan2 :: BoolExpr }

              deriving(Show)