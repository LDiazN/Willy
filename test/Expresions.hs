module Expresions where
import Tokens

data ProgPart = World{ worldName :: TokPos, properties :: [WorldStmnt]}     
              | Task                                                
              | NoProg
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

                deriving(Show)

data GoalTest = WillyAt{ willyAtPos :: (TokPos, TokPos) }   
              | WillyBasketObjs{ objIdBask :: TokPos, objAmountInBask :: TokPos }
              | WillyObjectsAt{ objIdAt :: TokPos, objAmountAt :: TokPos, objsPos :: (TokPos, TokPos) }

              deriving(Show)