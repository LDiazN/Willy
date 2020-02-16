module Expresions where
import Tokens

data ProgPart = World{ name :: TokPos, properties :: [WorldStmnt]}     
              | Task                                                
              | NoProg
              deriving(Show)


data WorldStmnt = Wall{direction :: TokPos, from :: (TokPos, TokPos), to :: (TokPos, TokPos)}
                | NoWorldStmnt
                deriving(Show)

