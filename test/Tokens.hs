module Tokens where

--Position type: row & column.
type TokPos = (Token ,Int, Int)

--Valid tokens
data Token = 
    TkBeginWorld                    |
    TkEndWorld                      | 
   
    TkWorld                         |
   
    TkWall                          |
   
    TkNorth                         |
    TkSouth                         |
    TkEast                          |
    TkWest                          |

    TkFrom                          |
    TkTo                            |

    TkObjectType                    |
    TkOf                            |

    TkColor                         |
    TkColorRed                      |
    TkColorBlue                     |
    TkColorMagenta                  |
    TkColorCyan                     |
    TkColorGreen                    |
    TkColorYellow                   |

    TkPlace                         |
    TkAt                            |
    TkIn                            |

    TkBasket                        |
    TkBasketOfCapacity              |
    TkCapacity                      |

    TkBoolean                       |
    TkTrue                          |
    TkFalse                         |
    
    TkWith                          |
    TkInitial                       |
    TkValue                         |

    TkHeading                       |
    TkStart                         |
    
    TkGoalIs                        |
    TkGoal                          |
    TkIs                            |
    TkFinal                         |

    TkObjects                       |
    TkWilly                         |

    TkAnd                           |
    TkOr                            |
    TkNot                           |

    TkBeginTask                     |
    TkOn                            |
    TkEndTask                       |

    TkIf                            |
    TkThen                          |
    TkElse                          |

    TkRepeat                        |
    TkTimes                         |

    TkWhile                         |
    TkDo                            |

    TkBegin                         |
    TkEnd                           |

    TkDefine                        |
    TkAs                            |

    TkMove                          |
    TkTurnLeft                      |
    TkTurnRight                     |
    TkPick                          |
    TkDrop                          |
    TkSet                           |
    TkClear                         |
    TkFlip                          |
    TkTerminate                     |

    TkFrontClear                    |
    TkLeftClear                     |
    TkRightClear                    |
    TkLookingNorth                  |
    TkLookingEast                   |
    TkLookingSouth                  |
    TkLookingWest                   |

    TkFound                         |
    TkParOpen                       |    
    TkParClose                      |  
    TkSemiColon                     |  
    TkCarrying                      |
    TkInLineComm                    |
    TkLongComm                      |    
    TkCommOpen                      |
    TkCommClose                     |

    TkInt                   Int     |

    TkId                    String  |

    TkEOF                           |
    TkEndl                          |

    TkUndef                 String 

    deriving (Eq)


instance Show Token where
    show TkBeginWorld             = "TkBeginWorld"
    show TkEndWorld               = "TkEndWorld"
   
   
    show TkWorld                  = "TkWorld"
   
    show TkWall                   = "TkWall"
   
    show TkNorth                  = "north"
    show TkSouth                  = "south"
    show TkEast                   = "east"
    show TkWest                   = "west"

    show TkFrom                   = "TkFrom"
    show TkTo                     = "TkTo"

    show TkObjectType             = "TkObjectType"
    show TkOf                     = "TkOf"

    show TkColor                  = "TkColor"
    show TkColorRed               = "TkColorRed"
    show TkColorBlue              = "TkColorBlue"
    show TkColorMagenta           = "TkColorMagenta"
    show TkColorCyan              = "TkColorCyan"
    show TkColorGreen             = "TkColorGreen"
    show TkColorYellow            = "TkColorYellow"

    show TkPlace                  = "TkPlace"
    show TkAt                     = "TkAt"
    show TkIn                     = "TkIn"

    show TkBasket                 = "TkBasket"
    show TkBasketOfCapacity       = "TkBasketOfCapacity"
    show TkCapacity               = "TkCapacity"

    show TkBoolean                = "TkBoolean"
    show TkTrue                   = "true"
    show TkFalse                  = "false"
    
    show TkWith                   = "TkWith"
    show TkInitial                = "TkInitial"
    show TkValue                  = "TkValue"

    show TkHeading                = "TkHeading"
    show TkStart                  = "TkStart"
    
    show TkGoalIs                 = "TkGoalIs"
    show TkGoal                   = "TkGoal"
    show TkIs                     = "TkIs"
    show TkFinal                  = "TkFinal"

    show TkObjects                = "TkObjects"
    show TkWilly                  = "TkWilly"

    show TkAnd                    = "CONJUNCION"
    show TkOr                     = "DISYUNCION"
    show TkNot                    = "not"

    show TkBeginTask              = "TkBeginTask"
    show TkOn                     = "TkOn"
    show TkEndTask                = "TkEndTask"

    show TkIf                     = "TkIf"
    show TkThen                   = "TkThen"
    show TkElse                   = "TkElse"

    show TkRepeat                 = "TkRepeat"
    show TkTimes                  = "TkTimes"

    show TkWhile                  = "TkWhile"
    show TkDo                     = "TkDo"

    show TkBegin                  = "TkBegin"
    show TkEnd                    = "TkEnd"

    show TkDefine                 = "TkDefine"
    show TkAs                     = "TkAs"

    show TkMove                   = "TkMove"
    show TkTurnLeft               = "TkTurnLeft"
    show TkTurnRight              = "TkTurnRight"
    show TkPick                   = "TkPick"
    show TkDrop                   = "TkDrop"
    show TkSet                    = "TkSet"
    show TkClear                  = "TkClear"
    show TkFlip                   = "TkFlip"
    show TkTerminate              = "TkTerminate"

    show TkFrontClear             = "front-clear"
    show TkLeftClear              = "left-clear"
    show TkRightClear             = "right-clear"
    show TkLookingNorth           = "looking-north"
    show TkLookingEast            = "looking-east"
    show TkLookingSouth           = "looking-south"
    show TkLookingWest            = "looking-west"
    show TkFound                  = "found"

    show TkParOpen                = "TkParOpen"
    show TkParClose               = "TkParClose"
    show TkSemiColon              = "TkSemiColon"
    show TkCarrying               = "carrying"
    show TkInLineComm             = "TkInLineComm"
    show TkLongComm               = "TkLongComm"
    show TkEndl                   = "TkEndl"
    show TkCommOpen               = "TkCommOpen"
    show TkCommClose              = "TkCommClose"
    show TkEOF                    = "TkEOF"

    show (TkUndef s)              = "TkUndef: " ++ s

    show (TkInt i)              = show i

    show (TkId s)               = show s

--  Aux functions:
    --Get the token from a tokpos
tok :: TokPos -> Token
tok (t, _, _) = t

    --Get the position from a tokpos
pos :: TokPos -> (Int, Int)
pos (_, r, c) = (r, c)

    -- Get the int from a TkInt
getInt :: Token -> Int
getInt (TkInt i) = i
getInt _ = -1

getInt' :: TokPos -> Int
getInt' (tkint, _, _) = getInt tkint

    -- Get the String from a tkId
getId :: Token -> String
getId (TkId s) = s
getId t        = error $ "El token recibido no es un tkid: "  ++ show t ++ "."

getId' :: TokPos -> String
getId' = getId . tok