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
   
    show TkNorth                  = "TkNorth"
    show TkSouth                  = "TkSouth"
    show TkEast                   = "TkEast"
    show TkWest                   = "TkWest"

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
    show TkTrue                   = "TkTrue"
    show TkFalse                  = "TkFalse"
    
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

    show TkAnd                    = "TkAnd"
    show TkOr                     = "TkOr"
    show TkNot                    = "TkNot"

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

    show TkFrontClear             = "TkFrontClear"
    show TkLeftClear              = "TkLeftClear"
    show TkRightClear             = "TkRightClear"
    show TkLookingNorth           = "TkLookingNorth"
    show TkLookingEast            = "TkLookingEast"
    show TkLookingSouth           = "TkLookingSouth"
    show TkLookingWest            = "TkLookingWest"

    show TkFound                  = "TkFound"
    show TkParOpen                = "TkParOpen"
    show TkParClose               = "TkParClose"
    show TkCarrying               = "TkCarrying"
    show TkInLineComm             = "TkInLineComm"
    show TkLongComm               = "TkLongComm"
    show TkEndl                   = "TkEndl";
    show TkCommOpen               = "TkCommOpen"
    show TkCommClose              = "TkCommClose"
    show (TkEOF)                  = "TkEOF"

    show (TkUndef s)              = "TkUndef: " ++ s

    show (TkInt i)              = "TkInt " ++ show i

    show (TkId s)               = "TkId " ++ show s

