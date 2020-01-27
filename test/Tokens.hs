module Tokens(Tokens) where

--Position type: row & column.
type Pos = (Int, Int)

--Valid tokens
data Token = 
    TkBeginWorld            Pos         |
    TkEndWorld              Pos         |
   
   
    TkWorld                 Pos         |
   
    TkWall                  Pos         |
   
    TkNorth                 Pos         |
    TkSouth                 Pos         |
    TkEast                  Pos         |
    TkWest                  Pos         |

    TkFrom                  Pos         |
    TkTo                    Pos         |

    TkObjectType            Pos         |
    TkOf                    Pos         |

    TkColor                 Pos         |
    TkColorRed              Pos         |
    TkColorBlue             Pos         |
    TkColorMagenta          Pos         |
    TkColorCyan             Pos         |
    TkColorGreen            Pos         |
    TkColorYellow           Pos         |

    TkPlace                 Pos         |
    TkAt                    Pos         |
    TkIn                    Pos         |

    TkBasket                Pos         |
    TkBasketOfCapacity      Pos         |
    TkCapacity              Pos         |

    TkBoolean               Pos         |
    TkTrue                  Pos         |
    TkFalse                 Pos         |
    
    TkWith                  Pos         |
    TkInitial               Pos         |
    TkValue                 Pos         |

    TkHeading               Pos         |
    TkStart                 Pos         |
    
    TkGoalIs                Pos         |
    TkGoal                  Pos         |
    TkIs                    Pos         |
    TkFinal                 Pos         |

    TkObjects               Pos         |
    TkWilly                 Pos         |

    TkAnd                   Pos         |
    TkOr                    Pos         |
    TkNot                   Pos         |

    TkBeginWork             Pos         |
    TkOn                    Pos         |
    TkEndWork               Pos         |

    TkIf                    Pos         |
    TkThen                  Pos         |
    TkElse                  Pos         |

    TkRepeat                Pos         |
    TkTimes                 Pos         |

    TkWhile                 Pos         |
    TkDo                    Pos         |

    TkBegin                 Pos         |
    TkEnd                   Pos         |

    TkDefine                Pos         |
    TkAs                    Pos         |

    TkMove                  Pos         |
    TkTurnLeft              Pos         |
    TkTurnRight             Pos         |
    TkPick                  Pos         |
    TkDrop                  Pos         |
    TkSet                   Pos         |
    TkClear                 Pos         |
    TkFlip                  Pos         |
    TkTerminate             Pos         |

    TkFrontClear            Pos         |
    TkLeftClear             Pos         |
    TkRightClear            Pos         |
    TkLookingNorth          Pos         |
    TkLookingEast           Pos         |
    TkLookingSouth          Pos         |
    TkLookingWest           Pos         |

    TkFound                 Pos         |
    TkParOpen               Pos         |    
    TkParClose              Pos         |    
    TkCarrying              Pos         |
    TkInLineComm            Pos         |
    TkLongComm              Pos         |    

    TkInt                   Pos Integer |

    TkId                    Pos String  |

    TkUndef                 Pos String 


    deriving (Eq)
}

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

    show TkBeginWork              = "TkBeginWork"
    show TkOn                     = "TkOn"
    show TkEndWork                = "TkEndWork"

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

    show (TkInt _ i)              = "TkInt" ++ show i

    show (TkId _ s)                     = "TkId" ++ show s

