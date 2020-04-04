{-# OPTIONS_GHC -w #-}
module Parser where
import Tokens
import Expresions
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t18
	= HappyTerminal (TokPos)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([ProgPart])
	| HappyAbsSyn5 (ProgPart)
	| HappyAbsSyn6 ([WorldStmnt])
	| HappyAbsSyn7 (WorldStmnt)
	| HappyAbsSyn8 (GoalTest)
	| HappyAbsSyn9 ([TaskStmnt])
	| HappyAbsSyn10 (TaskStmnt)
	| HappyAbsSyn11 (TokPos)
	| HappyAbsSyn12 (BoolExpr)
	| HappyAbsSyn17 ((TokPos, TokPos))
	| HappyAbsSyn18 t18

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,416) ([0,4,0,4,0,0,0,16,0,16,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,16,0,0,0,0,4096,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,8248,4433,2,0,2048,0,32992,17732,8,0,8192,0,0,0,0,0,0,0,3584,21576,132,0,0,2,0,0,0,0,0,0,0,0,0,0,0,4,0,60,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,256,0,0,8,0,0,0,0,0,4,0,0,0,0,0,4,0,0,0,0,0,16,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,4096,0,43624,510,2048,0,0,0,1,0,0,0,0,0,4,0,0,0,0,32768,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,4,0,1024,0,0,0,0,0,4096,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,32768,4611,8469,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,16,0,8192,0,0,0,0,0,4096,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,16384,0,0,0,4,0,0,0,0,0,512,0,0,4,0,256,0,0,0,4352,0,1024,0,43674,127,512,0,0,0,0,0,0,0,16384,0,43424,2042,8192,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,8,0,64,0,0,4088,1112,0,0,0,0,0,16,0,1024,0,0,65408,17792,0,4096,0,0,0,0,0,16384,0,43392,2046,0,0,0,0,0,0,32768,0,0,0,0,0,0,2,0,0,0,0,0,8,0,64,0,0,0,0,0,256,0,0,0,0,0,1024,0,0,0,0,0,4096,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,2,0,8,0,0,0,8,0,0,0,0,0,32,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,42496,8186,32768,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,64,0,0,0,256,0,3072,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,32768,33023,69,0,16,0,0,1022,278,0,0,0,16,0,0,0,0,0,8,0,3072,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,43424,2042,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3200,0,0,0,0,0,0,0,4096,0,0,0,4096,1,16384,0,0,0,16384,4,0,0,0,0,32768,0,0,0,16,0,0,0,0,16,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,960,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,4032,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,32768,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,6144,0,0,0,16,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3584,0,0,0,0,0,0,0,4096,0,0,0,4096,1,16384,0,0,0,16384,4,0,0,0,0,0,0,0,4,38912,32682,0,0,0,16,0,0,1022,278,0,64,0,0,4088,1112,0,256,0,60070,31,0,0,0,0,0,0,0,0,0,0,0,0,57344,0,16384,0,43392,2042,0,0,0,1,0,0,0,0,0,4,38912,32682,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,96,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,4,0,0,0,0,0,0,0,0,0,0,4096,0,43616,510,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","willy_prog","prog_part","world_stmts","world_stmt","goalTest","task_stmts","task_stmt","query","wboolExpr","boolExpr","direction","colorVal","boolVal","pos","sc","beginworld","endworld","World","Wall","north","south","east","west","from","to","name","ObjectType","of","color","Place","at","in","basket","Start","heading","Basket","capacity","Boolean","with","initial","value","Goal","goal","is","willy","objects","Final","beginTask","endTask","on","terminate","if","then","else","repeat","times","while","do","define","as","begin","end","move","turnLeft","turnRight","pick","drop","set","clear","flip","frontClear","leftClear","rightClear","lookingNorth","lookingEast","lookingSouth","lookingWest","found","carrying","red","blue","magenta","cyan","green","yellow","int","true","false","';'","'('","')'","and","or","not","%eof"]
        bit_start = st * 98
        bit_end = (st + 1) * 98
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..97]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (19) = happyShift action_3
action_0 (51) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (19) = happyShift action_3
action_1 (51) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (29) = happyShift action_8
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (29) = happyShift action_7
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (19) = happyShift action_3
action_5 (51) = happyShift action_4
action_5 (98) = happyAccept
action_5 (5) = happyGoto action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_2

action_7 (53) = happyShift action_23
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (20) = happyShift action_12
action_8 (21) = happyShift action_13
action_8 (22) = happyShift action_14
action_8 (30) = happyShift action_15
action_8 (33) = happyShift action_16
action_8 (37) = happyShift action_17
action_8 (39) = happyShift action_18
action_8 (41) = happyShift action_19
action_8 (45) = happyShift action_20
action_8 (50) = happyShift action_21
action_8 (92) = happyShift action_22
action_8 (6) = happyGoto action_9
action_8 (7) = happyGoto action_10
action_8 (18) = happyGoto action_11
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (20) = happyShift action_42
action_9 (21) = happyShift action_13
action_9 (22) = happyShift action_14
action_9 (30) = happyShift action_15
action_9 (33) = happyShift action_16
action_9 (37) = happyShift action_17
action_9 (39) = happyShift action_18
action_9 (41) = happyShift action_19
action_9 (45) = happyShift action_20
action_9 (50) = happyShift action_21
action_9 (92) = happyShift action_43
action_9 (7) = happyGoto action_41
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_13

action_11 (20) = happyShift action_39
action_11 (21) = happyShift action_13
action_11 (22) = happyShift action_14
action_11 (30) = happyShift action_15
action_11 (33) = happyShift action_16
action_11 (37) = happyShift action_17
action_11 (39) = happyShift action_18
action_11 (41) = happyShift action_19
action_11 (45) = happyShift action_20
action_11 (50) = happyShift action_21
action_11 (92) = happyShift action_40
action_11 (6) = happyGoto action_38
action_11 (7) = happyGoto action_10
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_5

action_13 (89) = happyShift action_37
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (23) = happyShift action_33
action_14 (24) = happyShift action_34
action_14 (25) = happyShift action_35
action_14 (26) = happyShift action_36
action_14 (14) = happyGoto action_32
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (29) = happyShift action_31
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (89) = happyShift action_30
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (34) = happyShift action_29
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (31) = happyShift action_28
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (29) = happyShift action_27
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (29) = happyShift action_26
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (46) = happyShift action_25
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_83

action_23 (29) = happyShift action_24
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (29) = happyShift action_58
action_24 (52) = happyShift action_59
action_24 (54) = happyShift action_60
action_24 (55) = happyShift action_61
action_24 (58) = happyShift action_62
action_24 (60) = happyShift action_63
action_24 (62) = happyShift action_64
action_24 (64) = happyShift action_65
action_24 (66) = happyShift action_66
action_24 (67) = happyShift action_67
action_24 (68) = happyShift action_68
action_24 (69) = happyShift action_69
action_24 (70) = happyShift action_70
action_24 (71) = happyShift action_71
action_24 (72) = happyShift action_72
action_24 (73) = happyShift action_73
action_24 (92) = happyShift action_22
action_24 (9) = happyGoto action_55
action_24 (10) = happyGoto action_56
action_24 (18) = happyGoto action_57
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (47) = happyShift action_54
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (47) = happyShift action_53
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (42) = happyShift action_52
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (40) = happyShift action_51
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (89) = happyShift action_50
action_29 (17) = happyGoto action_49
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (31) = happyShift action_48
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (31) = happyShift action_47
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (27) = happyShift action_46
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_70

action_34 _ = happyReduce_71

action_35 _ = happyReduce_72

action_36 _ = happyReduce_73

action_37 (89) = happyShift action_45
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (20) = happyShift action_44
action_38 (21) = happyShift action_13
action_38 (22) = happyShift action_14
action_38 (30) = happyShift action_15
action_38 (33) = happyShift action_16
action_38 (37) = happyShift action_17
action_38 (39) = happyShift action_18
action_38 (41) = happyShift action_19
action_38 (45) = happyShift action_20
action_38 (50) = happyShift action_21
action_38 (92) = happyShift action_43
action_38 (7) = happyGoto action_41
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_6

action_40 _ = happyReduce_84

action_41 _ = happyReduce_11

action_42 _ = happyReduce_3

action_43 _ = happyReduce_12

action_44 _ = happyReduce_4

action_45 (92) = happyShift action_124
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (89) = happyShift action_50
action_46 (17) = happyGoto action_123
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (32) = happyShift action_122
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (29) = happyShift action_121
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (38) = happyShift action_120
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (89) = happyShift action_119
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (89) = happyShift action_118
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (43) = happyShift action_117
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (48) = happyShift action_115
action_53 (89) = happyShift action_116
action_53 (8) = happyGoto action_114
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (29) = happyShift action_111
action_54 (93) = happyShift action_112
action_54 (97) = happyShift action_113
action_54 (12) = happyGoto action_110
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (29) = happyShift action_58
action_55 (52) = happyShift action_108
action_55 (54) = happyShift action_60
action_55 (55) = happyShift action_61
action_55 (58) = happyShift action_62
action_55 (60) = happyShift action_63
action_55 (62) = happyShift action_64
action_55 (64) = happyShift action_65
action_55 (66) = happyShift action_66
action_55 (67) = happyShift action_67
action_55 (68) = happyShift action_68
action_55 (69) = happyShift action_69
action_55 (70) = happyShift action_70
action_55 (71) = happyShift action_71
action_55 (72) = happyShift action_72
action_55 (73) = happyShift action_73
action_55 (92) = happyShift action_109
action_55 (10) = happyGoto action_107
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_29

action_57 (29) = happyShift action_58
action_57 (52) = happyShift action_106
action_57 (54) = happyShift action_60
action_57 (55) = happyShift action_61
action_57 (58) = happyShift action_62
action_57 (60) = happyShift action_63
action_57 (62) = happyShift action_64
action_57 (64) = happyShift action_65
action_57 (66) = happyShift action_66
action_57 (67) = happyShift action_67
action_57 (68) = happyShift action_68
action_57 (69) = happyShift action_69
action_57 (70) = happyShift action_70
action_57 (71) = happyShift action_71
action_57 (72) = happyShift action_72
action_57 (73) = happyShift action_73
action_57 (92) = happyShift action_40
action_57 (9) = happyGoto action_105
action_57 (10) = happyGoto action_56
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (92) = happyShift action_104
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_10

action_60 (92) = happyShift action_103
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (29) = happyShift action_87
action_61 (74) = happyShift action_88
action_61 (75) = happyShift action_89
action_61 (76) = happyShift action_90
action_61 (77) = happyShift action_91
action_61 (78) = happyShift action_92
action_61 (79) = happyShift action_93
action_61 (80) = happyShift action_94
action_61 (81) = happyShift action_95
action_61 (82) = happyShift action_96
action_61 (90) = happyShift action_97
action_61 (91) = happyShift action_98
action_61 (93) = happyShift action_99
action_61 (97) = happyShift action_100
action_61 (11) = happyGoto action_85
action_61 (13) = happyGoto action_102
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (89) = happyShift action_101
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (29) = happyShift action_87
action_63 (74) = happyShift action_88
action_63 (75) = happyShift action_89
action_63 (76) = happyShift action_90
action_63 (77) = happyShift action_91
action_63 (78) = happyShift action_92
action_63 (79) = happyShift action_93
action_63 (80) = happyShift action_94
action_63 (81) = happyShift action_95
action_63 (82) = happyShift action_96
action_63 (90) = happyShift action_97
action_63 (91) = happyShift action_98
action_63 (93) = happyShift action_99
action_63 (97) = happyShift action_100
action_63 (11) = happyGoto action_85
action_63 (13) = happyGoto action_86
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (29) = happyShift action_84
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (29) = happyShift action_58
action_65 (54) = happyShift action_60
action_65 (55) = happyShift action_61
action_65 (58) = happyShift action_62
action_65 (60) = happyShift action_63
action_65 (62) = happyShift action_64
action_65 (64) = happyShift action_65
action_65 (65) = happyShift action_83
action_65 (66) = happyShift action_66
action_65 (67) = happyShift action_67
action_65 (68) = happyShift action_68
action_65 (69) = happyShift action_69
action_65 (70) = happyShift action_70
action_65 (71) = happyShift action_71
action_65 (72) = happyShift action_72
action_65 (73) = happyShift action_73
action_65 (9) = happyGoto action_82
action_65 (10) = happyGoto action_56
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (92) = happyShift action_81
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (92) = happyShift action_80
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (92) = happyShift action_79
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (29) = happyShift action_78
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (29) = happyShift action_77
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (29) = happyShift action_76
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (29) = happyShift action_75
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (29) = happyShift action_74
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (92) = happyShift action_162
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (92) = happyShift action_161
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (28) = happyShift action_159
action_76 (92) = happyShift action_160
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (92) = happyShift action_158
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (92) = happyShift action_157
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_39

action_80 _ = happyReduce_38

action_81 _ = happyReduce_37

action_82 (29) = happyShift action_58
action_82 (54) = happyShift action_60
action_82 (55) = happyShift action_61
action_82 (58) = happyShift action_62
action_82 (60) = happyShift action_63
action_82 (62) = happyShift action_64
action_82 (64) = happyShift action_65
action_82 (65) = happyShift action_156
action_82 (66) = happyShift action_66
action_82 (67) = happyShift action_67
action_82 (68) = happyShift action_68
action_82 (69) = happyShift action_69
action_82 (70) = happyShift action_70
action_82 (71) = happyShift action_71
action_82 (72) = happyShift action_72
action_82 (73) = happyShift action_73
action_82 (92) = happyShift action_109
action_82 (10) = happyGoto action_107
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_34

action_84 (63) = happyShift action_155
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (93) = happyShift action_154
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (61) = happyShift action_153
action_86 (95) = happyShift action_148
action_86 (96) = happyShift action_149
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_57

action_88 _ = happyReduce_58

action_89 _ = happyReduce_60

action_90 _ = happyReduce_59

action_91 _ = happyReduce_61

action_92 _ = happyReduce_63

action_93 _ = happyReduce_62

action_94 _ = happyReduce_64

action_95 _ = happyReduce_48

action_96 _ = happyReduce_49

action_97 _ = happyReduce_55

action_98 _ = happyReduce_56

action_99 (29) = happyShift action_87
action_99 (74) = happyShift action_88
action_99 (75) = happyShift action_89
action_99 (76) = happyShift action_90
action_99 (77) = happyShift action_91
action_99 (78) = happyShift action_92
action_99 (79) = happyShift action_93
action_99 (80) = happyShift action_94
action_99 (81) = happyShift action_95
action_99 (82) = happyShift action_96
action_99 (90) = happyShift action_97
action_99 (91) = happyShift action_98
action_99 (93) = happyShift action_99
action_99 (97) = happyShift action_100
action_99 (11) = happyGoto action_85
action_99 (13) = happyGoto action_152
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (29) = happyShift action_87
action_100 (74) = happyShift action_88
action_100 (75) = happyShift action_89
action_100 (76) = happyShift action_90
action_100 (77) = happyShift action_91
action_100 (78) = happyShift action_92
action_100 (79) = happyShift action_93
action_100 (80) = happyShift action_94
action_100 (81) = happyShift action_95
action_100 (82) = happyShift action_96
action_100 (90) = happyShift action_97
action_100 (91) = happyShift action_98
action_100 (93) = happyShift action_99
action_100 (97) = happyShift action_100
action_100 (11) = happyGoto action_85
action_100 (13) = happyGoto action_151
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (59) = happyShift action_150
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (56) = happyShift action_147
action_102 (95) = happyShift action_148
action_102 (96) = happyShift action_149
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_46

action_104 _ = happyReduce_47

action_105 (29) = happyShift action_58
action_105 (52) = happyShift action_146
action_105 (54) = happyShift action_60
action_105 (55) = happyShift action_61
action_105 (58) = happyShift action_62
action_105 (60) = happyShift action_63
action_105 (62) = happyShift action_64
action_105 (64) = happyShift action_65
action_105 (66) = happyShift action_66
action_105 (67) = happyShift action_67
action_105 (68) = happyShift action_68
action_105 (69) = happyShift action_69
action_105 (70) = happyShift action_70
action_105 (71) = happyShift action_71
action_105 (72) = happyShift action_72
action_105 (73) = happyShift action_73
action_105 (92) = happyShift action_109
action_105 (10) = happyGoto action_107
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_9

action_107 _ = happyReduce_27

action_108 _ = happyReduce_8

action_109 _ = happyReduce_28

action_110 (92) = happyShift action_143
action_110 (95) = happyShift action_144
action_110 (96) = happyShift action_145
action_110 _ = happyFail (happyExpListPerState 110)

action_111 _ = happyReduce_50

action_112 (29) = happyShift action_111
action_112 (93) = happyShift action_112
action_112 (97) = happyShift action_113
action_112 (12) = happyGoto action_142
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (29) = happyShift action_111
action_113 (93) = happyShift action_112
action_113 (97) = happyShift action_113
action_113 (12) = happyGoto action_141
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (92) = happyShift action_140
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (47) = happyShift action_139
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (29) = happyShift action_138
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (44) = happyShift action_137
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (92) = happyShift action_136
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_82

action_120 (23) = happyShift action_33
action_120 (24) = happyShift action_34
action_120 (25) = happyShift action_35
action_120 (26) = happyShift action_36
action_120 (14) = happyGoto action_135
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (34) = happyShift action_133
action_121 (35) = happyShift action_134
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (83) = happyShift action_127
action_122 (84) = happyShift action_128
action_122 (85) = happyShift action_129
action_122 (86) = happyShift action_130
action_122 (87) = happyShift action_131
action_122 (88) = happyShift action_132
action_122 (15) = happyGoto action_126
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (28) = happyShift action_125
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_15

action_125 (89) = happyShift action_50
action_125 (17) = happyGoto action_184
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (92) = happyShift action_183
action_126 _ = happyFail (happyExpListPerState 126)

action_127 _ = happyReduce_74

action_128 _ = happyReduce_75

action_129 _ = happyReduce_76

action_130 _ = happyReduce_77

action_131 _ = happyReduce_78

action_132 _ = happyReduce_79

action_133 (89) = happyShift action_50
action_133 (17) = happyGoto action_182
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (36) = happyShift action_181
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (92) = happyShift action_180
action_135 _ = happyFail (happyExpListPerState 135)

action_136 _ = happyReduce_20

action_137 (90) = happyShift action_164
action_137 (91) = happyShift action_165
action_137 (16) = happyGoto action_179
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (49) = happyShift action_178
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (34) = happyShift action_177
action_139 _ = happyFail (happyExpListPerState 139)

action_140 _ = happyReduce_22

action_141 _ = happyReduce_53

action_142 (94) = happyShift action_176
action_142 (95) = happyShift action_144
action_142 (96) = happyShift action_145
action_142 _ = happyFail (happyExpListPerState 142)

action_143 _ = happyReduce_23

action_144 (29) = happyShift action_111
action_144 (93) = happyShift action_112
action_144 (97) = happyShift action_113
action_144 (12) = happyGoto action_175
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (29) = happyShift action_111
action_145 (93) = happyShift action_112
action_145 (97) = happyShift action_113
action_145 (12) = happyGoto action_174
action_145 _ = happyFail (happyExpListPerState 145)

action_146 _ = happyReduce_7

action_147 (29) = happyShift action_58
action_147 (54) = happyShift action_60
action_147 (55) = happyShift action_61
action_147 (58) = happyShift action_62
action_147 (60) = happyShift action_63
action_147 (62) = happyShift action_64
action_147 (64) = happyShift action_65
action_147 (66) = happyShift action_66
action_147 (67) = happyShift action_67
action_147 (68) = happyShift action_68
action_147 (69) = happyShift action_69
action_147 (70) = happyShift action_70
action_147 (71) = happyShift action_71
action_147 (72) = happyShift action_72
action_147 (73) = happyShift action_73
action_147 (10) = happyGoto action_173
action_147 _ = happyFail (happyExpListPerState 147)

action_148 (29) = happyShift action_87
action_148 (74) = happyShift action_88
action_148 (75) = happyShift action_89
action_148 (76) = happyShift action_90
action_148 (77) = happyShift action_91
action_148 (78) = happyShift action_92
action_148 (79) = happyShift action_93
action_148 (80) = happyShift action_94
action_148 (81) = happyShift action_95
action_148 (82) = happyShift action_96
action_148 (90) = happyShift action_97
action_148 (91) = happyShift action_98
action_148 (93) = happyShift action_99
action_148 (97) = happyShift action_100
action_148 (11) = happyGoto action_85
action_148 (13) = happyGoto action_172
action_148 _ = happyFail (happyExpListPerState 148)

action_149 (29) = happyShift action_87
action_149 (74) = happyShift action_88
action_149 (75) = happyShift action_89
action_149 (76) = happyShift action_90
action_149 (77) = happyShift action_91
action_149 (78) = happyShift action_92
action_149 (79) = happyShift action_93
action_149 (80) = happyShift action_94
action_149 (81) = happyShift action_95
action_149 (82) = happyShift action_96
action_149 (90) = happyShift action_97
action_149 (91) = happyShift action_98
action_149 (93) = happyShift action_99
action_149 (97) = happyShift action_100
action_149 (11) = happyGoto action_85
action_149 (13) = happyGoto action_171
action_149 _ = happyFail (happyExpListPerState 149)

action_150 (29) = happyShift action_58
action_150 (54) = happyShift action_60
action_150 (55) = happyShift action_61
action_150 (58) = happyShift action_62
action_150 (60) = happyShift action_63
action_150 (62) = happyShift action_64
action_150 (64) = happyShift action_65
action_150 (66) = happyShift action_66
action_150 (67) = happyShift action_67
action_150 (68) = happyShift action_68
action_150 (69) = happyShift action_69
action_150 (70) = happyShift action_70
action_150 (71) = happyShift action_71
action_150 (72) = happyShift action_72
action_150 (73) = happyShift action_73
action_150 (10) = happyGoto action_170
action_150 _ = happyFail (happyExpListPerState 150)

action_151 _ = happyReduce_68

action_152 (94) = happyShift action_169
action_152 (95) = happyShift action_148
action_152 (96) = happyShift action_149
action_152 _ = happyFail (happyExpListPerState 152)

action_153 (29) = happyShift action_58
action_153 (54) = happyShift action_60
action_153 (55) = happyShift action_61
action_153 (58) = happyShift action_62
action_153 (60) = happyShift action_63
action_153 (62) = happyShift action_64
action_153 (64) = happyShift action_65
action_153 (66) = happyShift action_66
action_153 (67) = happyShift action_67
action_153 (68) = happyShift action_68
action_153 (69) = happyShift action_69
action_153 (70) = happyShift action_70
action_153 (71) = happyShift action_71
action_153 (72) = happyShift action_72
action_153 (73) = happyShift action_73
action_153 (10) = happyGoto action_168
action_153 _ = happyFail (happyExpListPerState 153)

action_154 (29) = happyShift action_167
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (29) = happyShift action_58
action_155 (54) = happyShift action_60
action_155 (55) = happyShift action_61
action_155 (58) = happyShift action_62
action_155 (60) = happyShift action_63
action_155 (62) = happyShift action_64
action_155 (64) = happyShift action_65
action_155 (66) = happyShift action_66
action_155 (67) = happyShift action_67
action_155 (68) = happyShift action_68
action_155 (69) = happyShift action_69
action_155 (70) = happyShift action_70
action_155 (71) = happyShift action_71
action_155 (72) = happyShift action_72
action_155 (73) = happyShift action_73
action_155 (10) = happyGoto action_166
action_155 _ = happyFail (happyExpListPerState 155)

action_156 _ = happyReduce_35

action_157 _ = happyReduce_40

action_158 _ = happyReduce_41

action_159 (90) = happyShift action_164
action_159 (91) = happyShift action_165
action_159 (16) = happyGoto action_163
action_159 _ = happyFail (happyExpListPerState 159)

action_160 _ = happyReduce_42

action_161 _ = happyReduce_44

action_162 _ = happyReduce_45

action_163 (92) = happyShift action_194
action_163 _ = happyFail (happyExpListPerState 163)

action_164 _ = happyReduce_80

action_165 _ = happyReduce_81

action_166 _ = happyReduce_36

action_167 (94) = happyShift action_193
action_167 _ = happyFail (happyExpListPerState 167)

action_168 _ = happyReduce_33

action_169 _ = happyReduce_69

action_170 _ = happyReduce_32

action_171 (95) = happyShift action_148
action_171 _ = happyReduce_67

action_172 _ = happyReduce_66

action_173 (57) = happyShift action_192
action_173 _ = happyReduce_30

action_174 (95) = happyShift action_144
action_174 _ = happyReduce_52

action_175 _ = happyReduce_51

action_176 _ = happyReduce_54

action_177 (89) = happyShift action_50
action_177 (17) = happyGoto action_191
action_177 _ = happyFail (happyExpListPerState 177)

action_178 (34) = happyShift action_189
action_178 (35) = happyShift action_190
action_178 _ = happyFail (happyExpListPerState 178)

action_179 (92) = happyShift action_188
action_179 _ = happyFail (happyExpListPerState 179)

action_180 _ = happyReduce_19

action_181 (92) = happyShift action_187
action_181 _ = happyFail (happyExpListPerState 181)

action_182 (92) = happyShift action_186
action_182 _ = happyFail (happyExpListPerState 182)

action_183 _ = happyReduce_16

action_184 (92) = happyShift action_185
action_184 _ = happyFail (happyExpListPerState 184)

action_185 _ = happyReduce_14

action_186 _ = happyReduce_17

action_187 _ = happyReduce_18

action_188 _ = happyReduce_21

action_189 (89) = happyShift action_50
action_189 (17) = happyGoto action_197
action_189 _ = happyFail (happyExpListPerState 189)

action_190 (39) = happyShift action_196
action_190 _ = happyFail (happyExpListPerState 190)

action_191 _ = happyReduce_24

action_192 (29) = happyShift action_58
action_192 (54) = happyShift action_60
action_192 (55) = happyShift action_61
action_192 (58) = happyShift action_62
action_192 (60) = happyShift action_63
action_192 (62) = happyShift action_64
action_192 (64) = happyShift action_65
action_192 (66) = happyShift action_66
action_192 (67) = happyShift action_67
action_192 (68) = happyShift action_68
action_192 (69) = happyShift action_69
action_192 (70) = happyShift action_70
action_192 (71) = happyShift action_71
action_192 (72) = happyShift action_72
action_192 (73) = happyShift action_73
action_192 (10) = happyGoto action_195
action_192 _ = happyFail (happyExpListPerState 192)

action_193 _ = happyReduce_65

action_194 _ = happyReduce_43

action_195 _ = happyReduce_31

action_196 _ = happyReduce_25

action_197 _ = happyReduce_26

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (World happy_var_2 $ reverse happy_var_3
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 5 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (World happy_var_2 $ reverse happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn5
		 (World happy_var_2 []
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 4 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (World happy_var_2 []
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 7 5 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Task happy_var_2 happy_var_4 $ reverse happy_var_6
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 6 5 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	(HappyTerminal happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Task happy_var_2 happy_var_4 $ reverse happy_var_5
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 6 5 happyReduction_9
happyReduction_9 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Task happy_var_2 happy_var_4 []
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 5 5 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyTerminal happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Task happy_var_2 happy_var_4 []
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_2  6 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_2:happy_var_1
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  6 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  6 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happyReduce 7 7 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Wall happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 4 7 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (WorldSize happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 7 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (ObjectType happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 7 7 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (PlaceAt happy_var_4 happy_var_2 happy_var_6
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 7 7 happyReduction_18
happyReduction_18 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (PlaceIn happy_var_4 happy_var_2
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 6 7 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (StartAt happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 5 7 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyTerminal happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (BasketCapacity happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 7 7 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (BooleanVar happy_var_2 happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 5 7 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Goal happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 5 7 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (FGoal happy_var_4 happy_var_1
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 4 8 happyReduction_24
happyReduction_24 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (WillyAt happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 5 8 happyReduction_25
happyReduction_25 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (WillyBasketObjs happy_var_2 happy_var_1
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 5 8 happyReduction_26
happyReduction_26 ((HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (WillyObjectsAt happy_var_2 happy_var_1 happy_var_5
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_2  9 happyReduction_27
happyReduction_27 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_2:happy_var_1
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  9 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  9 happyReduction_29
happyReduction_29 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happyReduce 4 10 happyReduction_30
happyReduction_30 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (IfCondition happy_var_2 happy_var_4 Skip 0
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 6 10 happyReduction_31
happyReduction_31 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (IfCondition happy_var_2 happy_var_4 happy_var_6 0
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 4 10 happyReduction_32
happyReduction_32 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Repeat happy_var_2 happy_var_4 0
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 4 10 happyReduction_33
happyReduction_33 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (WhileCond happy_var_2 happy_var_4 0
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_2  10 happyReduction_34
happyReduction_34 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (BeginEnd happy_var_1 []
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  10 happyReduction_35
happyReduction_35 _
	(HappyAbsSyn9  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (BeginEnd happy_var_1 $ reverse happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happyReduce 4 10 happyReduction_36
happyReduction_36 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (DefineFunc happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_2  10 happyReduction_37
happyReduction_37 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (Move happy_var_1
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_2  10 happyReduction_38
happyReduction_38 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (TurnLeft happy_var_1
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  10 happyReduction_39
happyReduction_39 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (TurnRight happy_var_1
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  10 happyReduction_40
happyReduction_40 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Pick happy_var_2
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  10 happyReduction_41
happyReduction_41 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Drop happy_var_2
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  10 happyReduction_42
happyReduction_42 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn10
		 (SetOper happy_var_2 (TkTrue, 0, 0)
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happyReduce 5 10 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (SetOper happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_3  10 happyReduction_44
happyReduction_44 _
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (ClearOper happy_var_1 happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  10 happyReduction_45
happyReduction_45 _
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (FlipOper happy_var_1 happy_var_2
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  10 happyReduction_46
happyReduction_46 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (Terminate $ pos happy_var_1
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  10 happyReduction_47
happyReduction_47 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (FuncCall happy_var_1
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  11 happyReduction_48
happyReduction_48 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  11 happyReduction_49
happyReduction_49 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  12 happyReduction_50
happyReduction_50 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (Constant happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  12 happyReduction_51
happyReduction_51 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Operation happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  12 happyReduction_52
happyReduction_52 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Operation happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  12 happyReduction_53
happyReduction_53 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (NotExpr happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  12 happyReduction_54
happyReduction_54 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  13 happyReduction_55
happyReduction_55 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (Constant happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  13 happyReduction_56
happyReduction_56 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (Constant happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  13 happyReduction_57
happyReduction_57 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (Constant happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  13 happyReduction_58
happyReduction_58 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (Constant happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  13 happyReduction_59
happyReduction_59 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (Constant happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  13 happyReduction_60
happyReduction_60 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (Constant happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  13 happyReduction_61
happyReduction_61 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (Constant happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  13 happyReduction_62
happyReduction_62 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (Constant happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  13 happyReduction_63
happyReduction_63 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (Constant happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  13 happyReduction_64
happyReduction_64 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (Constant happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happyReduce 4 13 happyReduction_65
happyReduction_65 (_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Query happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_66 = happySpecReduce_3  13 happyReduction_66
happyReduction_66 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Operation happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  13 happyReduction_67
happyReduction_67 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Operation happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  13 happyReduction_68
happyReduction_68 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (NotExpr happy_var_2
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  13 happyReduction_69
happyReduction_69 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  14 happyReduction_70
happyReduction_70 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  14 happyReduction_71
happyReduction_71 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  14 happyReduction_72
happyReduction_72 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  14 happyReduction_73
happyReduction_73 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  15 happyReduction_74
happyReduction_74 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  15 happyReduction_75
happyReduction_75 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  15 happyReduction_76
happyReduction_76 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  15 happyReduction_77
happyReduction_77 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  15 happyReduction_78
happyReduction_78 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  15 happyReduction_79
happyReduction_79 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  16 happyReduction_80
happyReduction_80 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  16 happyReduction_81
happyReduction_81 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_2  17 happyReduction_82
happyReduction_82 (HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 ((happy_var_1, happy_var_2)
	)
happyReduction_82 _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  18 happyReduction_83
happyReduction_83 _
	 =  HappyAbsSyn18
		 ([]
	)

happyReduce_84 = happySpecReduce_2  18 happyReduction_84
happyReduction_84 _
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_84 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 98 98 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	(TkBeginWorld, _, _) -> cont 19;
	(TkEndWorld, _, _) -> cont 20;
	(TkWorld, _, _) -> cont 21;
	(TkWall, _, _) -> cont 22;
	(TkNorth, _, _) -> cont 23;
	(TkSouth, _, _) -> cont 24;
	(TkEast, _, _) -> cont 25;
	(TkWest ,_, _) -> cont 26;
	(TkFrom, _, _) -> cont 27;
	(TkTo, _, _) -> cont 28;
	(TkId _, _, _) -> cont 29;
	(TkObjectType, _, _) -> cont 30;
	(TkOf, _, _) -> cont 31;
	(TkColor, _, _) -> cont 32;
	(TkPlace, _, _) -> cont 33;
	(TkAt, _, _) -> cont 34;
	(TkIn, _, _) -> cont 35;
	(TkBasket, _, _) -> cont 36;
	(TkStart, _, _) -> cont 37;
	(TkHeading, _, _) -> cont 38;
	(TkBasketOfCapacity, _, _) -> cont 39;
	(TkCapacity, _, _) -> cont 40;
	(TkBoolean, _, _) -> cont 41;
	(TkWith, _, _) -> cont 42;
	(TkInitial, _, _) -> cont 43;
	(TkValue, _, _) -> cont 44;
	(TkGoalIs, _, _) -> cont 45;
	(TkGoal, _, _) -> cont 46;
	(TkIs, _, _) -> cont 47;
	(TkWilly, _, _) -> cont 48;
	(TkObjects, _, _) -> cont 49;
	(TkFinal, _, _) -> cont 50;
	(TkBeginTask, _, _) -> cont 51;
	(TkEndTask, _, _) -> cont 52;
	(TkOn, _, _) -> cont 53;
	(TkTerminate, _, _) -> cont 54;
	(TkIf, _, _) -> cont 55;
	(TkThen, _, _) -> cont 56;
	(TkElse, _, _) -> cont 57;
	(TkRepeat, _, _) -> cont 58;
	(TkTimes, _, _) -> cont 59;
	(TkWhile, _, _) -> cont 60;
	(TkDo, _, _) -> cont 61;
	(TkDefine, _, _) -> cont 62;
	(TkAs, _, _) -> cont 63;
	(TkBegin, _, _) -> cont 64;
	(TkEnd, _, _) -> cont 65;
	(TkMove, _ , _) -> cont 66;
	(TkTurnLeft, _, _) -> cont 67;
	(TkTurnRight, _, _) -> cont 68;
	(TkPick, _, _) -> cont 69;
	(TkDrop, _, _) -> cont 70;
	(TkSet, _, _) -> cont 71;
	(TkClear, _, _) -> cont 72;
	(TkFlip, _, _) -> cont 73;
	(TkFrontClear, _, _) -> cont 74;
	(TkLeftClear, _, _) -> cont 75;
	(TkRightClear, _, _) -> cont 76;
	(TkLookingNorth, _, _) -> cont 77;
	(TkLookingEast, _, _) -> cont 78;
	(TkLookingSouth, _, _) -> cont 79;
	(TkLookingWest, _, _) -> cont 80;
	(TkFound, _, _) -> cont 81;
	(TkCarrying, _, _) -> cont 82;
	(TkColorRed, _, _) -> cont 83;
	(TkColorBlue, _, _) -> cont 84;
	(TkColorMagenta, _, _) -> cont 85;
	(TkColorCyan, _, _) -> cont 86;
	(TkColorGreen, _, _) -> cont 87;
	(TkColorYellow, _, _) -> cont 88;
	(TkInt _,_,_) -> cont 89;
	(TkTrue, _, _) -> cont 90;
	(TkFalse, _, _) -> cont 91;
	(TkSemiColon, _, _) -> cont 92;
	(TkParOpen, _, _) -> cont 93;
	(TkParClose, _, _) -> cont 94;
	(TkAnd, _, _) -> cont 95;
	(TkOr, _, _) -> cont 96;
	(TkNot, _, _) -> cont 97;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 98 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(TokPos)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- < Required by happy > --
parseError :: [TokPos] -> a
parseError (e:es) = error $ "Unexpected token: " ++ show (tok e)  ++ 
                            "\n At line: "  ++ show ( fst (pos e) )  ++ 
                            ", Column: "  ++ show ( snd (pos e) )
parseError []     = error "Unexpected EOF"


    -- < Parser functions > --
parseClean :: [TokPos] -> [ProgPart]
parseClean = reverse . parse
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}






# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 6 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 6 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 6 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
