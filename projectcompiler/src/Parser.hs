{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import System.Directory ( doesFileExist )

import Control.Monad
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28
	= HappyTerminal (LexemeClass)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,350) ([0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,4,0,0,61440,35272,512,0,8192,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,16384,0,0,50688,12415,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,57344,34816,512,0,57344,34816,512,0,57344,34816,4608,0,57344,34816,512,0,57344,34816,512,0,57344,34816,512,0,57344,34816,512,0,8192,0,0,0,0,0,0,0,0,0,0,0,57344,35272,512,0,0,0,0,0,0,0,0,0,0,50688,9343,0,0,0,0,0,0,49152,8319,0,0,50688,8447,0,0,50688,12415,0,0,0,0,0,0,50688,8447,0,0,50688,8447,0,57344,34816,512,0,57344,34816,512,0,57344,34816,512,0,57344,34816,512,0,57344,34816,512,0,57344,34816,512,0,57344,34816,512,0,57344,34816,512,0,57344,34816,512,0,57344,34816,512,0,57344,34816,512,0,57344,34816,512,0,0,0,0,0,8192,0,0,0,0,0,0,0,57344,34816,512,0,8192,0,0,0,0,0,0,0,0,48,0,0,8192,0,0,0,0,0,1024,0,0,0,16384,0,0,0,0,0,57344,34816,512,0,0,0,128,0,0,8192,0,0,0,50688,12415,0,0,0,512,0,0,49152,8195,0,0,49152,8195,0,0,49152,8195,0,0,49152,8195,0,0,49152,8195,0,0,0,8192,0,0,0,8192,0,0,0,8195,0,0,0,8195,0,0,49664,8319,0,0,49152,8319,0,0,50688,8319,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,256,0,0,0,0,0,0,0,16512,0,8192,0,0,0,0,0,0,0,8192,0,256,0,57344,35272,768,0,0,0,0,0,0,0,2048,0,57344,34816,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50688,8447,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,57344,34816,512,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,512,0,0,0,0,0,8192,0,0,0,0,0,128,0,8192,0,0,0,8192,0,0,0,0,50688,8319,0,0,0,0,0,0,0,2048,0,8192,0,0,0,0,0,128,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,8192,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parserCalc","Prog","Cl","ClassSigBody","ClassSignature","CS","FormalArgs","IDs","ClassBody","Method","MethodRest","MR","StmtBlock","Stmts","Stmt","IfStmt","EStmt","ElifStmt","ElseStmt","AStmt","Typecase","TypeAlternative","LExpr","RExpr","ActualArgs","ActArg","eof","ident","integer_literal","string_literal","class","def","extends","if","elif","else","while","return","typecase","and","or","not","none","'='","'+'","'-'","'*'","'/'","'=='","'>='","'<'","'<='","'>'","'{'","'}'","'('","')'","','","';'","'.'","':'","%eof"]
        bit_start = st * 64
        bit_end = (st + 1) * 64
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..63]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (33) = happyShift action_8
action_2 (6) = happyGoto action_4
action_2 (7) = happyGoto action_5
action_2 (8) = happyGoto action_6
action_2 (16) = happyGoto action_7
action_2 _ = happyReduce_19

action_3 (64) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (56) = happyShift action_28
action_5 (11) = happyGoto action_27
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (35) = happyShift action_26
action_6 _ = happyReduce_5

action_7 (29) = happyShift action_15
action_7 (30) = happyShift action_16
action_7 (31) = happyShift action_17
action_7 (32) = happyShift action_18
action_7 (36) = happyShift action_19
action_7 (39) = happyShift action_20
action_7 (40) = happyShift action_21
action_7 (41) = happyShift action_22
action_7 (44) = happyShift action_23
action_7 (48) = happyShift action_24
action_7 (58) = happyShift action_25
action_7 (17) = happyGoto action_10
action_7 (18) = happyGoto action_11
action_7 (23) = happyGoto action_12
action_7 (25) = happyGoto action_13
action_7 (26) = happyGoto action_14
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (30) = happyShift action_9
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (58) = happyShift action_59
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_20

action_11 (19) = happyGoto action_57
action_11 (20) = happyGoto action_58
action_11 _ = happyReduce_31

action_12 _ = happyReduce_28

action_13 (46) = happyShift action_55
action_13 (63) = happyShift action_56
action_13 (22) = happyGoto action_54
action_13 _ = happyReduce_43

action_14 (42) = happyShift action_41
action_14 (43) = happyShift action_42
action_14 (47) = happyShift action_43
action_14 (48) = happyShift action_44
action_14 (49) = happyShift action_45
action_14 (50) = happyShift action_46
action_14 (51) = happyShift action_47
action_14 (52) = happyShift action_48
action_14 (53) = happyShift action_49
action_14 (54) = happyShift action_50
action_14 (55) = happyShift action_51
action_14 (61) = happyShift action_52
action_14 (62) = happyShift action_53
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_1

action_16 (58) = happyShift action_40
action_16 _ = happyReduce_39

action_17 _ = happyReduce_42

action_18 _ = happyReduce_41

action_19 (30) = happyShift action_16
action_19 (31) = happyShift action_17
action_19 (32) = happyShift action_18
action_19 (44) = happyShift action_23
action_19 (48) = happyShift action_24
action_19 (58) = happyShift action_25
action_19 (25) = happyGoto action_31
action_19 (26) = happyGoto action_39
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (30) = happyShift action_16
action_20 (31) = happyShift action_17
action_20 (32) = happyShift action_18
action_20 (44) = happyShift action_23
action_20 (48) = happyShift action_24
action_20 (58) = happyShift action_25
action_20 (25) = happyGoto action_31
action_20 (26) = happyGoto action_38
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (30) = happyShift action_16
action_21 (31) = happyShift action_17
action_21 (32) = happyShift action_18
action_21 (44) = happyShift action_23
action_21 (48) = happyShift action_24
action_21 (58) = happyShift action_25
action_21 (61) = happyShift action_37
action_21 (25) = happyGoto action_31
action_21 (26) = happyGoto action_36
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (30) = happyShift action_16
action_22 (31) = happyShift action_17
action_22 (32) = happyShift action_18
action_22 (44) = happyShift action_23
action_22 (48) = happyShift action_24
action_22 (58) = happyShift action_25
action_22 (25) = happyGoto action_31
action_22 (26) = happyGoto action_35
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (30) = happyShift action_16
action_23 (31) = happyShift action_17
action_23 (32) = happyShift action_18
action_23 (44) = happyShift action_23
action_23 (48) = happyShift action_24
action_23 (58) = happyShift action_25
action_23 (25) = happyGoto action_31
action_23 (26) = happyGoto action_34
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (30) = happyShift action_16
action_24 (31) = happyShift action_17
action_24 (32) = happyShift action_18
action_24 (44) = happyShift action_23
action_24 (48) = happyShift action_24
action_24 (58) = happyShift action_25
action_24 (25) = happyGoto action_31
action_24 (26) = happyGoto action_33
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (30) = happyShift action_16
action_25 (31) = happyShift action_17
action_25 (32) = happyShift action_18
action_25 (44) = happyShift action_23
action_25 (48) = happyShift action_24
action_25 (58) = happyShift action_25
action_25 (25) = happyGoto action_31
action_25 (26) = happyGoto action_32
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (30) = happyShift action_30
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_4

action_28 (16) = happyGoto action_29
action_28 _ = happyReduce_19

action_29 (30) = happyShift action_16
action_29 (31) = happyShift action_17
action_29 (32) = happyShift action_18
action_29 (36) = happyShift action_19
action_29 (39) = happyShift action_20
action_29 (40) = happyShift action_21
action_29 (41) = happyShift action_22
action_29 (44) = happyShift action_23
action_29 (48) = happyShift action_24
action_29 (58) = happyShift action_25
action_29 (12) = happyGoto action_87
action_29 (17) = happyGoto action_10
action_29 (18) = happyGoto action_11
action_29 (23) = happyGoto action_12
action_29 (25) = happyGoto action_13
action_29 (26) = happyGoto action_14
action_29 _ = happyReduce_13

action_30 _ = happyReduce_6

action_31 _ = happyReduce_43

action_32 (42) = happyShift action_41
action_32 (43) = happyShift action_42
action_32 (47) = happyShift action_43
action_32 (48) = happyShift action_44
action_32 (49) = happyShift action_45
action_32 (50) = happyShift action_46
action_32 (51) = happyShift action_47
action_32 (52) = happyShift action_48
action_32 (53) = happyShift action_49
action_32 (54) = happyShift action_50
action_32 (55) = happyShift action_51
action_32 (59) = happyShift action_86
action_32 (62) = happyShift action_53
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_48

action_34 (47) = happyShift action_43
action_34 (48) = happyShift action_44
action_34 (49) = happyShift action_45
action_34 (50) = happyShift action_46
action_34 (51) = happyShift action_47
action_34 (52) = happyShift action_48
action_34 (53) = happyShift action_49
action_34 (54) = happyShift action_50
action_34 (55) = happyShift action_51
action_34 (62) = happyShift action_53
action_34 _ = happyReduce_57

action_35 (42) = happyShift action_41
action_35 (43) = happyShift action_42
action_35 (47) = happyShift action_43
action_35 (48) = happyShift action_44
action_35 (49) = happyShift action_45
action_35 (50) = happyShift action_46
action_35 (51) = happyShift action_47
action_35 (52) = happyShift action_48
action_35 (53) = happyShift action_49
action_35 (54) = happyShift action_50
action_35 (55) = happyShift action_51
action_35 (56) = happyShift action_85
action_35 (62) = happyShift action_53
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (42) = happyShift action_41
action_36 (43) = happyShift action_42
action_36 (47) = happyShift action_43
action_36 (48) = happyShift action_44
action_36 (49) = happyShift action_45
action_36 (50) = happyShift action_46
action_36 (51) = happyShift action_47
action_36 (52) = happyShift action_48
action_36 (53) = happyShift action_49
action_36 (54) = happyShift action_50
action_36 (55) = happyShift action_51
action_36 (61) = happyShift action_84
action_36 (62) = happyShift action_53
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_26

action_38 (42) = happyShift action_41
action_38 (43) = happyShift action_42
action_38 (47) = happyShift action_43
action_38 (48) = happyShift action_44
action_38 (49) = happyShift action_45
action_38 (50) = happyShift action_46
action_38 (51) = happyShift action_47
action_38 (52) = happyShift action_48
action_38 (53) = happyShift action_49
action_38 (54) = happyShift action_50
action_38 (55) = happyShift action_51
action_38 (56) = happyShift action_82
action_38 (62) = happyShift action_53
action_38 (15) = happyGoto action_83
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (42) = happyShift action_41
action_39 (43) = happyShift action_42
action_39 (47) = happyShift action_43
action_39 (48) = happyShift action_44
action_39 (49) = happyShift action_45
action_39 (50) = happyShift action_46
action_39 (51) = happyShift action_47
action_39 (52) = happyShift action_48
action_39 (53) = happyShift action_49
action_39 (54) = happyShift action_50
action_39 (55) = happyShift action_51
action_39 (56) = happyShift action_82
action_39 (62) = happyShift action_53
action_39 (15) = happyGoto action_81
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (30) = happyShift action_16
action_40 (31) = happyShift action_17
action_40 (32) = happyShift action_18
action_40 (44) = happyShift action_23
action_40 (48) = happyShift action_24
action_40 (58) = happyShift action_25
action_40 (25) = happyGoto action_31
action_40 (26) = happyGoto action_79
action_40 (27) = happyGoto action_80
action_40 _ = happyReduce_60

action_41 (30) = happyShift action_16
action_41 (31) = happyShift action_17
action_41 (32) = happyShift action_18
action_41 (44) = happyShift action_23
action_41 (48) = happyShift action_24
action_41 (58) = happyShift action_25
action_41 (25) = happyGoto action_31
action_41 (26) = happyGoto action_78
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (30) = happyShift action_16
action_42 (31) = happyShift action_17
action_42 (32) = happyShift action_18
action_42 (44) = happyShift action_23
action_42 (48) = happyShift action_24
action_42 (58) = happyShift action_25
action_42 (25) = happyGoto action_31
action_42 (26) = happyGoto action_77
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (30) = happyShift action_16
action_43 (31) = happyShift action_17
action_43 (32) = happyShift action_18
action_43 (44) = happyShift action_23
action_43 (48) = happyShift action_24
action_43 (58) = happyShift action_25
action_43 (25) = happyGoto action_31
action_43 (26) = happyGoto action_76
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (30) = happyShift action_16
action_44 (31) = happyShift action_17
action_44 (32) = happyShift action_18
action_44 (44) = happyShift action_23
action_44 (48) = happyShift action_24
action_44 (58) = happyShift action_25
action_44 (25) = happyGoto action_31
action_44 (26) = happyGoto action_75
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (30) = happyShift action_16
action_45 (31) = happyShift action_17
action_45 (32) = happyShift action_18
action_45 (44) = happyShift action_23
action_45 (48) = happyShift action_24
action_45 (58) = happyShift action_25
action_45 (25) = happyGoto action_31
action_45 (26) = happyGoto action_74
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (30) = happyShift action_16
action_46 (31) = happyShift action_17
action_46 (32) = happyShift action_18
action_46 (44) = happyShift action_23
action_46 (48) = happyShift action_24
action_46 (58) = happyShift action_25
action_46 (25) = happyGoto action_31
action_46 (26) = happyGoto action_73
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (30) = happyShift action_16
action_47 (31) = happyShift action_17
action_47 (32) = happyShift action_18
action_47 (44) = happyShift action_23
action_47 (48) = happyShift action_24
action_47 (58) = happyShift action_25
action_47 (25) = happyGoto action_31
action_47 (26) = happyGoto action_72
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (30) = happyShift action_16
action_48 (31) = happyShift action_17
action_48 (32) = happyShift action_18
action_48 (44) = happyShift action_23
action_48 (48) = happyShift action_24
action_48 (58) = happyShift action_25
action_48 (25) = happyGoto action_31
action_48 (26) = happyGoto action_71
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (30) = happyShift action_16
action_49 (31) = happyShift action_17
action_49 (32) = happyShift action_18
action_49 (44) = happyShift action_23
action_49 (48) = happyShift action_24
action_49 (58) = happyShift action_25
action_49 (25) = happyGoto action_31
action_49 (26) = happyGoto action_70
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (30) = happyShift action_16
action_50 (31) = happyShift action_17
action_50 (32) = happyShift action_18
action_50 (44) = happyShift action_23
action_50 (48) = happyShift action_24
action_50 (58) = happyShift action_25
action_50 (25) = happyGoto action_31
action_50 (26) = happyGoto action_69
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (30) = happyShift action_16
action_51 (31) = happyShift action_17
action_51 (32) = happyShift action_18
action_51 (44) = happyShift action_23
action_51 (48) = happyShift action_24
action_51 (58) = happyShift action_25
action_51 (25) = happyGoto action_31
action_51 (26) = happyGoto action_68
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_25

action_53 (30) = happyShift action_67
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_23

action_55 (30) = happyShift action_16
action_55 (31) = happyShift action_17
action_55 (32) = happyShift action_18
action_55 (44) = happyShift action_23
action_55 (48) = happyShift action_24
action_55 (58) = happyShift action_25
action_55 (25) = happyGoto action_31
action_55 (26) = happyGoto action_66
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (30) = happyShift action_65
action_56 _ = happyFail (happyExpListPerState 56)

action_57 _ = happyReduce_21

action_58 (37) = happyShift action_63
action_58 (38) = happyShift action_64
action_58 (21) = happyGoto action_62
action_58 _ = happyReduce_33

action_59 (30) = happyShift action_61
action_59 (9) = happyGoto action_60
action_59 _ = happyReduce_8

action_60 (59) = happyShift action_102
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (63) = happyShift action_101
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_30

action_63 (30) = happyShift action_16
action_63 (31) = happyShift action_17
action_63 (32) = happyShift action_18
action_63 (44) = happyShift action_23
action_63 (48) = happyShift action_24
action_63 (58) = happyShift action_25
action_63 (25) = happyGoto action_31
action_63 (26) = happyGoto action_100
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (56) = happyShift action_82
action_64 (15) = happyGoto action_99
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (46) = happyShift action_55
action_65 (22) = happyGoto action_98
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (42) = happyShift action_41
action_66 (43) = happyShift action_42
action_66 (47) = happyShift action_43
action_66 (48) = happyShift action_44
action_66 (49) = happyShift action_45
action_66 (50) = happyShift action_46
action_66 (51) = happyShift action_47
action_66 (52) = happyShift action_48
action_66 (53) = happyShift action_49
action_66 (54) = happyShift action_50
action_66 (55) = happyShift action_51
action_66 (61) = happyShift action_97
action_66 (62) = happyShift action_53
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (58) = happyShift action_96
action_67 _ = happyReduce_40

action_68 (47) = happyShift action_43
action_68 (48) = happyShift action_44
action_68 (49) = happyShift action_45
action_68 (50) = happyShift action_46
action_68 (51) = happyFail []
action_68 (52) = happyFail []
action_68 (53) = happyFail []
action_68 (54) = happyFail []
action_68 (55) = happyFail []
action_68 (62) = happyShift action_53
action_68 _ = happyReduce_54

action_69 (47) = happyShift action_43
action_69 (48) = happyShift action_44
action_69 (49) = happyShift action_45
action_69 (50) = happyShift action_46
action_69 (51) = happyFail []
action_69 (52) = happyFail []
action_69 (53) = happyFail []
action_69 (54) = happyFail []
action_69 (55) = happyFail []
action_69 (62) = happyShift action_53
action_69 _ = happyReduce_51

action_70 (47) = happyShift action_43
action_70 (48) = happyShift action_44
action_70 (49) = happyShift action_45
action_70 (50) = happyShift action_46
action_70 (51) = happyFail []
action_70 (52) = happyFail []
action_70 (53) = happyFail []
action_70 (54) = happyFail []
action_70 (55) = happyFail []
action_70 (62) = happyShift action_53
action_70 _ = happyReduce_52

action_71 (47) = happyShift action_43
action_71 (48) = happyShift action_44
action_71 (49) = happyShift action_45
action_71 (50) = happyShift action_46
action_71 (51) = happyFail []
action_71 (52) = happyFail []
action_71 (53) = happyFail []
action_71 (54) = happyFail []
action_71 (55) = happyFail []
action_71 (62) = happyShift action_53
action_71 _ = happyReduce_53

action_72 (47) = happyShift action_43
action_72 (48) = happyShift action_44
action_72 (49) = happyShift action_45
action_72 (50) = happyShift action_46
action_72 (51) = happyFail []
action_72 (52) = happyFail []
action_72 (53) = happyFail []
action_72 (54) = happyFail []
action_72 (55) = happyFail []
action_72 (62) = happyShift action_53
action_72 _ = happyReduce_50

action_73 (62) = happyShift action_53
action_73 _ = happyReduce_47

action_74 (62) = happyShift action_53
action_74 _ = happyReduce_46

action_75 (49) = happyShift action_45
action_75 (50) = happyShift action_46
action_75 (62) = happyShift action_53
action_75 _ = happyReduce_45

action_76 (49) = happyShift action_45
action_76 (50) = happyShift action_46
action_76 (62) = happyShift action_53
action_76 _ = happyReduce_44

action_77 (42) = happyShift action_41
action_77 (47) = happyShift action_43
action_77 (48) = happyShift action_44
action_77 (49) = happyShift action_45
action_77 (50) = happyShift action_46
action_77 (51) = happyShift action_47
action_77 (52) = happyShift action_48
action_77 (53) = happyShift action_49
action_77 (54) = happyShift action_50
action_77 (55) = happyShift action_51
action_77 (62) = happyShift action_53
action_77 _ = happyReduce_56

action_78 (47) = happyShift action_43
action_78 (48) = happyShift action_44
action_78 (49) = happyShift action_45
action_78 (50) = happyShift action_46
action_78 (51) = happyShift action_47
action_78 (52) = happyShift action_48
action_78 (53) = happyShift action_49
action_78 (54) = happyShift action_50
action_78 (55) = happyShift action_51
action_78 (62) = happyShift action_53
action_78 _ = happyReduce_55

action_79 (42) = happyShift action_41
action_79 (43) = happyShift action_42
action_79 (47) = happyShift action_43
action_79 (48) = happyShift action_44
action_79 (49) = happyShift action_45
action_79 (50) = happyShift action_46
action_79 (51) = happyShift action_47
action_79 (52) = happyShift action_48
action_79 (53) = happyShift action_49
action_79 (54) = happyShift action_50
action_79 (55) = happyShift action_51
action_79 (62) = happyShift action_53
action_79 (28) = happyGoto action_95
action_79 _ = happyReduce_62

action_80 (59) = happyShift action_94
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_29

action_82 (16) = happyGoto action_93
action_82 _ = happyReduce_19

action_83 _ = happyReduce_22

action_84 _ = happyReduce_27

action_85 (24) = happyGoto action_92
action_85 _ = happyReduce_37

action_86 _ = happyReduce_49

action_87 (34) = happyShift action_90
action_87 (57) = happyShift action_91
action_87 (13) = happyGoto action_88
action_87 (14) = happyGoto action_89
action_87 _ = happyFail (happyExpListPerState 87)

action_88 _ = happyReduce_14

action_89 (56) = happyShift action_82
action_89 (63) = happyShift action_112
action_89 (15) = happyGoto action_111
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (30) = happyShift action_110
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_12

action_92 (30) = happyShift action_108
action_92 (57) = happyShift action_109
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (30) = happyShift action_16
action_93 (31) = happyShift action_17
action_93 (32) = happyShift action_18
action_93 (36) = happyShift action_19
action_93 (39) = happyShift action_20
action_93 (40) = happyShift action_21
action_93 (41) = happyShift action_22
action_93 (44) = happyShift action_23
action_93 (48) = happyShift action_24
action_93 (57) = happyShift action_107
action_93 (58) = happyShift action_25
action_93 (17) = happyGoto action_10
action_93 (18) = happyGoto action_11
action_93 (23) = happyGoto action_12
action_93 (25) = happyGoto action_13
action_93 (26) = happyGoto action_14
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_59

action_95 (60) = happyShift action_106
action_95 _ = happyReduce_61

action_96 (30) = happyShift action_16
action_96 (31) = happyShift action_17
action_96 (32) = happyShift action_18
action_96 (44) = happyShift action_23
action_96 (48) = happyShift action_24
action_96 (58) = happyShift action_25
action_96 (25) = happyGoto action_31
action_96 (26) = happyGoto action_79
action_96 (27) = happyGoto action_105
action_96 _ = happyReduce_60

action_97 _ = happyReduce_35

action_98 _ = happyReduce_24

action_99 _ = happyReduce_34

action_100 (42) = happyShift action_41
action_100 (43) = happyShift action_42
action_100 (47) = happyShift action_43
action_100 (48) = happyShift action_44
action_100 (49) = happyShift action_45
action_100 (50) = happyShift action_46
action_100 (51) = happyShift action_47
action_100 (52) = happyShift action_48
action_100 (53) = happyShift action_49
action_100 (54) = happyShift action_50
action_100 (55) = happyShift action_51
action_100 (56) = happyShift action_82
action_100 (62) = happyShift action_53
action_100 (15) = happyGoto action_104
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (30) = happyShift action_103
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_7

action_103 (10) = happyGoto action_118
action_103 _ = happyReduce_10

action_104 _ = happyReduce_32

action_105 (59) = happyShift action_117
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (30) = happyShift action_16
action_106 (31) = happyShift action_17
action_106 (32) = happyShift action_18
action_106 (44) = happyShift action_23
action_106 (48) = happyShift action_24
action_106 (58) = happyShift action_25
action_106 (25) = happyGoto action_31
action_106 (26) = happyGoto action_116
action_106 _ = happyFail (happyExpListPerState 106)

action_107 _ = happyReduce_18

action_108 (63) = happyShift action_115
action_108 _ = happyFail (happyExpListPerState 108)

action_109 _ = happyReduce_36

action_110 (58) = happyShift action_114
action_110 _ = happyFail (happyExpListPerState 110)

action_111 _ = happyReduce_15

action_112 (30) = happyShift action_113
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (56) = happyShift action_82
action_113 (15) = happyGoto action_122
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (30) = happyShift action_61
action_114 (9) = happyGoto action_121
action_114 _ = happyReduce_8

action_115 (30) = happyShift action_120
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (42) = happyShift action_41
action_116 (43) = happyShift action_42
action_116 (47) = happyShift action_43
action_116 (48) = happyShift action_44
action_116 (49) = happyShift action_45
action_116 (50) = happyShift action_46
action_116 (51) = happyShift action_47
action_116 (52) = happyShift action_48
action_116 (53) = happyShift action_49
action_116 (54) = happyShift action_50
action_116 (55) = happyShift action_51
action_116 (62) = happyShift action_53
action_116 _ = happyReduce_63

action_117 _ = happyReduce_58

action_118 (60) = happyShift action_119
action_118 _ = happyReduce_9

action_119 (30) = happyShift action_125
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (56) = happyShift action_82
action_120 (15) = happyGoto action_124
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (59) = happyShift action_123
action_121 _ = happyFail (happyExpListPerState 121)

action_122 _ = happyReduce_16

action_123 _ = happyReduce_17

action_124 _ = happyReduce_38

action_125 (63) = happyShift action_126
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (30) = happyShift action_127
action_126 _ = happyFail (happyExpListPerState 126)

action_127 _ = happyReduce_11

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	_
	_
	 =  HappyAbsSyn4
		 (
	)

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 (
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 _
	_
	 =  HappyAbsSyn5
		 (
	)

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 _
	_
	 =  HappyAbsSyn6
		 (
	)

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn7
		 (
	)

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 _
	_
	_
	 =  HappyAbsSyn7
		 (
	)

happyReduce_7 = happyReduce 5 8 happyReduction_7
happyReduction_7 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_0  9 happyReduction_8
happyReduction_8  =  HappyAbsSyn9
		 (
	)

happyReduce_9 = happyReduce 4 9 happyReduction_9
happyReduction_9 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_0  10 happyReduction_10
happyReduction_10  =  HappyAbsSyn10
		 (
	)

happyReduce_11 = happyReduce 5 10 happyReduction_11
happyReduction_11 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 4 11 happyReduction_12
happyReduction_12 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_0  12 happyReduction_13
happyReduction_13  =  HappyAbsSyn12
		 (
	)

happyReduce_14 = happySpecReduce_2  12 happyReduction_14
happyReduction_14 _
	_
	 =  HappyAbsSyn12
		 (
	)

happyReduce_15 = happySpecReduce_2  13 happyReduction_15
happyReduction_15 _
	_
	 =  HappyAbsSyn13
		 (
	)

happyReduce_16 = happyReduce 4 13 happyReduction_16
happyReduction_16 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 5 14 happyReduction_17
happyReduction_17 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_3  15 happyReduction_18
happyReduction_18 _
	_
	_
	 =  HappyAbsSyn15
		 (
	)

happyReduce_19 = happySpecReduce_0  16 happyReduction_19
happyReduction_19  =  HappyAbsSyn16
		 (
	)

happyReduce_20 = happySpecReduce_2  16 happyReduction_20
happyReduction_20 _
	_
	 =  HappyAbsSyn16
		 (
	)

happyReduce_21 = happySpecReduce_2  17 happyReduction_21
happyReduction_21 _
	_
	 =  HappyAbsSyn17
		 (
	)

happyReduce_22 = happySpecReduce_3  17 happyReduction_22
happyReduction_22 _
	_
	_
	 =  HappyAbsSyn17
		 (
	)

happyReduce_23 = happySpecReduce_2  17 happyReduction_23
happyReduction_23 _
	_
	 =  HappyAbsSyn17
		 (
	)

happyReduce_24 = happyReduce 4 17 happyReduction_24
happyReduction_24 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_2  17 happyReduction_25
happyReduction_25 _
	_
	 =  HappyAbsSyn17
		 (
	)

happyReduce_26 = happySpecReduce_2  17 happyReduction_26
happyReduction_26 _
	_
	 =  HappyAbsSyn17
		 (
	)

happyReduce_27 = happySpecReduce_3  17 happyReduction_27
happyReduction_27 _
	_
	_
	 =  HappyAbsSyn17
		 (
	)

happyReduce_28 = happySpecReduce_1  17 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn17
		 (
	)

happyReduce_29 = happySpecReduce_3  18 happyReduction_29
happyReduction_29 _
	_
	_
	 =  HappyAbsSyn18
		 (
	)

happyReduce_30 = happySpecReduce_2  19 happyReduction_30
happyReduction_30 _
	_
	 =  HappyAbsSyn19
		 (
	)

happyReduce_31 = happySpecReduce_0  20 happyReduction_31
happyReduction_31  =  HappyAbsSyn20
		 (
	)

happyReduce_32 = happyReduce 4 20 happyReduction_32
happyReduction_32 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_0  21 happyReduction_33
happyReduction_33  =  HappyAbsSyn21
		 (
	)

happyReduce_34 = happySpecReduce_2  21 happyReduction_34
happyReduction_34 _
	_
	 =  HappyAbsSyn21
		 (
	)

happyReduce_35 = happySpecReduce_3  22 happyReduction_35
happyReduction_35 _
	_
	_
	 =  HappyAbsSyn22
		 (
	)

happyReduce_36 = happyReduce 5 23 happyReduction_36
happyReduction_36 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_0  24 happyReduction_37
happyReduction_37  =  HappyAbsSyn24
		 (
	)

happyReduce_38 = happyReduce 5 24 happyReduction_38
happyReduction_38 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_1  25 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn25
		 (
	)

happyReduce_40 = happySpecReduce_3  25 happyReduction_40
happyReduction_40 _
	_
	_
	 =  HappyAbsSyn25
		 (
	)

happyReduce_41 = happySpecReduce_1  26 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn26
		 (
	)

happyReduce_42 = happySpecReduce_1  26 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn26
		 (
	)

happyReduce_43 = happySpecReduce_1  26 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn26
		 (
	)

happyReduce_44 = happySpecReduce_3  26 happyReduction_44
happyReduction_44 _
	_
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_45 = happySpecReduce_3  26 happyReduction_45
happyReduction_45 _
	_
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_46 = happySpecReduce_3  26 happyReduction_46
happyReduction_46 _
	_
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_47 = happySpecReduce_3  26 happyReduction_47
happyReduction_47 _
	_
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_48 = happySpecReduce_2  26 happyReduction_48
happyReduction_48 _
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_49 = happySpecReduce_3  26 happyReduction_49
happyReduction_49 _
	_
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_50 = happySpecReduce_3  26 happyReduction_50
happyReduction_50 _
	_
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_51 = happySpecReduce_3  26 happyReduction_51
happyReduction_51 _
	_
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_52 = happySpecReduce_3  26 happyReduction_52
happyReduction_52 _
	_
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_53 = happySpecReduce_3  26 happyReduction_53
happyReduction_53 _
	_
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_54 = happySpecReduce_3  26 happyReduction_54
happyReduction_54 _
	_
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_55 = happySpecReduce_3  26 happyReduction_55
happyReduction_55 _
	_
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_56 = happySpecReduce_3  26 happyReduction_56
happyReduction_56 _
	_
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_57 = happySpecReduce_2  26 happyReduction_57
happyReduction_57 _
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_58 = happyReduce 6 26 happyReduction_58
happyReduction_58 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (
	) `HappyStk` happyRest

happyReduce_59 = happyReduce 4 26 happyReduction_59
happyReduction_59 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (
	) `HappyStk` happyRest

happyReduce_60 = happySpecReduce_0  27 happyReduction_60
happyReduction_60  =  HappyAbsSyn27
		 (
	)

happyReduce_61 = happySpecReduce_2  27 happyReduction_61
happyReduction_61 _
	_
	 =  HappyAbsSyn27
		 (
	)

happyReduce_62 = happySpecReduce_0  28 happyReduction_62
happyReduction_62  =  HappyAbsSyn28
		 (
	)

happyReduce_63 = happySpecReduce_3  28 happyReduction_63
happyReduction_63 _
	_
	_
	 =  HappyAbsSyn28
		 (
	)

happyNewToken action sts stk [] =
	action 64 64 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	EOF -> cont 29;
	ID happy_dollar_dollar -> cont 30;
	INT happy_dollar_dollar -> cont 31;
	STRINGLITERAL happy_dollar_dollar -> cont 32;
	CLASS -> cont 33;
	DEF -> cont 34;
	EXTENDS -> cont 35;
	IF -> cont 36;
	ELIF -> cont 37;
	ELSE -> cont 38;
	WHILE -> cont 39;
	RETURN -> cont 40;
	TYPECASE -> cont 41;
	AND -> cont 42;
	OR -> cont 43;
	NOT -> cont 44;
	NONE -> cont 45;
	GETS -> cont 46;
	PLUS -> cont 47;
	MINUS -> cont 48;
	TIMES -> cont 49;
	DIVIDE -> cont 50;
	EQUALS -> cont 51;
	ATMOST -> cont 52;
	LESS -> cont 53;
	ATLEAST -> cont 54;
	MORE -> cont 55;
	LBRACE -> cont 56;
	RBRACE -> cont 57;
	LPAREN -> cont 58;
	RPAREN -> cont 59;
	COMMA -> cont 60;
	SEMICOLON -> cont 61;
	DOT -> cont 62;
	COLON -> cont 63;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 64 tk tks = happyError' (tks, explist)
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
happyError' :: () => ([(LexemeClass)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parserCalc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [LexemeClass] -> a
parseError x  = error ("Parse error\n" ++ " Invalid Syntax at the First token of " ++ show (take 10 x) )

fixer:: Lexeme ->  LexemeClass
fixer (Lexeme _ x _) = x



main_Parser :: String -> IO ()
main_Parser filename=
    do flag <- doesFileExist filename
       when (not flag) (error ("The following file does not exist : " ++ filename))
       putStrLn ("Beginning Parsing of the Quack program in file " ++  filename)
       s <- readFile filename
       let sr = scanner s
       case sr of
            Left st  -> error st
            Right ls -> print (parserCalc (map fixer ls))
{-# LINE 1 "templates/GenericTemplate.hs" #-}





































































































































































































-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 











data Happy_IntList = HappyCons Int Happy_IntList




















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

