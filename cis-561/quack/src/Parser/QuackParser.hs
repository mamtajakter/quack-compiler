{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Parser.QuackParser where

import Parser.Lexer.QuackLexer
import Prelude hiding ( GT, LT, EQ)
import Control.Monad
import System.Environment ( getArgs, getProgName )
import System.Directory ( doesFileExist )
import Data.Maybe (fromJust)
import Control.Monad.IO.Class
import Parser.QuackAST
import Control.Monad.State.Lazy
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30
	= HappyTerminal (Lexeme)
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
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,306) ([0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,51648,32845,16,0,0,0,0,0,0,0,64,0,16384,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,48832,271,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,12295,16897,0,0,49180,2052,1,0,112,8211,4,0,448,32844,16,0,0,0,0,0,0,0,0,0,28672,4864,1056,0,49152,19457,4224,0,0,12295,16897,0,0,0,0,0,0,0,64492,8,0,0,0,2,0,0,48640,15,0,0,64256,126,0,0,0,4096,0,0,45056,1007,0,0,49152,8126,0,0,0,32507,0,0,112,8211,4,0,448,32844,16,0,1792,304,66,0,7168,1216,264,0,28672,4864,1056,0,49152,19457,4224,0,0,12295,16897,0,0,49180,2052,1,0,112,8211,4,0,448,32844,16,0,1792,304,66,0,7168,1216,264,0,16384,0,0,0,0,0,0,0,0,0,64,0,0,16,0,0,0,29296,8211,4,0,256,0,0,0,0,0,128,0,0,0,2048,0,0,1,512,0,0,0,0,0,0,12295,16897,0,0,0,0,1,0,0,38912,0,0,0,24576,2,0,0,0,8,0,0,0,32,0,0,0,248,0,0,0,992,0,0,0,3968,0,0,0,15872,0,0,0,63488,0,0,0,61312,3,0,0,48768,15,0,0,64256,62,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,128,0,28672,4978,1568,0,0,48,0,0,0,0,0,0,0,0,0,16,0,112,8211,4,0,0,61360,67,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,1,0,0,0,2048,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,12295,16897,0,0,0,0,0,0,0,0,0,0,448,32844,16,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,2,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,64256,126,0,0,60416,251,0,0,0,0,0,0,4,0,0,0,0,0,16,0,0,0,0,0,256,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,16384,0,0,64,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseQuack","Prog","Cl","Stmt","Statement","TyAlt","TyAltR","MRExpr","RexpStmtB","RexpStmt","StmtBlock1","LExpr","RExpr","ActArgs","RExprs1","RExprs","Class","ClassSig","Exten","FormArgs","ArgList1","TyAssign","ClassBody","Method1","Method","Ident11","Ident1","StmtBlock","num","str","ident","class","def","if","elif","else","while","return","typecase","extends","true","false","or","and","not","\">=\"","'>'","\"==\"","'<'","\"<=\"","'='","'/'","'*'","'-'","'+'","'.'","'{'","'}'","'('","')'","';'","':'","','","%eof"]
        bit_start = st * 66
        bit_end = (st + 1) * 66
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..65]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (34) = happyShift action_7
action_2 (6) = happyGoto action_4
action_2 (19) = happyGoto action_5
action_2 (20) = happyGoto action_6
action_2 _ = happyReduce_4

action_3 (66) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (31) = happyShift action_14
action_4 (32) = happyShift action_15
action_4 (33) = happyShift action_16
action_4 (36) = happyShift action_17
action_4 (39) = happyShift action_18
action_4 (40) = happyShift action_19
action_4 (41) = happyShift action_20
action_4 (43) = happyShift action_21
action_4 (44) = happyShift action_22
action_4 (47) = happyShift action_23
action_4 (56) = happyShift action_24
action_4 (61) = happyShift action_25
action_4 (7) = happyGoto action_11
action_4 (14) = happyGoto action_12
action_4 (15) = happyGoto action_13
action_4 _ = happyReduce_1

action_5 _ = happyReduce_3

action_6 (59) = happyShift action_10
action_6 (25) = happyGoto action_9
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (33) = happyShift action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (61) = happyShift action_52
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_50

action_10 (6) = happyGoto action_51
action_10 _ = happyReduce_4

action_11 _ = happyReduce_5

action_12 (53) = happyReduce_65
action_12 (64) = happyShift action_50
action_12 (29) = happyGoto action_49
action_12 _ = happyReduce_34

action_13 (45) = happyShift action_36
action_13 (46) = happyShift action_37
action_13 (48) = happyShift action_38
action_13 (49) = happyShift action_39
action_13 (50) = happyShift action_40
action_13 (51) = happyShift action_41
action_13 (52) = happyShift action_42
action_13 (54) = happyShift action_43
action_13 (55) = happyShift action_44
action_13 (56) = happyShift action_45
action_13 (57) = happyShift action_46
action_13 (58) = happyShift action_47
action_13 (63) = happyShift action_48
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_31

action_15 _ = happyReduce_30

action_16 (61) = happyShift action_35
action_16 _ = happyReduce_22

action_17 (31) = happyShift action_14
action_17 (32) = happyShift action_15
action_17 (33) = happyShift action_16
action_17 (43) = happyShift action_21
action_17 (44) = happyShift action_22
action_17 (47) = happyShift action_23
action_17 (56) = happyShift action_24
action_17 (61) = happyShift action_25
action_17 (14) = happyGoto action_26
action_17 (15) = happyGoto action_34
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (31) = happyShift action_14
action_18 (32) = happyShift action_15
action_18 (33) = happyShift action_16
action_18 (43) = happyShift action_21
action_18 (44) = happyShift action_22
action_18 (47) = happyShift action_23
action_18 (56) = happyShift action_24
action_18 (61) = happyShift action_25
action_18 (14) = happyGoto action_26
action_18 (15) = happyGoto action_33
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (31) = happyShift action_14
action_19 (32) = happyShift action_15
action_19 (33) = happyShift action_16
action_19 (43) = happyShift action_21
action_19 (44) = happyShift action_22
action_19 (47) = happyShift action_23
action_19 (56) = happyShift action_24
action_19 (61) = happyShift action_25
action_19 (10) = happyGoto action_31
action_19 (14) = happyGoto action_26
action_19 (15) = happyGoto action_32
action_19 _ = happyReduce_15

action_20 (31) = happyShift action_14
action_20 (32) = happyShift action_15
action_20 (33) = happyShift action_16
action_20 (43) = happyShift action_21
action_20 (44) = happyShift action_22
action_20 (47) = happyShift action_23
action_20 (56) = happyShift action_24
action_20 (61) = happyShift action_25
action_20 (14) = happyGoto action_26
action_20 (15) = happyGoto action_30
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_32

action_22 _ = happyReduce_33

action_23 (31) = happyShift action_14
action_23 (32) = happyShift action_15
action_23 (33) = happyShift action_16
action_23 (43) = happyShift action_21
action_23 (44) = happyShift action_22
action_23 (47) = happyShift action_23
action_23 (56) = happyShift action_24
action_23 (61) = happyShift action_25
action_23 (14) = happyGoto action_26
action_23 (15) = happyGoto action_29
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (31) = happyShift action_14
action_24 (32) = happyShift action_15
action_24 (33) = happyShift action_16
action_24 (43) = happyShift action_21
action_24 (44) = happyShift action_22
action_24 (47) = happyShift action_23
action_24 (56) = happyShift action_24
action_24 (61) = happyShift action_25
action_24 (14) = happyGoto action_26
action_24 (15) = happyGoto action_28
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (31) = happyShift action_14
action_25 (32) = happyShift action_15
action_25 (33) = happyShift action_16
action_25 (43) = happyShift action_21
action_25 (44) = happyShift action_22
action_25 (47) = happyShift action_23
action_25 (56) = happyShift action_24
action_25 (61) = happyShift action_25
action_25 (14) = happyGoto action_26
action_25 (15) = happyGoto action_27
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_34

action_27 (45) = happyShift action_36
action_27 (46) = happyShift action_37
action_27 (48) = happyShift action_38
action_27 (49) = happyShift action_39
action_27 (50) = happyShift action_40
action_27 (51) = happyShift action_41
action_27 (52) = happyShift action_42
action_27 (54) = happyShift action_43
action_27 (55) = happyShift action_44
action_27 (56) = happyShift action_45
action_27 (57) = happyShift action_46
action_27 (58) = happyShift action_47
action_27 (62) = happyShift action_77
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (58) = happyShift action_47
action_28 _ = happyReduce_29

action_29 (48) = happyShift action_38
action_29 (49) = happyShift action_39
action_29 (50) = happyShift action_40
action_29 (51) = happyShift action_41
action_29 (52) = happyShift action_42
action_29 (54) = happyShift action_43
action_29 (55) = happyShift action_44
action_29 (56) = happyShift action_45
action_29 (57) = happyShift action_46
action_29 (58) = happyShift action_47
action_29 _ = happyReduce_42

action_30 (45) = happyShift action_36
action_30 (46) = happyShift action_37
action_30 (48) = happyShift action_38
action_30 (49) = happyShift action_39
action_30 (50) = happyShift action_40
action_30 (51) = happyShift action_41
action_30 (52) = happyShift action_42
action_30 (54) = happyShift action_43
action_30 (55) = happyShift action_44
action_30 (56) = happyShift action_45
action_30 (57) = happyShift action_46
action_30 (58) = happyShift action_47
action_30 (59) = happyShift action_76
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (63) = happyShift action_75
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (45) = happyShift action_36
action_32 (46) = happyShift action_37
action_32 (48) = happyShift action_38
action_32 (49) = happyShift action_39
action_32 (50) = happyShift action_40
action_32 (51) = happyShift action_41
action_32 (52) = happyShift action_42
action_32 (54) = happyShift action_43
action_32 (55) = happyShift action_44
action_32 (56) = happyShift action_45
action_32 (57) = happyShift action_46
action_32 (58) = happyShift action_47
action_32 _ = happyReduce_16

action_33 (45) = happyShift action_36
action_33 (46) = happyShift action_37
action_33 (48) = happyShift action_38
action_33 (49) = happyShift action_39
action_33 (50) = happyShift action_40
action_33 (51) = happyShift action_41
action_33 (52) = happyShift action_42
action_33 (54) = happyShift action_43
action_33 (55) = happyShift action_44
action_33 (56) = happyShift action_45
action_33 (57) = happyShift action_46
action_33 (58) = happyShift action_47
action_33 (59) = happyShift action_73
action_33 (30) = happyGoto action_74
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (45) = happyShift action_36
action_34 (46) = happyShift action_37
action_34 (48) = happyShift action_38
action_34 (49) = happyShift action_39
action_34 (50) = happyShift action_40
action_34 (51) = happyShift action_41
action_34 (52) = happyShift action_42
action_34 (54) = happyShift action_43
action_34 (55) = happyShift action_44
action_34 (56) = happyShift action_45
action_34 (57) = happyShift action_46
action_34 (58) = happyShift action_47
action_34 (59) = happyShift action_73
action_34 (30) = happyGoto action_72
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (31) = happyShift action_14
action_35 (32) = happyShift action_15
action_35 (33) = happyShift action_16
action_35 (43) = happyShift action_21
action_35 (44) = happyShift action_22
action_35 (47) = happyShift action_23
action_35 (56) = happyShift action_24
action_35 (61) = happyShift action_25
action_35 (14) = happyGoto action_26
action_35 (15) = happyGoto action_70
action_35 (16) = happyGoto action_71
action_35 _ = happyReduce_45

action_36 (31) = happyShift action_14
action_36 (32) = happyShift action_15
action_36 (33) = happyShift action_16
action_36 (43) = happyShift action_21
action_36 (44) = happyShift action_22
action_36 (47) = happyShift action_23
action_36 (56) = happyShift action_24
action_36 (61) = happyShift action_25
action_36 (14) = happyGoto action_26
action_36 (15) = happyGoto action_69
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (31) = happyShift action_14
action_37 (32) = happyShift action_15
action_37 (33) = happyShift action_16
action_37 (43) = happyShift action_21
action_37 (44) = happyShift action_22
action_37 (47) = happyShift action_23
action_37 (56) = happyShift action_24
action_37 (61) = happyShift action_25
action_37 (14) = happyGoto action_26
action_37 (15) = happyGoto action_68
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (31) = happyShift action_14
action_38 (32) = happyShift action_15
action_38 (33) = happyShift action_16
action_38 (43) = happyShift action_21
action_38 (44) = happyShift action_22
action_38 (47) = happyShift action_23
action_38 (56) = happyShift action_24
action_38 (61) = happyShift action_25
action_38 (14) = happyGoto action_26
action_38 (15) = happyGoto action_67
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (31) = happyShift action_14
action_39 (32) = happyShift action_15
action_39 (33) = happyShift action_16
action_39 (43) = happyShift action_21
action_39 (44) = happyShift action_22
action_39 (47) = happyShift action_23
action_39 (56) = happyShift action_24
action_39 (61) = happyShift action_25
action_39 (14) = happyGoto action_26
action_39 (15) = happyGoto action_66
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (31) = happyShift action_14
action_40 (32) = happyShift action_15
action_40 (33) = happyShift action_16
action_40 (43) = happyShift action_21
action_40 (44) = happyShift action_22
action_40 (47) = happyShift action_23
action_40 (56) = happyShift action_24
action_40 (61) = happyShift action_25
action_40 (14) = happyGoto action_26
action_40 (15) = happyGoto action_65
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (31) = happyShift action_14
action_41 (32) = happyShift action_15
action_41 (33) = happyShift action_16
action_41 (43) = happyShift action_21
action_41 (44) = happyShift action_22
action_41 (47) = happyShift action_23
action_41 (56) = happyShift action_24
action_41 (61) = happyShift action_25
action_41 (14) = happyGoto action_26
action_41 (15) = happyGoto action_64
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (31) = happyShift action_14
action_42 (32) = happyShift action_15
action_42 (33) = happyShift action_16
action_42 (43) = happyShift action_21
action_42 (44) = happyShift action_22
action_42 (47) = happyShift action_23
action_42 (56) = happyShift action_24
action_42 (61) = happyShift action_25
action_42 (14) = happyGoto action_26
action_42 (15) = happyGoto action_63
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (31) = happyShift action_14
action_43 (32) = happyShift action_15
action_43 (33) = happyShift action_16
action_43 (43) = happyShift action_21
action_43 (44) = happyShift action_22
action_43 (47) = happyShift action_23
action_43 (56) = happyShift action_24
action_43 (61) = happyShift action_25
action_43 (14) = happyGoto action_26
action_43 (15) = happyGoto action_62
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (31) = happyShift action_14
action_44 (32) = happyShift action_15
action_44 (33) = happyShift action_16
action_44 (43) = happyShift action_21
action_44 (44) = happyShift action_22
action_44 (47) = happyShift action_23
action_44 (56) = happyShift action_24
action_44 (61) = happyShift action_25
action_44 (14) = happyGoto action_26
action_44 (15) = happyGoto action_61
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (31) = happyShift action_14
action_45 (32) = happyShift action_15
action_45 (33) = happyShift action_16
action_45 (43) = happyShift action_21
action_45 (44) = happyShift action_22
action_45 (47) = happyShift action_23
action_45 (56) = happyShift action_24
action_45 (61) = happyShift action_25
action_45 (14) = happyGoto action_26
action_45 (15) = happyGoto action_60
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (31) = happyShift action_14
action_46 (32) = happyShift action_15
action_46 (33) = happyShift action_16
action_46 (43) = happyShift action_21
action_46 (44) = happyShift action_22
action_46 (47) = happyShift action_23
action_46 (56) = happyShift action_24
action_46 (61) = happyShift action_25
action_46 (14) = happyGoto action_26
action_46 (15) = happyGoto action_59
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (33) = happyShift action_58
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_7

action_49 (53) = happyShift action_57
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (33) = happyShift action_56
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (31) = happyShift action_14
action_51 (32) = happyShift action_15
action_51 (33) = happyShift action_16
action_51 (36) = happyShift action_17
action_51 (39) = happyShift action_18
action_51 (40) = happyShift action_19
action_51 (41) = happyShift action_20
action_51 (43) = happyShift action_21
action_51 (44) = happyShift action_22
action_51 (47) = happyShift action_23
action_51 (56) = happyShift action_24
action_51 (61) = happyShift action_25
action_51 (7) = happyGoto action_11
action_51 (14) = happyGoto action_12
action_51 (15) = happyGoto action_13
action_51 (26) = happyGoto action_55
action_51 _ = happyReduce_60

action_52 (33) = happyShift action_54
action_52 (22) = happyGoto action_53
action_52 _ = happyReduce_54

action_53 (62) = happyShift action_89
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (64) = happyShift action_88
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (35) = happyShift action_86
action_55 (60) = happyShift action_87
action_55 (27) = happyGoto action_85
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_66

action_57 (31) = happyShift action_14
action_57 (32) = happyShift action_15
action_57 (33) = happyShift action_16
action_57 (43) = happyShift action_21
action_57 (44) = happyShift action_22
action_57 (47) = happyShift action_23
action_57 (56) = happyShift action_24
action_57 (61) = happyShift action_25
action_57 (14) = happyGoto action_26
action_57 (15) = happyGoto action_84
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (61) = happyShift action_83
action_58 _ = happyReduce_23

action_59 (54) = happyShift action_43
action_59 (55) = happyShift action_44
action_59 (58) = happyShift action_47
action_59 _ = happyReduce_24

action_60 (54) = happyShift action_43
action_60 (55) = happyShift action_44
action_60 (58) = happyShift action_47
action_60 _ = happyReduce_25

action_61 (58) = happyShift action_47
action_61 _ = happyReduce_26

action_62 (58) = happyShift action_47
action_62 _ = happyReduce_27

action_63 (48) = happyFail []
action_63 (49) = happyFail []
action_63 (50) = happyFail []
action_63 (51) = happyFail []
action_63 (52) = happyFail []
action_63 (54) = happyShift action_43
action_63 (55) = happyShift action_44
action_63 (56) = happyShift action_45
action_63 (57) = happyShift action_46
action_63 (58) = happyShift action_47
action_63 _ = happyReduce_36

action_64 (48) = happyFail []
action_64 (49) = happyFail []
action_64 (50) = happyFail []
action_64 (51) = happyFail []
action_64 (52) = happyFail []
action_64 (54) = happyShift action_43
action_64 (55) = happyShift action_44
action_64 (56) = happyShift action_45
action_64 (57) = happyShift action_46
action_64 (58) = happyShift action_47
action_64 _ = happyReduce_39

action_65 (48) = happyFail []
action_65 (49) = happyFail []
action_65 (50) = happyFail []
action_65 (51) = happyFail []
action_65 (52) = happyFail []
action_65 (54) = happyShift action_43
action_65 (55) = happyShift action_44
action_65 (56) = happyShift action_45
action_65 (57) = happyShift action_46
action_65 (58) = happyShift action_47
action_65 _ = happyReduce_35

action_66 (48) = happyFail []
action_66 (49) = happyFail []
action_66 (50) = happyFail []
action_66 (51) = happyFail []
action_66 (52) = happyFail []
action_66 (54) = happyShift action_43
action_66 (55) = happyShift action_44
action_66 (56) = happyShift action_45
action_66 (57) = happyShift action_46
action_66 (58) = happyShift action_47
action_66 _ = happyReduce_38

action_67 (48) = happyFail []
action_67 (49) = happyFail []
action_67 (50) = happyFail []
action_67 (51) = happyFail []
action_67 (52) = happyFail []
action_67 (54) = happyShift action_43
action_67 (55) = happyShift action_44
action_67 (56) = happyShift action_45
action_67 (57) = happyShift action_46
action_67 (58) = happyShift action_47
action_67 _ = happyReduce_37

action_68 (48) = happyShift action_38
action_68 (49) = happyShift action_39
action_68 (50) = happyShift action_40
action_68 (51) = happyShift action_41
action_68 (52) = happyShift action_42
action_68 (54) = happyShift action_43
action_68 (55) = happyShift action_44
action_68 (56) = happyShift action_45
action_68 (57) = happyShift action_46
action_68 (58) = happyShift action_47
action_68 _ = happyReduce_40

action_69 (46) = happyShift action_37
action_69 (48) = happyShift action_38
action_69 (49) = happyShift action_39
action_69 (50) = happyShift action_40
action_69 (51) = happyShift action_41
action_69 (52) = happyShift action_42
action_69 (54) = happyShift action_43
action_69 (55) = happyShift action_44
action_69 (56) = happyShift action_45
action_69 (57) = happyShift action_46
action_69 (58) = happyShift action_47
action_69 _ = happyReduce_41

action_70 (45) = happyShift action_36
action_70 (46) = happyShift action_37
action_70 (48) = happyShift action_38
action_70 (49) = happyShift action_39
action_70 (50) = happyShift action_40
action_70 (51) = happyShift action_41
action_70 (52) = happyShift action_42
action_70 (54) = happyShift action_43
action_70 (55) = happyShift action_44
action_70 (56) = happyShift action_45
action_70 (57) = happyShift action_46
action_70 (58) = happyShift action_47
action_70 (17) = happyGoto action_82
action_70 _ = happyReduce_47

action_71 (62) = happyShift action_81
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (11) = happyGoto action_80
action_72 _ = happyReduce_17

action_73 (6) = happyGoto action_79
action_73 _ = happyReduce_4

action_74 _ = happyReduce_9

action_75 _ = happyReduce_10

action_76 (8) = happyGoto action_78
action_76 _ = happyReduce_12

action_77 _ = happyReduce_28

action_78 (33) = happyShift action_105
action_78 (60) = happyShift action_106
action_78 (9) = happyGoto action_103
action_78 (24) = happyGoto action_104
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (31) = happyShift action_14
action_79 (32) = happyShift action_15
action_79 (33) = happyShift action_16
action_79 (36) = happyShift action_17
action_79 (39) = happyShift action_18
action_79 (40) = happyShift action_19
action_79 (41) = happyShift action_20
action_79 (43) = happyShift action_21
action_79 (44) = happyShift action_22
action_79 (47) = happyShift action_23
action_79 (56) = happyShift action_24
action_79 (60) = happyShift action_102
action_79 (61) = happyShift action_25
action_79 (7) = happyGoto action_11
action_79 (14) = happyGoto action_12
action_79 (15) = happyGoto action_13
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (37) = happyShift action_100
action_80 (38) = happyShift action_101
action_80 (12) = happyGoto action_98
action_80 (13) = happyGoto action_99
action_80 _ = happyReduce_20

action_81 _ = happyReduce_44

action_82 (65) = happyShift action_97
action_82 (18) = happyGoto action_96
action_82 _ = happyReduce_46

action_83 (31) = happyShift action_14
action_83 (32) = happyShift action_15
action_83 (33) = happyShift action_16
action_83 (43) = happyShift action_21
action_83 (44) = happyShift action_22
action_83 (47) = happyShift action_23
action_83 (56) = happyShift action_24
action_83 (61) = happyShift action_25
action_83 (14) = happyGoto action_26
action_83 (15) = happyGoto action_70
action_83 (16) = happyGoto action_95
action_83 _ = happyReduce_45

action_84 (45) = happyShift action_36
action_84 (46) = happyShift action_37
action_84 (48) = happyShift action_38
action_84 (49) = happyShift action_39
action_84 (50) = happyShift action_40
action_84 (51) = happyShift action_41
action_84 (52) = happyShift action_42
action_84 (54) = happyShift action_43
action_84 (55) = happyShift action_44
action_84 (56) = happyShift action_45
action_84 (57) = happyShift action_46
action_84 (58) = happyShift action_47
action_84 (63) = happyShift action_94
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_61

action_86 (33) = happyShift action_93
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_59

action_88 (33) = happyShift action_92
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (42) = happyShift action_91
action_89 (21) = happyGoto action_90
action_89 _ = happyReduce_52

action_90 _ = happyReduce_51

action_91 (33) = happyShift action_115
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (23) = happyGoto action_114
action_92 _ = happyReduce_56

action_93 (61) = happyShift action_113
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_6

action_95 (62) = happyShift action_112
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_48

action_97 (31) = happyShift action_14
action_97 (32) = happyShift action_15
action_97 (33) = happyShift action_16
action_97 (43) = happyShift action_21
action_97 (44) = happyShift action_22
action_97 (47) = happyShift action_23
action_97 (56) = happyShift action_24
action_97 (61) = happyShift action_25
action_97 (14) = happyGoto action_26
action_97 (15) = happyGoto action_111
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_18

action_99 _ = happyReduce_8

action_100 (31) = happyShift action_14
action_100 (32) = happyShift action_15
action_100 (33) = happyShift action_16
action_100 (43) = happyShift action_21
action_100 (44) = happyShift action_22
action_100 (47) = happyShift action_23
action_100 (56) = happyShift action_24
action_100 (61) = happyShift action_25
action_100 (14) = happyGoto action_26
action_100 (15) = happyGoto action_110
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (59) = happyShift action_73
action_101 (30) = happyGoto action_109
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_67

action_103 _ = happyReduce_13

action_104 (59) = happyShift action_73
action_104 (30) = happyGoto action_108
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (64) = happyShift action_107
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_11

action_107 (33) = happyShift action_119
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_14

action_109 _ = happyReduce_21

action_110 (45) = happyShift action_36
action_110 (46) = happyShift action_37
action_110 (48) = happyShift action_38
action_110 (49) = happyShift action_39
action_110 (50) = happyShift action_40
action_110 (51) = happyShift action_41
action_110 (52) = happyShift action_42
action_110 (54) = happyShift action_43
action_110 (55) = happyShift action_44
action_110 (56) = happyShift action_45
action_110 (57) = happyShift action_46
action_110 (58) = happyShift action_47
action_110 (59) = happyShift action_73
action_110 (30) = happyGoto action_118
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (45) = happyShift action_36
action_111 (46) = happyShift action_37
action_111 (48) = happyShift action_38
action_111 (49) = happyShift action_39
action_111 (50) = happyShift action_40
action_111 (51) = happyShift action_41
action_111 (52) = happyShift action_42
action_111 (54) = happyShift action_43
action_111 (55) = happyShift action_44
action_111 (56) = happyShift action_45
action_111 (57) = happyShift action_46
action_111 (58) = happyShift action_47
action_111 _ = happyReduce_49

action_112 _ = happyReduce_43

action_113 (33) = happyShift action_54
action_113 (22) = happyGoto action_117
action_113 _ = happyReduce_54

action_114 (65) = happyShift action_116
action_114 _ = happyReduce_55

action_115 _ = happyReduce_53

action_116 (33) = happyShift action_105
action_116 (24) = happyGoto action_121
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (62) = happyShift action_120
action_117 _ = happyFail (happyExpListPerState 117)

action_118 _ = happyReduce_19

action_119 _ = happyReduce_58

action_120 (64) = happyShift action_123
action_120 (28) = happyGoto action_122
action_120 _ = happyReduce_63

action_121 _ = happyReduce_57

action_122 (59) = happyShift action_73
action_122 (30) = happyGoto action_125
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (33) = happyShift action_124
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_64

action_125 _ = happyReduce_62

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Prog happy_var_1 happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  6 happyReduction_4
happyReduction_4  =  HappyAbsSyn6
		 ([]
	)

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 5 7 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Assignment happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn7
		 (StatementR (desugarRexpr happy_var_1)
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 5 7 happyReduction_8
happyReduction_8 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (If (desugarRexpr happy_var_2, happy_var_3) happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn30  happy_var_3)
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (While (desugarRexpr happy_var_2, happy_var_3)
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Return happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 5 7 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TyCase (desugarRexpr happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_0  8 happyReduction_12
happyReduction_12  =  HappyAbsSyn8
		 ([]
	)

happyReduce_13 = happySpecReduce_2  8 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  9 happyReduction_14
happyReduction_14 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn9
		 ((happy_var_1, happy_var_2)
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  10 happyReduction_15
happyReduction_15  =  HappyAbsSyn10
		 (Nothing
	)

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn10
		 (Just $ desugarRexpr happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  11 happyReduction_17
happyReduction_17  =  HappyAbsSyn11
		 ([]
	)

happyReduce_18 = happySpecReduce_2  11 happyReduction_18
happyReduction_18 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 (HappyAbsSyn30  happy_var_3)
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (If (desugarRexpr happy_var_2, happy_var_3) [] Nothing
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  13 happyReduction_20
happyReduction_20  =  HappyAbsSyn13
		 (Nothing
	)

happyReduce_21 = happySpecReduce_2  13 happyReduction_21
happyReduction_21 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Just happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 (HappyTerminal (Lexeme (LID happy_var_1) _ _))
	 =  HappyAbsSyn14
		 (Lid happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  14 happyReduction_23
happyReduction_23 (HappyTerminal (Lexeme (LID happy_var_3) _ _))
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (ObjField (desugarRexpr happy_var_1) happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  15 happyReduction_24
happyReduction_24 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (desugarRexpr $ Plus  happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  15 happyReduction_25
happyReduction_25 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (desugarRexpr $ Minus happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  15 happyReduction_26
happyReduction_26 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (desugarRexpr $ Times happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  15 happyReduction_27
happyReduction_27 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (desugarRexpr $ Div   happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  15 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (desugarRexpr happy_var_2
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  15 happyReduction_29
happyReduction_29 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (Neg (desugarRexpr happy_var_2)
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  15 happyReduction_30
happyReduction_30 (HappyTerminal (Lexeme (LSTRING happy_var_1) _ _))
	 =  HappyAbsSyn15
		 (Str happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  15 happyReduction_31
happyReduction_31 (HappyTerminal (Lexeme (LINT happy_var_1) _ _))
	 =  HappyAbsSyn15
		 (Int $ toInteger happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn15
		 (Bool True
	)

happyReduce_33 = happySpecReduce_1  15 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn15
		 (Bool False
	)

happyReduce_34 = happySpecReduce_1  15 happyReduction_34
happyReduction_34 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (LExpr happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  15 happyReduction_35
happyReduction_35 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (desugarRexpr $ PEQ happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  15 happyReduction_36
happyReduction_36 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (desugarRexpr $ PLEQ happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  15 happyReduction_37
happyReduction_37 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (desugarRexpr $ GEQ happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  15 happyReduction_38
happyReduction_38 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (desugarRexpr $ PGT  happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  15 happyReduction_39
happyReduction_39 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (desugarRexpr $ PLT happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  15 happyReduction_40
happyReduction_40 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (AND happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  15 happyReduction_41
happyReduction_41 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (OR happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  15 happyReduction_42
happyReduction_42 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (NOT happy_var_2
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happyReduce 6 15 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Lexeme (LID happy_var_3) _ _)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (RArgs happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_44 = happyReduce 4 15 happyReduction_44
happyReduction_44 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Lexeme (LID happy_var_1) _ _)) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Constr happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_45 = happySpecReduce_0  16 happyReduction_45
happyReduction_45  =  HappyAbsSyn16
		 (Nothing
	)

happyReduce_46 = happySpecReduce_2  16 happyReduction_46
happyReduction_46 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (Just (desugarRexpr happy_var_1 : happy_var_2)
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_0  17 happyReduction_47
happyReduction_47  =  HappyAbsSyn17
		 ([]
	)

happyReduce_48 = happySpecReduce_2  17 happyReduction_48
happyReduction_48 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  18 happyReduction_49
happyReduction_49 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (desugarRexpr happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  19 happyReduction_50
happyReduction_50 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (Class happy_var_1 happy_var_2
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happyMonadReduce 6 20 happyReduction_51
happyReduction_51 ((HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Lexeme (LID happy_var_2) _ _)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( state (\s -> (ClassSig happy_var_2 happy_var_4 happy_var_6, (PState happy_var_2)))))
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_52 = happySpecReduce_0  21 happyReduction_52
happyReduction_52  =  HappyAbsSyn21
		 (Just "Obj"
	)

happyReduce_53 = happySpecReduce_2  21 happyReduction_53
happyReduction_53 (HappyTerminal (Lexeme (LID happy_var_2) _ _))
	_
	 =  HappyAbsSyn21
		 (Just happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_0  22 happyReduction_54
happyReduction_54  =  HappyAbsSyn22
		 ([]
	)

happyReduce_55 = happyReduce 4 22 happyReduction_55
happyReduction_55 ((HappyAbsSyn23  happy_var_4) `HappyStk`
	(HappyTerminal (Lexeme (LID happy_var_3) _ _)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Lexeme (LID happy_var_1) _ _)) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 ((TyAssign (happy_var_1, happy_var_3):happy_var_4)
	) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_0  23 happyReduction_56
happyReduction_56  =  HappyAbsSyn23
		 ([]
	)

happyReduce_57 = happySpecReduce_3  23 happyReduction_57
happyReduction_57 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  24 happyReduction_58
happyReduction_58 (HappyTerminal (Lexeme (LID happy_var_3) _ _))
	_
	(HappyTerminal (Lexeme (LID happy_var_1) _ _))
	 =  HappyAbsSyn24
		 (TyAssign (happy_var_1, happy_var_3)
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happyReduce 4 25 happyReduction_59
happyReduction_59 (_ `HappyStk`
	(HappyAbsSyn26  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (ClassBody happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_60 = happySpecReduce_0  26 happyReduction_60
happyReduction_60  =  HappyAbsSyn26
		 ([]
	)

happyReduce_61 = happySpecReduce_2  26 happyReduction_61
happyReduction_61 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happyMonadReduce 7 27 happyReduction_62
happyReduction_62 ((HappyAbsSyn30  happy_var_7) `HappyStk`
	(HappyAbsSyn28  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Lexeme (LID happy_var_2) _ _)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( get >>= \(PState s) -> return $ Method s happy_var_2 happy_var_4 happy_var_6 happy_var_7))
	) (\r -> happyReturn (HappyAbsSyn27 r))

happyReduce_63 = happySpecReduce_0  28 happyReduction_63
happyReduction_63  =  HappyAbsSyn28
		 (Just "Nothing"
	)

happyReduce_64 = happySpecReduce_2  28 happyReduction_64
happyReduction_64 (HappyTerminal (Lexeme (LID happy_var_2) _ _))
	_
	 =  HappyAbsSyn28
		 (Just happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_0  29 happyReduction_65
happyReduction_65  =  HappyAbsSyn29
		 (Nothing
	)

happyReduce_66 = happySpecReduce_2  29 happyReduction_66
happyReduction_66 (HappyTerminal (Lexeme (LID happy_var_2) _ _))
	_
	 =  HappyAbsSyn29
		 (Just happy_var_2
	)
happyReduction_66 _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  30 happyReduction_67
happyReduction_67 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (happy_var_2
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 66 66 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Lexeme (LINT happy_dollar_dollar) _ _ -> cont 31;
	Lexeme (LSTRING happy_dollar_dollar) _ _ -> cont 32;
	Lexeme (LID happy_dollar_dollar) _ _ -> cont 33;
	Lexeme (LCLASS) _ _ -> cont 34;
	Lexeme LDEF _ _ -> cont 35;
	Lexeme LIF _ _ -> cont 36;
	Lexeme LELIF _ _ -> cont 37;
	Lexeme LELSE _ _ -> cont 38;
	Lexeme LWHILE _ _ -> cont 39;
	Lexeme LRETURN _ _ -> cont 40;
	Lexeme LTYPECASE _ _ -> cont 41;
	Lexeme LEXTENDS _ _ -> cont 42;
	Lexeme LTRUE _ _ -> cont 43;
	Lexeme LFALSE _ _ -> cont 44;
	Lexeme LOR _ _ -> cont 45;
	Lexeme LAND _ _ -> cont 46;
	Lexeme LNOT _ _ -> cont 47;
	Lexeme LGE _ _ -> cont 48;
	Lexeme LGT _ _ -> cont 49;
	Lexeme LEQUALS _ _ -> cont 50;
	Lexeme LLT _ _ -> cont 51;
	Lexeme LLE _ _ -> cont 52;
	Lexeme LLEQ _ _ -> cont 53;
	Lexeme LDIVIDE _ _ -> cont 54;
	Lexeme LTIMES _ _ -> cont 55;
	Lexeme LMINUS _ _ -> cont 56;
	Lexeme LPLUS _ _ -> cont 57;
	Lexeme LDOT _ _ -> cont 58;
	Lexeme LBRACEL _ _ -> cont 59;
	Lexeme LBRACER _ _ -> cont 60;
	Lexeme LPARENL _ _ -> cont 61;
	Lexeme LPARENR _ _ -> cont 62;
	Lexeme LSEMICOLON _ _ -> cont 63;
	Lexeme LCOLON _ _ -> cont 64;
	Lexeme LCOMMA _ _ -> cont 65;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 66 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => MonadParser a -> (a -> MonadParser b) -> MonadParser b
happyThen = (>>=)
happyReturn :: () => a -> MonadParser a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> MonadParser a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Lexeme)], [String]) -> MonadParser a
happyError' = (\(tokens, _) -> parseError tokens)
parseQuack tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


getLexemeClass :: Lexeme -> LexemeClass
getLexemeClass (Lexeme cls _ _) = cls

parseError :: [Lexeme] -> MonadParser a
parseError l   = failE ("Parse Error on token (" ++ show (head l) ++ ")")

getString :: LexemeClass -> Maybe String
getString (LSTRING str) = Just str
getString _             = Nothing -- error "getString called on non-string token"

getInt :: LexemeClass -> Maybe Integer
getInt (LINT int) = Just $ toInteger int
getInt _          = Nothing -- error "getInt called on non-integer token"



data MonadParser a = MParser {runParser :: ParserState -> Either String (a, ParserState)}


data ParserState = PState
                 {
                   className :: String
                 }

startState :: ParserState
startState = PState ""

instance Functor MonadParser where
  fmap f mp  = MParser $ \ps ->
               case runParser mp ps of
                     Left msg       -> Left msg
                     Right (a, pst) -> Right (f a, pst)


instance Applicative MonadParser where
  pure       = return
  (<*>)        = ap


instance Monad MonadParser where
  return x = MParser $ \ps -> Right (x, ps)
  m >>= k  = MParser $ \ps -> case runParser m ps of
                        Left msg       -> Left msg
                        Right (a, pst) -> runParser (k a) pst


instance MonadState ParserState MonadParser where
  get        = MParser $ \s -> Right(s,s)
  put ps     = MParser $ \_ -> Right ((), ps)
  state func = MParser $ \s -> Right $ func s



failE :: String -> MonadParser a
failE err = MParser $ \_ -> Left err


mainParse :: String -> IO (MonadParser Prog)
mainParse filename =
  do
     flag <- doesFileExist filename
     when (not flag) (error' ("The following file does not exist : " ++ filename))
     putStrLn ("Parsing Quack program in file " ++ filename)
     s <- readFile filename
     let sr = scanner s
     case sr of
         Left msg      -> return $ failE msg
         Right tokList -> return $ parseQuack $ init tokList
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 15 "<built-in>" #-}
{-# LINE 1 "/Users/boscondemeye/.stack/programs/x86_64-osx/ghc-8.2.2/lib/ghc-8.2.2/include/ghcversion.h" #-}
















{-# LINE 16 "<built-in>" #-}
{-# LINE 1 "/var/folders/q_/zms8f3w15q9d8zws8_gnnn000000gn/T/ghc91322_0/ghc_2.h" #-}



































































































































































{-# LINE 17 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 










{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList








{-# LINE 65 "templates/GenericTemplate.hs" #-}


{-# LINE 75 "templates/GenericTemplate.hs" #-}










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

