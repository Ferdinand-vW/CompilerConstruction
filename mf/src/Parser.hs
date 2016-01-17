{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import Administration
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20
	= HappyTerminal (Token)
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

action_0 (50) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail

action_1 (50) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (6) = happyGoto action_5
action_3 _ = happyReduce_4

action_4 (63) = happyAccept
action_4 _ = happyFail

action_5 (21) = happyShift action_10
action_5 (24) = happyShift action_11
action_5 (27) = happyShift action_12
action_5 (29) = happyShift action_13
action_5 (46) = happyShift action_14
action_5 (48) = happyShift action_15
action_5 (52) = happyShift action_16
action_5 (7) = happyGoto action_6
action_5 (12) = happyGoto action_7
action_5 (13) = happyGoto action_8
action_5 (14) = happyGoto action_9
action_5 _ = happyFail

action_6 _ = happyReduce_3

action_7 (21) = happyShift action_10
action_7 (24) = happyShift action_11
action_7 (27) = happyShift action_12
action_7 (29) = happyShift action_13
action_7 (46) = happyShift action_14
action_7 (48) = happyShift action_15
action_7 (51) = happyShift action_35
action_7 (13) = happyGoto action_34
action_7 (14) = happyGoto action_9
action_7 _ = happyFail

action_8 _ = happyReduce_13

action_9 _ = happyReduce_17

action_10 (31) = happyShift action_33
action_10 _ = happyFail

action_11 (21) = happyShift action_25
action_11 (22) = happyShift action_26
action_11 (23) = happyShift action_27
action_11 (30) = happyShift action_28
action_11 (34) = happyShift action_29
action_11 (44) = happyShift action_30
action_11 (46) = happyShift action_31
action_11 (17) = happyGoto action_21
action_11 (18) = happyGoto action_22
action_11 (19) = happyGoto action_32
action_11 (20) = happyGoto action_24
action_11 _ = happyFail

action_12 (21) = happyShift action_25
action_12 (22) = happyShift action_26
action_12 (23) = happyShift action_27
action_12 (30) = happyShift action_28
action_12 (34) = happyShift action_29
action_12 (44) = happyShift action_30
action_12 (46) = happyShift action_31
action_12 (17) = happyGoto action_21
action_12 (18) = happyGoto action_22
action_12 (19) = happyGoto action_23
action_12 (20) = happyGoto action_24
action_12 _ = happyFail

action_13 (43) = happyShift action_20
action_13 _ = happyFail

action_14 (21) = happyShift action_10
action_14 (24) = happyShift action_11
action_14 (27) = happyShift action_12
action_14 (29) = happyShift action_13
action_14 (46) = happyShift action_14
action_14 (48) = happyShift action_15
action_14 (12) = happyGoto action_19
action_14 (13) = happyGoto action_8
action_14 (14) = happyGoto action_9
action_14 _ = happyFail

action_15 (21) = happyShift action_10
action_15 (24) = happyShift action_11
action_15 (27) = happyShift action_12
action_15 (29) = happyShift action_13
action_15 (46) = happyShift action_14
action_15 (48) = happyShift action_15
action_15 (12) = happyGoto action_18
action_15 (13) = happyGoto action_8
action_15 (14) = happyGoto action_9
action_15 _ = happyFail

action_16 (21) = happyShift action_17
action_16 _ = happyFail

action_17 (46) = happyShift action_61
action_17 _ = happyFail

action_18 (21) = happyShift action_10
action_18 (24) = happyShift action_11
action_18 (27) = happyShift action_12
action_18 (29) = happyShift action_13
action_18 (46) = happyShift action_14
action_18 (48) = happyShift action_15
action_18 (49) = happyShift action_60
action_18 (13) = happyGoto action_34
action_18 (14) = happyGoto action_9
action_18 _ = happyFail

action_19 (21) = happyShift action_10
action_19 (24) = happyShift action_11
action_19 (27) = happyShift action_12
action_19 (29) = happyShift action_13
action_19 (46) = happyShift action_14
action_19 (47) = happyShift action_59
action_19 (48) = happyShift action_15
action_19 (13) = happyGoto action_34
action_19 (14) = happyGoto action_9
action_19 _ = happyFail

action_20 _ = happyReduce_18

action_21 (32) = happyShift action_50
action_21 (33) = happyShift action_51
action_21 (34) = happyShift action_52
action_21 (35) = happyShift action_53
action_21 (38) = happyShift action_54
action_21 (39) = happyShift action_55
action_21 (40) = happyShift action_56
action_21 (41) = happyShift action_57
action_21 (42) = happyShift action_58
action_21 _ = happyFail

action_22 _ = happyReduce_32

action_23 (28) = happyShift action_49
action_23 (36) = happyShift action_39
action_23 (37) = happyShift action_40
action_23 (38) = happyShift action_41
action_23 _ = happyFail

action_24 _ = happyReduce_48

action_25 (44) = happyShift action_48
action_25 _ = happyReduce_34

action_26 _ = happyReduce_33

action_27 _ = happyReduce_49

action_28 (21) = happyShift action_25
action_28 (22) = happyShift action_26
action_28 (23) = happyShift action_27
action_28 (30) = happyShift action_28
action_28 (34) = happyShift action_29
action_28 (44) = happyShift action_30
action_28 (46) = happyShift action_31
action_28 (17) = happyGoto action_21
action_28 (18) = happyGoto action_22
action_28 (19) = happyGoto action_47
action_28 (20) = happyGoto action_24
action_28 _ = happyFail

action_29 (21) = happyShift action_25
action_29 (22) = happyShift action_26
action_29 (34) = happyShift action_29
action_29 (44) = happyShift action_30
action_29 (46) = happyShift action_45
action_29 (18) = happyGoto action_46
action_29 _ = happyFail

action_30 (21) = happyShift action_25
action_30 (22) = happyShift action_26
action_30 (34) = happyShift action_29
action_30 (44) = happyShift action_30
action_30 (46) = happyShift action_45
action_30 (17) = happyGoto action_44
action_30 (18) = happyGoto action_22
action_30 _ = happyFail

action_31 (21) = happyShift action_25
action_31 (22) = happyShift action_26
action_31 (23) = happyShift action_27
action_31 (30) = happyShift action_28
action_31 (34) = happyShift action_29
action_31 (44) = happyShift action_30
action_31 (46) = happyShift action_31
action_31 (17) = happyGoto action_42
action_31 (18) = happyGoto action_22
action_31 (19) = happyGoto action_43
action_31 (20) = happyGoto action_24
action_31 _ = happyFail

action_32 (25) = happyShift action_38
action_32 (36) = happyShift action_39
action_32 (37) = happyShift action_40
action_32 (38) = happyShift action_41
action_32 _ = happyFail

action_33 (21) = happyShift action_25
action_33 (22) = happyShift action_26
action_33 (23) = happyShift action_27
action_33 (30) = happyShift action_28
action_33 (34) = happyShift action_29
action_33 (44) = happyShift action_30
action_33 (46) = happyShift action_31
action_33 (17) = happyGoto action_36
action_33 (18) = happyGoto action_22
action_33 (19) = happyGoto action_37
action_33 (20) = happyGoto action_24
action_33 _ = happyFail

action_34 _ = happyReduce_12

action_35 _ = happyReduce_2

action_36 (32) = happyShift action_50
action_36 (33) = happyShift action_51
action_36 (34) = happyShift action_52
action_36 (35) = happyShift action_53
action_36 (38) = happyShift action_54
action_36 (39) = happyShift action_55
action_36 (40) = happyShift action_56
action_36 (41) = happyShift action_57
action_36 (42) = happyShift action_58
action_36 (43) = happyShift action_84
action_36 _ = happyFail

action_37 (36) = happyShift action_39
action_37 (37) = happyShift action_40
action_37 (38) = happyShift action_41
action_37 (43) = happyShift action_83
action_37 _ = happyFail

action_38 (21) = happyShift action_10
action_38 (29) = happyShift action_13
action_38 (46) = happyShift action_14
action_38 (48) = happyShift action_15
action_38 (14) = happyGoto action_82
action_38 _ = happyFail

action_39 (21) = happyShift action_25
action_39 (22) = happyShift action_26
action_39 (23) = happyShift action_27
action_39 (30) = happyShift action_28
action_39 (34) = happyShift action_29
action_39 (44) = happyShift action_30
action_39 (46) = happyShift action_31
action_39 (17) = happyGoto action_21
action_39 (18) = happyGoto action_22
action_39 (19) = happyGoto action_81
action_39 (20) = happyGoto action_24
action_39 _ = happyFail

action_40 (21) = happyShift action_25
action_40 (22) = happyShift action_26
action_40 (23) = happyShift action_27
action_40 (30) = happyShift action_28
action_40 (34) = happyShift action_29
action_40 (44) = happyShift action_30
action_40 (46) = happyShift action_31
action_40 (17) = happyGoto action_21
action_40 (18) = happyGoto action_22
action_40 (19) = happyGoto action_80
action_40 (20) = happyGoto action_24
action_40 _ = happyFail

action_41 (21) = happyShift action_25
action_41 (22) = happyShift action_26
action_41 (23) = happyShift action_27
action_41 (30) = happyShift action_28
action_41 (34) = happyShift action_29
action_41 (44) = happyShift action_30
action_41 (46) = happyShift action_31
action_41 (17) = happyGoto action_21
action_41 (18) = happyGoto action_22
action_41 (19) = happyGoto action_79
action_41 (20) = happyGoto action_24
action_41 _ = happyFail

action_42 (32) = happyShift action_50
action_42 (33) = happyShift action_51
action_42 (34) = happyShift action_52
action_42 (35) = happyShift action_53
action_42 (38) = happyShift action_54
action_42 (39) = happyShift action_55
action_42 (40) = happyShift action_56
action_42 (41) = happyShift action_57
action_42 (42) = happyShift action_58
action_42 (47) = happyShift action_78
action_42 _ = happyFail

action_43 (36) = happyShift action_39
action_43 (37) = happyShift action_40
action_43 (38) = happyShift action_41
action_43 (47) = happyShift action_77
action_43 _ = happyFail

action_44 (32) = happyShift action_50
action_44 (33) = happyShift action_51
action_44 (34) = happyShift action_52
action_44 (35) = happyShift action_53
action_44 (45) = happyShift action_76
action_44 _ = happyFail

action_45 (21) = happyShift action_25
action_45 (22) = happyShift action_26
action_45 (34) = happyShift action_29
action_45 (44) = happyShift action_30
action_45 (46) = happyShift action_45
action_45 (17) = happyGoto action_75
action_45 (18) = happyGoto action_22
action_45 _ = happyFail

action_46 _ = happyReduce_37

action_47 _ = happyReduce_39

action_48 (21) = happyShift action_25
action_48 (22) = happyShift action_26
action_48 (34) = happyShift action_29
action_48 (44) = happyShift action_30
action_48 (46) = happyShift action_45
action_48 (17) = happyGoto action_74
action_48 (18) = happyGoto action_22
action_48 _ = happyFail

action_49 (21) = happyShift action_10
action_49 (29) = happyShift action_13
action_49 (46) = happyShift action_14
action_49 (48) = happyShift action_15
action_49 (14) = happyGoto action_73
action_49 _ = happyFail

action_50 (21) = happyShift action_25
action_50 (22) = happyShift action_26
action_50 (34) = happyShift action_29
action_50 (44) = happyShift action_30
action_50 (46) = happyShift action_45
action_50 (17) = happyGoto action_72
action_50 (18) = happyGoto action_22
action_50 _ = happyFail

action_51 (21) = happyShift action_25
action_51 (22) = happyShift action_26
action_51 (34) = happyShift action_29
action_51 (44) = happyShift action_30
action_51 (46) = happyShift action_45
action_51 (17) = happyGoto action_71
action_51 (18) = happyGoto action_22
action_51 _ = happyFail

action_52 (21) = happyShift action_25
action_52 (22) = happyShift action_26
action_52 (34) = happyShift action_29
action_52 (44) = happyShift action_30
action_52 (46) = happyShift action_45
action_52 (17) = happyGoto action_70
action_52 (18) = happyGoto action_22
action_52 _ = happyFail

action_53 (21) = happyShift action_25
action_53 (22) = happyShift action_26
action_53 (34) = happyShift action_29
action_53 (44) = happyShift action_30
action_53 (46) = happyShift action_45
action_53 (17) = happyGoto action_69
action_53 (18) = happyGoto action_22
action_53 _ = happyFail

action_54 (21) = happyShift action_25
action_54 (22) = happyShift action_26
action_54 (34) = happyShift action_29
action_54 (44) = happyShift action_30
action_54 (46) = happyShift action_45
action_54 (17) = happyGoto action_68
action_54 (18) = happyGoto action_22
action_54 _ = happyFail

action_55 (21) = happyShift action_25
action_55 (22) = happyShift action_26
action_55 (34) = happyShift action_29
action_55 (44) = happyShift action_30
action_55 (46) = happyShift action_45
action_55 (17) = happyGoto action_67
action_55 (18) = happyGoto action_22
action_55 _ = happyFail

action_56 (21) = happyShift action_25
action_56 (22) = happyShift action_26
action_56 (34) = happyShift action_29
action_56 (44) = happyShift action_30
action_56 (46) = happyShift action_45
action_56 (17) = happyGoto action_66
action_56 (18) = happyGoto action_22
action_56 _ = happyFail

action_57 (21) = happyShift action_25
action_57 (22) = happyShift action_26
action_57 (34) = happyShift action_29
action_57 (44) = happyShift action_30
action_57 (46) = happyShift action_45
action_57 (17) = happyGoto action_65
action_57 (18) = happyGoto action_22
action_57 _ = happyFail

action_58 (21) = happyShift action_25
action_58 (22) = happyShift action_26
action_58 (34) = happyShift action_29
action_58 (44) = happyShift action_30
action_58 (46) = happyShift action_45
action_58 (17) = happyGoto action_64
action_58 (18) = happyGoto action_22
action_58 _ = happyFail

action_59 _ = happyReduce_21

action_60 _ = happyReduce_22

action_61 (54) = happyShift action_63
action_61 (8) = happyGoto action_62
action_61 _ = happyReduce_7

action_62 (55) = happyShift action_91
action_62 (9) = happyGoto action_90
action_62 _ = happyFail

action_63 (21) = happyShift action_89
action_63 (10) = happyGoto action_87
action_63 (11) = happyGoto action_88
action_63 _ = happyFail

action_64 (32) = happyShift action_50
action_64 (33) = happyShift action_51
action_64 (34) = happyShift action_52
action_64 (35) = happyShift action_53
action_64 _ = happyReduce_46

action_65 (32) = happyShift action_50
action_65 (33) = happyShift action_51
action_65 (34) = happyShift action_52
action_65 (35) = happyShift action_53
action_65 _ = happyReduce_47

action_66 (32) = happyShift action_50
action_66 (33) = happyShift action_51
action_66 (34) = happyShift action_52
action_66 (35) = happyShift action_53
action_66 _ = happyReduce_45

action_67 (32) = happyShift action_50
action_67 (33) = happyShift action_51
action_67 (34) = happyShift action_52
action_67 (35) = happyShift action_53
action_67 _ = happyReduce_44

action_68 (32) = happyShift action_50
action_68 (33) = happyShift action_51
action_68 (34) = happyShift action_52
action_68 (35) = happyShift action_53
action_68 _ = happyReduce_43

action_69 _ = happyReduce_31

action_70 _ = happyReduce_30

action_71 (34) = happyShift action_52
action_71 (35) = happyShift action_53
action_71 _ = happyReduce_29

action_72 (34) = happyShift action_52
action_72 (35) = happyShift action_53
action_72 _ = happyReduce_28

action_73 _ = happyReduce_16

action_74 (32) = happyShift action_50
action_74 (33) = happyShift action_51
action_74 (34) = happyShift action_52
action_74 (35) = happyShift action_53
action_74 (45) = happyShift action_86
action_74 _ = happyFail

action_75 (32) = happyShift action_50
action_75 (33) = happyShift action_51
action_75 (34) = happyShift action_52
action_75 (35) = happyShift action_53
action_75 (47) = happyShift action_78
action_75 _ = happyFail

action_76 _ = happyReduce_36

action_77 _ = happyReduce_50

action_78 _ = happyReduce_35

action_79 (38) = happyFail
action_79 _ = happyReduce_42

action_80 (36) = happyShift action_39
action_80 (38) = happyShift action_41
action_80 _ = happyReduce_41

action_81 (38) = happyShift action_41
action_81 _ = happyReduce_40

action_82 (26) = happyShift action_85
action_82 _ = happyReduce_15

action_83 _ = happyReduce_20

action_84 _ = happyReduce_19

action_85 (21) = happyShift action_10
action_85 (29) = happyShift action_13
action_85 (46) = happyShift action_14
action_85 (48) = happyShift action_15
action_85 (14) = happyGoto action_95
action_85 _ = happyFail

action_86 _ = happyReduce_38

action_87 (57) = happyShift action_94
action_87 _ = happyFail

action_88 _ = happyReduce_10

action_89 _ = happyReduce_11

action_90 (47) = happyShift action_93
action_90 _ = happyFail

action_91 (21) = happyShift action_89
action_91 (11) = happyGoto action_92
action_91 _ = happyFail

action_92 _ = happyReduce_8

action_93 (53) = happyShift action_97
action_93 _ = happyFail

action_94 (21) = happyShift action_89
action_94 (11) = happyGoto action_96
action_94 _ = happyReduce_6

action_95 _ = happyReduce_14

action_96 _ = happyReduce_9

action_97 (21) = happyShift action_10
action_97 (24) = happyShift action_11
action_97 (27) = happyShift action_12
action_97 (29) = happyShift action_13
action_97 (46) = happyShift action_14
action_97 (48) = happyShift action_15
action_97 (12) = happyGoto action_98
action_97 (13) = happyGoto action_8
action_97 (14) = happyGoto action_9
action_97 _ = happyFail

action_98 (21) = happyShift action_10
action_98 (24) = happyShift action_11
action_98 (27) = happyShift action_12
action_98 (29) = happyShift action_13
action_98 (46) = happyShift action_14
action_98 (48) = happyShift action_15
action_98 (51) = happyShift action_99
action_98 (13) = happyGoto action_34
action_98 (14) = happyGoto action_9
action_98 _ = happyFail

action_99 _ = happyReduce_5

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happyReduce 4 5 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Program happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [ happy_var_2 ]
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  6 happyReduction_4
happyReduction_4  =  HappyAbsSyn6
		 ([ ]
	)

happyReduce_5 = happyReduce 9 7 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Proc happy_var_2 happy_var_4 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  8 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  8 happyReduction_7
happyReduction_7  =  HappyAbsSyn8
		 ([]
	)

happyReduce_8 = happySpecReduce_2  9 happyReduction_8
happyReduction_8 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  10 happyReduction_9
happyReduction_9 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [ happy_var_3 ]
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([ happy_var_1 ]
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  11 happyReduction_11
happyReduction_11 (HappyTerminal (TIdent happy_var_1))
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  12 happyReduction_12
happyReduction_12 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Seq happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  12 happyReduction_13
happyReduction_13 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happyReduce 6 13 happyReduction_14
happyReduction_14 ((HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (IfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 4 13 happyReduction_15
happyReduction_15 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (IfThenElse happy_var_2 happy_var_4 Skip
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 4 13 happyReduction_16
happyReduction_16 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  14 happyReduction_18
happyReduction_18 _
	_
	 =  HappyAbsSyn14
		 (Skip
	)

happyReduce_19 = happyReduce 4 14 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (IAssign happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 14 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (BAssign happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_3  14 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  14 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  15 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [ happy_var_3 ]
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  15 happyReduction_24
happyReduction_24 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([ happy_var_1 ]
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_0  15 happyReduction_25
happyReduction_25  =  HappyAbsSyn15
		 ([]
	)

happyReduce_26 = happySpecReduce_1  16 happyReduction_26
happyReduction_26 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (I happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn16
		 (B happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  17 happyReduction_28
happyReduction_28 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  17 happyReduction_29
happyReduction_29 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  17 happyReduction_30
happyReduction_30 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Times happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  17 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Divide happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  17 happyReduction_32
happyReduction_32 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 (HappyTerminal (TInt happy_var_1))
	 =  HappyAbsSyn18
		 (IConst happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 (HappyTerminal (TIdent happy_var_1))
	 =  HappyAbsSyn18
		 (Var happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  18 happyReduction_35
happyReduction_35 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  18 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  18 happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (Deref happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 4 18 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Deref (Plus (Var happy_var_1) happy_var_3)
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_2  19 happyReduction_39
happyReduction_39 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (Not happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  19 happyReduction_40
happyReduction_40 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (And happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  19 happyReduction_41
happyReduction_41 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Or happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  19 happyReduction_42
happyReduction_42 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (BEqual happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  19 happyReduction_43
happyReduction_43 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (IEqual happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  19 happyReduction_44
happyReduction_44 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (LessThan happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  19 happyReduction_45
happyReduction_45 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (GreaterThan happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  19 happyReduction_46
happyReduction_46 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (GreaterEqual happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  19 happyReduction_47
happyReduction_47 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (LessEqual happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  19 happyReduction_48
happyReduction_48 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  20 happyReduction_49
happyReduction_49 (HappyTerminal (TBool happy_var_1))
	 =  HappyAbsSyn20
		 (BConst happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  20 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 63 63 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TIdent happy_dollar_dollar -> cont 21;
	TInt happy_dollar_dollar -> cont 22;
	TBool happy_dollar_dollar -> cont 23;
	TIf -> cont 24;
	TThen -> cont 25;
	TElse -> cont 26;
	TWhile -> cont 27;
	TDo -> cont 28;
	TSkip -> cont 29;
	TNot -> cont 30;
	TAssign -> cont 31;
	TArithmeticOp "+" -> cont 32;
	TArithmeticOp "-" -> cont 33;
	TStar -> cont 34;
	TArithmeticOp "/" -> cont 35;
	TBoolOp "and" -> cont 36;
	TBoolOp "or" -> cont 37;
	TRelOp "==" -> cont 38;
	TRelOp "<" -> cont 39;
	TRelOp ">" -> cont 40;
	TRelOp "<=" -> cont 41;
	TRelOp ">=" -> cont 42;
	TSemicolon -> cont 43;
	TBlockOpen -> cont 44;
	TBlockClose -> cont 45;
	TParenOpen -> cont 46;
	TParenClose -> cont 47;
	TBraceOpen -> cont 48;
	TBraceClose -> cont 49;
	TBegin -> cont 50;
	TEnd -> cont 51;
	TProc -> cont 52;
	TIs -> cont 53;
	TVal -> cont 54;
	TRes -> cont 55;
	TCall -> cont 56;
	TComma -> cont 57;
	TMalloc -> cont 58;
	TFree -> cont 59;
	TContinue -> cont 60;
	TBreak -> cont 61;
	TTyInt -> cont 62;
	_ -> happyError' (tk:tks)
	}

happyError_ 63 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

happy tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError (x:xs) = error ("Parse error: " ++ show xs)
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}






{-# LINE 1 "G:\\GitHub\\haskell-platform\\build\\ghc-bindist\\local\\lib/include\\ghcversion.h" #-}

















{-# LINE 6 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}

{-# LINE 46 "templates\\GenericTemplate.hs" #-}








{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

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

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

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
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

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
happyFail  i tk (HappyState (action)) sts stk =
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

{-# LINE 322 "templates\\GenericTemplate.hs" #-}
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
