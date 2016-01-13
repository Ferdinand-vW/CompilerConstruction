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
action_5 (34) = happyShift action_14
action_5 (46) = happyShift action_15
action_5 (48) = happyShift action_16
action_5 (52) = happyShift action_17
action_5 (56) = happyShift action_18
action_5 (58) = happyShift action_19
action_5 (59) = happyShift action_20
action_5 (60) = happyShift action_21
action_5 (61) = happyShift action_22
action_5 (62) = happyShift action_23
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
action_7 (34) = happyShift action_14
action_7 (46) = happyShift action_15
action_7 (48) = happyShift action_16
action_7 (51) = happyShift action_51
action_7 (56) = happyShift action_18
action_7 (58) = happyShift action_19
action_7 (59) = happyShift action_20
action_7 (60) = happyShift action_21
action_7 (61) = happyShift action_22
action_7 (62) = happyShift action_23
action_7 (13) = happyGoto action_50
action_7 (14) = happyGoto action_9
action_7 _ = happyFail

action_8 _ = happyReduce_13

action_9 _ = happyReduce_17

action_10 (31) = happyShift action_48
action_10 (44) = happyShift action_49
action_10 _ = happyFail

action_11 (21) = happyShift action_34
action_11 (22) = happyShift action_35
action_11 (23) = happyShift action_44
action_11 (30) = happyShift action_45
action_11 (34) = happyShift action_36
action_11 (44) = happyShift action_37
action_11 (46) = happyShift action_46
action_11 (17) = happyGoto action_40
action_11 (18) = happyGoto action_41
action_11 (19) = happyGoto action_47
action_11 (20) = happyGoto action_43
action_11 _ = happyFail

action_12 (21) = happyShift action_34
action_12 (22) = happyShift action_35
action_12 (23) = happyShift action_44
action_12 (30) = happyShift action_45
action_12 (34) = happyShift action_36
action_12 (44) = happyShift action_37
action_12 (46) = happyShift action_46
action_12 (17) = happyGoto action_40
action_12 (18) = happyGoto action_41
action_12 (19) = happyGoto action_42
action_12 (20) = happyGoto action_43
action_12 _ = happyFail

action_13 (43) = happyShift action_39
action_13 _ = happyFail

action_14 (21) = happyShift action_34
action_14 (22) = happyShift action_35
action_14 (34) = happyShift action_36
action_14 (44) = happyShift action_37
action_14 (46) = happyShift action_38
action_14 (18) = happyGoto action_33
action_14 _ = happyFail

action_15 (21) = happyShift action_10
action_15 (24) = happyShift action_11
action_15 (27) = happyShift action_12
action_15 (29) = happyShift action_13
action_15 (34) = happyShift action_14
action_15 (46) = happyShift action_15
action_15 (48) = happyShift action_16
action_15 (56) = happyShift action_18
action_15 (58) = happyShift action_19
action_15 (59) = happyShift action_20
action_15 (60) = happyShift action_21
action_15 (61) = happyShift action_22
action_15 (62) = happyShift action_23
action_15 (12) = happyGoto action_32
action_15 (13) = happyGoto action_8
action_15 (14) = happyGoto action_9
action_15 _ = happyFail

action_16 (21) = happyShift action_10
action_16 (24) = happyShift action_11
action_16 (27) = happyShift action_12
action_16 (29) = happyShift action_13
action_16 (34) = happyShift action_14
action_16 (46) = happyShift action_15
action_16 (48) = happyShift action_16
action_16 (56) = happyShift action_18
action_16 (58) = happyShift action_19
action_16 (59) = happyShift action_20
action_16 (60) = happyShift action_21
action_16 (61) = happyShift action_22
action_16 (62) = happyShift action_23
action_16 (12) = happyGoto action_31
action_16 (13) = happyGoto action_8
action_16 (14) = happyGoto action_9
action_16 _ = happyFail

action_17 (21) = happyShift action_30
action_17 _ = happyFail

action_18 (21) = happyShift action_29
action_18 _ = happyFail

action_19 (46) = happyShift action_28
action_19 _ = happyFail

action_20 (46) = happyShift action_27
action_20 _ = happyFail

action_21 (43) = happyShift action_26
action_21 _ = happyFail

action_22 (43) = happyShift action_25
action_22 _ = happyFail

action_23 (21) = happyShift action_24
action_23 _ = happyFail

action_24 (44) = happyShift action_83
action_24 _ = happyFail

action_25 _ = happyReduce_28

action_26 _ = happyReduce_27

action_27 (21) = happyShift action_34
action_27 (22) = happyShift action_35
action_27 (34) = happyShift action_36
action_27 (44) = happyShift action_37
action_27 (46) = happyShift action_38
action_27 (18) = happyGoto action_82
action_27 _ = happyFail

action_28 (21) = happyShift action_81
action_28 _ = happyFail

action_29 (46) = happyShift action_80
action_29 _ = happyFail

action_30 (46) = happyShift action_79
action_30 _ = happyFail

action_31 (21) = happyShift action_10
action_31 (24) = happyShift action_11
action_31 (27) = happyShift action_12
action_31 (29) = happyShift action_13
action_31 (34) = happyShift action_14
action_31 (46) = happyShift action_15
action_31 (48) = happyShift action_16
action_31 (49) = happyShift action_78
action_31 (56) = happyShift action_18
action_31 (58) = happyShift action_19
action_31 (59) = happyShift action_20
action_31 (60) = happyShift action_21
action_31 (61) = happyShift action_22
action_31 (62) = happyShift action_23
action_31 (13) = happyGoto action_50
action_31 (14) = happyGoto action_9
action_31 _ = happyFail

action_32 (21) = happyShift action_10
action_32 (24) = happyShift action_11
action_32 (27) = happyShift action_12
action_32 (29) = happyShift action_13
action_32 (34) = happyShift action_14
action_32 (46) = happyShift action_15
action_32 (47) = happyShift action_77
action_32 (48) = happyShift action_16
action_32 (56) = happyShift action_18
action_32 (58) = happyShift action_19
action_32 (59) = happyShift action_20
action_32 (60) = happyShift action_21
action_32 (61) = happyShift action_22
action_32 (62) = happyShift action_23
action_32 (13) = happyGoto action_50
action_32 (14) = happyGoto action_9
action_32 _ = happyFail

action_33 (31) = happyShift action_76
action_33 _ = happyFail

action_34 (44) = happyShift action_75
action_34 _ = happyReduce_42

action_35 _ = happyReduce_41

action_36 (21) = happyShift action_34
action_36 (22) = happyShift action_35
action_36 (34) = happyShift action_36
action_36 (44) = happyShift action_37
action_36 (46) = happyShift action_38
action_36 (18) = happyGoto action_74
action_36 _ = happyFail

action_37 (21) = happyShift action_34
action_37 (22) = happyShift action_35
action_37 (34) = happyShift action_36
action_37 (44) = happyShift action_37
action_37 (46) = happyShift action_38
action_37 (17) = happyGoto action_73
action_37 (18) = happyGoto action_41
action_37 _ = happyFail

action_38 (21) = happyShift action_34
action_38 (22) = happyShift action_35
action_38 (34) = happyShift action_36
action_38 (44) = happyShift action_37
action_38 (46) = happyShift action_38
action_38 (17) = happyGoto action_72
action_38 (18) = happyGoto action_41
action_38 _ = happyFail

action_39 _ = happyReduce_18

action_40 (32) = happyShift action_63
action_40 (33) = happyShift action_64
action_40 (34) = happyShift action_65
action_40 (35) = happyShift action_66
action_40 (38) = happyShift action_67
action_40 (39) = happyShift action_68
action_40 (40) = happyShift action_69
action_40 (41) = happyShift action_70
action_40 (42) = happyShift action_71
action_40 _ = happyFail

action_41 _ = happyReduce_40

action_42 (28) = happyShift action_62
action_42 (36) = happyShift action_56
action_42 (37) = happyShift action_57
action_42 (38) = happyShift action_58
action_42 _ = happyFail

action_43 _ = happyReduce_56

action_44 _ = happyReduce_57

action_45 (21) = happyShift action_34
action_45 (22) = happyShift action_35
action_45 (23) = happyShift action_44
action_45 (30) = happyShift action_45
action_45 (34) = happyShift action_36
action_45 (44) = happyShift action_37
action_45 (46) = happyShift action_46
action_45 (17) = happyGoto action_40
action_45 (18) = happyGoto action_41
action_45 (19) = happyGoto action_61
action_45 (20) = happyGoto action_43
action_45 _ = happyFail

action_46 (21) = happyShift action_34
action_46 (22) = happyShift action_35
action_46 (23) = happyShift action_44
action_46 (30) = happyShift action_45
action_46 (34) = happyShift action_36
action_46 (44) = happyShift action_37
action_46 (46) = happyShift action_46
action_46 (17) = happyGoto action_59
action_46 (18) = happyGoto action_41
action_46 (19) = happyGoto action_60
action_46 (20) = happyGoto action_43
action_46 _ = happyFail

action_47 (25) = happyShift action_55
action_47 (36) = happyShift action_56
action_47 (37) = happyShift action_57
action_47 (38) = happyShift action_58
action_47 _ = happyFail

action_48 (21) = happyShift action_34
action_48 (22) = happyShift action_35
action_48 (23) = happyShift action_44
action_48 (30) = happyShift action_45
action_48 (34) = happyShift action_36
action_48 (44) = happyShift action_37
action_48 (46) = happyShift action_46
action_48 (17) = happyGoto action_53
action_48 (18) = happyGoto action_41
action_48 (19) = happyGoto action_54
action_48 (20) = happyGoto action_43
action_48 _ = happyFail

action_49 (21) = happyShift action_34
action_49 (22) = happyShift action_35
action_49 (34) = happyShift action_36
action_49 (44) = happyShift action_37
action_49 (46) = happyShift action_38
action_49 (17) = happyGoto action_52
action_49 (18) = happyGoto action_41
action_49 _ = happyFail

action_50 _ = happyReduce_12

action_51 _ = happyReduce_2

action_52 (32) = happyShift action_63
action_52 (33) = happyShift action_64
action_52 (34) = happyShift action_65
action_52 (35) = happyShift action_66
action_52 (45) = happyShift action_114
action_52 _ = happyFail

action_53 (32) = happyShift action_63
action_53 (33) = happyShift action_64
action_53 (34) = happyShift action_65
action_53 (35) = happyShift action_66
action_53 (38) = happyShift action_67
action_53 (39) = happyShift action_68
action_53 (40) = happyShift action_69
action_53 (41) = happyShift action_70
action_53 (42) = happyShift action_71
action_53 (43) = happyShift action_113
action_53 _ = happyFail

action_54 (36) = happyShift action_56
action_54 (37) = happyShift action_57
action_54 (38) = happyShift action_58
action_54 (43) = happyShift action_112
action_54 _ = happyFail

action_55 (21) = happyShift action_10
action_55 (29) = happyShift action_13
action_55 (34) = happyShift action_14
action_55 (46) = happyShift action_15
action_55 (48) = happyShift action_16
action_55 (56) = happyShift action_18
action_55 (58) = happyShift action_19
action_55 (59) = happyShift action_20
action_55 (60) = happyShift action_21
action_55 (61) = happyShift action_22
action_55 (62) = happyShift action_23
action_55 (14) = happyGoto action_111
action_55 _ = happyFail

action_56 (21) = happyShift action_34
action_56 (22) = happyShift action_35
action_56 (23) = happyShift action_44
action_56 (30) = happyShift action_45
action_56 (34) = happyShift action_36
action_56 (44) = happyShift action_37
action_56 (46) = happyShift action_46
action_56 (17) = happyGoto action_40
action_56 (18) = happyGoto action_41
action_56 (19) = happyGoto action_110
action_56 (20) = happyGoto action_43
action_56 _ = happyFail

action_57 (21) = happyShift action_34
action_57 (22) = happyShift action_35
action_57 (23) = happyShift action_44
action_57 (30) = happyShift action_45
action_57 (34) = happyShift action_36
action_57 (44) = happyShift action_37
action_57 (46) = happyShift action_46
action_57 (17) = happyGoto action_40
action_57 (18) = happyGoto action_41
action_57 (19) = happyGoto action_109
action_57 (20) = happyGoto action_43
action_57 _ = happyFail

action_58 (21) = happyShift action_34
action_58 (22) = happyShift action_35
action_58 (23) = happyShift action_44
action_58 (30) = happyShift action_45
action_58 (34) = happyShift action_36
action_58 (44) = happyShift action_37
action_58 (46) = happyShift action_46
action_58 (17) = happyGoto action_40
action_58 (18) = happyGoto action_41
action_58 (19) = happyGoto action_108
action_58 (20) = happyGoto action_43
action_58 _ = happyFail

action_59 (32) = happyShift action_63
action_59 (33) = happyShift action_64
action_59 (34) = happyShift action_65
action_59 (35) = happyShift action_66
action_59 (38) = happyShift action_67
action_59 (39) = happyShift action_68
action_59 (40) = happyShift action_69
action_59 (41) = happyShift action_70
action_59 (42) = happyShift action_71
action_59 (47) = happyShift action_96
action_59 _ = happyFail

action_60 (36) = happyShift action_56
action_60 (37) = happyShift action_57
action_60 (38) = happyShift action_58
action_60 (47) = happyShift action_107
action_60 _ = happyFail

action_61 _ = happyReduce_47

action_62 (21) = happyShift action_10
action_62 (29) = happyShift action_13
action_62 (34) = happyShift action_14
action_62 (46) = happyShift action_15
action_62 (48) = happyShift action_16
action_62 (56) = happyShift action_18
action_62 (58) = happyShift action_19
action_62 (59) = happyShift action_20
action_62 (60) = happyShift action_21
action_62 (61) = happyShift action_22
action_62 (62) = happyShift action_23
action_62 (14) = happyGoto action_106
action_62 _ = happyFail

action_63 (21) = happyShift action_34
action_63 (22) = happyShift action_35
action_63 (34) = happyShift action_36
action_63 (44) = happyShift action_37
action_63 (46) = happyShift action_38
action_63 (17) = happyGoto action_105
action_63 (18) = happyGoto action_41
action_63 _ = happyFail

action_64 (21) = happyShift action_34
action_64 (22) = happyShift action_35
action_64 (34) = happyShift action_36
action_64 (44) = happyShift action_37
action_64 (46) = happyShift action_38
action_64 (17) = happyGoto action_104
action_64 (18) = happyGoto action_41
action_64 _ = happyFail

action_65 (21) = happyShift action_34
action_65 (22) = happyShift action_35
action_65 (34) = happyShift action_36
action_65 (44) = happyShift action_37
action_65 (46) = happyShift action_38
action_65 (17) = happyGoto action_103
action_65 (18) = happyGoto action_41
action_65 _ = happyFail

action_66 (21) = happyShift action_34
action_66 (22) = happyShift action_35
action_66 (34) = happyShift action_36
action_66 (44) = happyShift action_37
action_66 (46) = happyShift action_38
action_66 (17) = happyGoto action_102
action_66 (18) = happyGoto action_41
action_66 _ = happyFail

action_67 (21) = happyShift action_34
action_67 (22) = happyShift action_35
action_67 (34) = happyShift action_36
action_67 (44) = happyShift action_37
action_67 (46) = happyShift action_38
action_67 (17) = happyGoto action_101
action_67 (18) = happyGoto action_41
action_67 _ = happyFail

action_68 (21) = happyShift action_34
action_68 (22) = happyShift action_35
action_68 (34) = happyShift action_36
action_68 (44) = happyShift action_37
action_68 (46) = happyShift action_38
action_68 (17) = happyGoto action_100
action_68 (18) = happyGoto action_41
action_68 _ = happyFail

action_69 (21) = happyShift action_34
action_69 (22) = happyShift action_35
action_69 (34) = happyShift action_36
action_69 (44) = happyShift action_37
action_69 (46) = happyShift action_38
action_69 (17) = happyGoto action_99
action_69 (18) = happyGoto action_41
action_69 _ = happyFail

action_70 (21) = happyShift action_34
action_70 (22) = happyShift action_35
action_70 (34) = happyShift action_36
action_70 (44) = happyShift action_37
action_70 (46) = happyShift action_38
action_70 (17) = happyGoto action_98
action_70 (18) = happyGoto action_41
action_70 _ = happyFail

action_71 (21) = happyShift action_34
action_71 (22) = happyShift action_35
action_71 (34) = happyShift action_36
action_71 (44) = happyShift action_37
action_71 (46) = happyShift action_38
action_71 (17) = happyGoto action_97
action_71 (18) = happyGoto action_41
action_71 _ = happyFail

action_72 (32) = happyShift action_63
action_72 (33) = happyShift action_64
action_72 (34) = happyShift action_65
action_72 (35) = happyShift action_66
action_72 (47) = happyShift action_96
action_72 _ = happyFail

action_73 (32) = happyShift action_63
action_73 (33) = happyShift action_64
action_73 (34) = happyShift action_65
action_73 (35) = happyShift action_66
action_73 (45) = happyShift action_95
action_73 _ = happyFail

action_74 _ = happyReduce_45

action_75 (21) = happyShift action_34
action_75 (22) = happyShift action_35
action_75 (34) = happyShift action_36
action_75 (44) = happyShift action_37
action_75 (46) = happyShift action_38
action_75 (17) = happyGoto action_94
action_75 (18) = happyGoto action_41
action_75 _ = happyFail

action_76 (21) = happyShift action_34
action_76 (22) = happyShift action_35
action_76 (34) = happyShift action_36
action_76 (44) = happyShift action_37
action_76 (46) = happyShift action_38
action_76 (18) = happyGoto action_93
action_76 _ = happyFail

action_77 _ = happyReduce_25

action_78 _ = happyReduce_26

action_79 (54) = happyShift action_92
action_79 (8) = happyGoto action_91
action_79 _ = happyReduce_7

action_80 (21) = happyShift action_34
action_80 (22) = happyShift action_35
action_80 (23) = happyShift action_44
action_80 (30) = happyShift action_45
action_80 (34) = happyShift action_36
action_80 (44) = happyShift action_37
action_80 (46) = happyShift action_46
action_80 (15) = happyGoto action_87
action_80 (16) = happyGoto action_88
action_80 (17) = happyGoto action_89
action_80 (18) = happyGoto action_41
action_80 (19) = happyGoto action_90
action_80 (20) = happyGoto action_43
action_80 _ = happyReduce_33

action_81 (57) = happyShift action_86
action_81 _ = happyFail

action_82 (47) = happyShift action_85
action_82 _ = happyFail

action_83 (21) = happyShift action_34
action_83 (22) = happyShift action_35
action_83 (34) = happyShift action_36
action_83 (44) = happyShift action_37
action_83 (46) = happyShift action_38
action_83 (17) = happyGoto action_84
action_83 (18) = happyGoto action_41
action_83 _ = happyFail

action_84 (32) = happyShift action_63
action_84 (33) = happyShift action_64
action_84 (34) = happyShift action_65
action_84 (35) = happyShift action_66
action_84 (45) = happyShift action_127
action_84 _ = happyFail

action_85 (43) = happyShift action_126
action_85 _ = happyFail

action_86 (21) = happyShift action_34
action_86 (22) = happyShift action_35
action_86 (34) = happyShift action_36
action_86 (44) = happyShift action_37
action_86 (46) = happyShift action_38
action_86 (17) = happyGoto action_125
action_86 (18) = happyGoto action_41
action_86 _ = happyFail

action_87 (57) = happyShift action_124
action_87 _ = happyFail

action_88 _ = happyReduce_32

action_89 (32) = happyShift action_63
action_89 (33) = happyShift action_64
action_89 (34) = happyShift action_65
action_89 (35) = happyShift action_66
action_89 (38) = happyShift action_67
action_89 (39) = happyShift action_68
action_89 (40) = happyShift action_69
action_89 (41) = happyShift action_70
action_89 (42) = happyShift action_71
action_89 _ = happyReduce_34

action_90 (36) = happyShift action_56
action_90 (37) = happyShift action_57
action_90 (38) = happyShift action_58
action_90 _ = happyReduce_35

action_91 (55) = happyShift action_123
action_91 (9) = happyGoto action_122
action_91 _ = happyFail

action_92 (21) = happyShift action_121
action_92 (10) = happyGoto action_119
action_92 (11) = happyGoto action_120
action_92 _ = happyFail

action_93 (43) = happyShift action_118
action_93 _ = happyFail

action_94 (32) = happyShift action_63
action_94 (33) = happyShift action_64
action_94 (34) = happyShift action_65
action_94 (35) = happyShift action_66
action_94 (45) = happyShift action_117
action_94 _ = happyFail

action_95 _ = happyReduce_44

action_96 _ = happyReduce_43

action_97 (32) = happyShift action_63
action_97 (33) = happyShift action_64
action_97 (34) = happyShift action_65
action_97 (35) = happyShift action_66
action_97 _ = happyReduce_54

action_98 (32) = happyShift action_63
action_98 (33) = happyShift action_64
action_98 (34) = happyShift action_65
action_98 (35) = happyShift action_66
action_98 _ = happyReduce_55

action_99 (32) = happyShift action_63
action_99 (33) = happyShift action_64
action_99 (34) = happyShift action_65
action_99 (35) = happyShift action_66
action_99 _ = happyReduce_53

action_100 (32) = happyShift action_63
action_100 (33) = happyShift action_64
action_100 (34) = happyShift action_65
action_100 (35) = happyShift action_66
action_100 _ = happyReduce_52

action_101 (32) = happyShift action_63
action_101 (33) = happyShift action_64
action_101 (34) = happyShift action_65
action_101 (35) = happyShift action_66
action_101 _ = happyReduce_51

action_102 _ = happyReduce_39

action_103 _ = happyReduce_38

action_104 (34) = happyShift action_65
action_104 (35) = happyShift action_66
action_104 _ = happyReduce_37

action_105 (34) = happyShift action_65
action_105 (35) = happyShift action_66
action_105 _ = happyReduce_36

action_106 _ = happyReduce_16

action_107 _ = happyReduce_58

action_108 (38) = happyFail
action_108 _ = happyReduce_50

action_109 (36) = happyShift action_56
action_109 (38) = happyShift action_58
action_109 _ = happyReduce_49

action_110 (38) = happyShift action_58
action_110 _ = happyReduce_48

action_111 (26) = happyShift action_116
action_111 _ = happyReduce_15

action_112 _ = happyReduce_20

action_113 _ = happyReduce_19

action_114 (31) = happyShift action_115
action_114 _ = happyFail

action_115 (21) = happyShift action_34
action_115 (22) = happyShift action_35
action_115 (34) = happyShift action_36
action_115 (44) = happyShift action_37
action_115 (46) = happyShift action_38
action_115 (18) = happyGoto action_136
action_115 _ = happyFail

action_116 (21) = happyShift action_10
action_116 (29) = happyShift action_13
action_116 (34) = happyShift action_14
action_116 (46) = happyShift action_15
action_116 (48) = happyShift action_16
action_116 (56) = happyShift action_18
action_116 (58) = happyShift action_19
action_116 (59) = happyShift action_20
action_116 (60) = happyShift action_21
action_116 (61) = happyShift action_22
action_116 (62) = happyShift action_23
action_116 (14) = happyGoto action_135
action_116 _ = happyFail

action_117 _ = happyReduce_46

action_118 _ = happyReduce_21

action_119 (57) = happyShift action_134
action_119 _ = happyFail

action_120 _ = happyReduce_10

action_121 _ = happyReduce_11

action_122 (47) = happyShift action_133
action_122 _ = happyFail

action_123 (21) = happyShift action_121
action_123 (11) = happyGoto action_132
action_123 _ = happyFail

action_124 (21) = happyShift action_131
action_124 (22) = happyShift action_35
action_124 (23) = happyShift action_44
action_124 (30) = happyShift action_45
action_124 (34) = happyShift action_36
action_124 (44) = happyShift action_37
action_124 (46) = happyShift action_46
action_124 (16) = happyGoto action_130
action_124 (17) = happyGoto action_89
action_124 (18) = happyGoto action_41
action_124 (19) = happyGoto action_90
action_124 (20) = happyGoto action_43
action_124 _ = happyFail

action_125 (32) = happyShift action_63
action_125 (33) = happyShift action_64
action_125 (34) = happyShift action_65
action_125 (35) = happyShift action_66
action_125 (47) = happyShift action_129
action_125 _ = happyFail

action_126 _ = happyReduce_24

action_127 (43) = happyShift action_128
action_127 _ = happyFail

action_128 _ = happyReduce_29

action_129 (43) = happyShift action_141
action_129 _ = happyFail

action_130 _ = happyReduce_31

action_131 (44) = happyShift action_75
action_131 (47) = happyShift action_140
action_131 _ = happyReduce_42

action_132 _ = happyReduce_8

action_133 (53) = happyShift action_139
action_133 _ = happyFail

action_134 (21) = happyShift action_121
action_134 (11) = happyGoto action_138
action_134 _ = happyReduce_6

action_135 _ = happyReduce_14

action_136 (43) = happyShift action_137
action_136 _ = happyFail

action_137 _ = happyReduce_30

action_138 _ = happyReduce_9

action_139 (21) = happyShift action_10
action_139 (24) = happyShift action_11
action_139 (27) = happyShift action_12
action_139 (29) = happyShift action_13
action_139 (34) = happyShift action_14
action_139 (46) = happyShift action_15
action_139 (48) = happyShift action_16
action_139 (56) = happyShift action_18
action_139 (58) = happyShift action_19
action_139 (59) = happyShift action_20
action_139 (60) = happyShift action_21
action_139 (61) = happyShift action_22
action_139 (62) = happyShift action_23
action_139 (12) = happyGoto action_143
action_139 (13) = happyGoto action_8
action_139 (14) = happyGoto action_9
action_139 _ = happyFail

action_140 (43) = happyShift action_142
action_140 _ = happyFail

action_141 _ = happyReduce_23

action_142 _ = happyReduce_22

action_143 (21) = happyShift action_10
action_143 (24) = happyShift action_11
action_143 (27) = happyShift action_12
action_143 (29) = happyShift action_13
action_143 (34) = happyShift action_14
action_143 (46) = happyShift action_15
action_143 (48) = happyShift action_16
action_143 (51) = happyShift action_144
action_143 (56) = happyShift action_18
action_143 (58) = happyShift action_19
action_143 (59) = happyShift action_20
action_143 (60) = happyShift action_21
action_143 (61) = happyShift action_22
action_143 (62) = happyShift action_23
action_143 (13) = happyGoto action_50
action_143 (14) = happyGoto action_9
action_143 _ = happyFail

action_144 _ = happyReduce_5

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

happyReduce_21 = happyReduce 5 14 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (RefAssign happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 8 14 happyReduction_22
happyReduction_22 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Call happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 7 14 happyReduction_23
happyReduction_23 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Malloc happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 5 14 happyReduction_24
happyReduction_24 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Free happy_var_3
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  14 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  14 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  14 happyReduction_27
happyReduction_27 _
	_
	 =  HappyAbsSyn14
		 (Continue
	)

happyReduce_28 = happySpecReduce_2  14 happyReduction_28
happyReduction_28 _
	_
	 =  HappyAbsSyn14
		 (Break
	)

happyReduce_29 = happyReduce 6 14 happyReduction_29
happyReduction_29 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Malloc happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 7 14 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (RefAssign (Plus (Var happy_var_1) happy_var_3) happy_var_6
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_3  15 happyReduction_31
happyReduction_31 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [ happy_var_3 ]
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([ happy_var_1 ]
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_0  15 happyReduction_33
happyReduction_33  =  HappyAbsSyn15
		 ([]
	)

happyReduce_34 = happySpecReduce_1  16 happyReduction_34
happyReduction_34 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (I happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  16 happyReduction_35
happyReduction_35 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn16
		 (B happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  17 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  17 happyReduction_37
happyReduction_37 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  17 happyReduction_38
happyReduction_38 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Times happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  17 happyReduction_39
happyReduction_39 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Divide happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  17 happyReduction_40
happyReduction_40 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  18 happyReduction_41
happyReduction_41 (HappyTerminal (TInt happy_var_1))
	 =  HappyAbsSyn18
		 (IConst happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  18 happyReduction_42
happyReduction_42 (HappyTerminal (TIdent happy_var_1))
	 =  HappyAbsSyn18
		 (Var happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  18 happyReduction_43
happyReduction_43 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  18 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  18 happyReduction_45
happyReduction_45 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (Deref happy_var_2
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happyReduce 4 18 happyReduction_46
happyReduction_46 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Deref (Plus (Var happy_var_1) happy_var_3)
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_2  19 happyReduction_47
happyReduction_47 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (Not happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  19 happyReduction_48
happyReduction_48 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (And happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  19 happyReduction_49
happyReduction_49 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Or happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  19 happyReduction_50
happyReduction_50 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (BEqual happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  19 happyReduction_51
happyReduction_51 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (IEqual happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  19 happyReduction_52
happyReduction_52 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (LessThan happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  19 happyReduction_53
happyReduction_53 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (GreaterThan happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  19 happyReduction_54
happyReduction_54 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (GreaterEqual happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  19 happyReduction_55
happyReduction_55 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 (LessEqual happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  19 happyReduction_56
happyReduction_56 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  20 happyReduction_57
happyReduction_57 (HappyTerminal (TBool happy_var_1))
	 =  HappyAbsSyn20
		 (BConst happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  20 happyReduction_58
happyReduction_58 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_58 _ _ _  = notHappyAtAll 

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
