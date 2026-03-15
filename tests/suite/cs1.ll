; ModuleID = 'cs1.c'
source_filename = "cs1.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%union.U1 = type { ptr }
%struct.S0 = type { [13 x i8] }

@.str = private unnamed_addr constant [2 x i8] c"1\00", align 1
@g_14 = internal global [8 x i64] [i64 1, i64 1, i64 1, i64 1, i64 1, i64 1, i64 1, i64 1], align 16
@.str.1 = private unnamed_addr constant [8 x i8] c"g_14[i]\00", align 1
@.str.2 = private unnamed_addr constant [14 x i8] c"index = [%d]\0A\00", align 1
@g_17 = internal global [7 x [1 x [8 x i32]]] [[1 x [8 x i32]] [[8 x i32] [i32 -1, i32 2, i32 2, i32 -1, i32 2, i32 2, i32 -1, i32 2]], [1 x [8 x i32]] [[8 x i32] [i32 -1, i32 -1, i32 234790728, i32 -1, i32 -1, i32 234790728, i32 -1, i32 -1]], [1 x [8 x i32]] [[8 x i32] [i32 2, i32 -1, i32 2, i32 2, i32 -1, i32 2, i32 2, i32 -1]], [1 x [8 x i32]] [[8 x i32] [i32 -1, i32 2, i32 2, i32 -1, i32 2, i32 2, i32 -1, i32 2]], [1 x [8 x i32]] [[8 x i32] [i32 -1, i32 -1, i32 234790728, i32 -1, i32 -1, i32 234790728, i32 2, i32 2]], [1 x [8 x i32]] [[8 x i32] [i32 234790728, i32 2, i32 234790728, i32 234790728, i32 2, i32 234790728, i32 234790728, i32 2]], [1 x [8 x i32]] [[8 x i32] [i32 2, i32 234790728, i32 234790728, i32 2, i32 234790728, i32 234790728, i32 2, i32 234790728]]], align 16
@.str.3 = private unnamed_addr constant [14 x i8] c"g_17[i][j][k]\00", align 1
@.str.4 = private unnamed_addr constant [22 x i8] c"index = [%d][%d][%d]\0A\00", align 1
@g_45 = internal global i16 28311, align 2
@.str.5 = private unnamed_addr constant [5 x i8] c"g_45\00", align 1
@g_71 = internal global i32 530546753, align 4
@.str.6 = private unnamed_addr constant [5 x i8] c"g_71\00", align 1
@g_72 = internal global i32 8, align 4
@.str.7 = private unnamed_addr constant [5 x i8] c"g_72\00", align 1
@g_81 = internal global i16 7, align 2
@.str.8 = private unnamed_addr constant [5 x i8] c"g_81\00", align 1
@g_88 = internal global i8 66, align 1
@.str.9 = private unnamed_addr constant [5 x i8] c"g_88\00", align 1
@g_118 = internal global i8 -83, align 1
@.str.10 = private unnamed_addr constant [6 x i8] c"g_118\00", align 1
@g_123 = internal global i32 1762784094, align 4
@.str.11 = private unnamed_addr constant [6 x i8] c"g_123\00", align 1
@g_147 = internal global [3 x i32] [i32 982159620, i32 982159620, i32 982159620], align 4
@.str.12 = private unnamed_addr constant [9 x i8] c"g_147[i]\00", align 1
@g_160 = internal global [9 x [4 x [2 x i64]]] [[4 x [2 x i64]] [[2 x i64] [i64 -8348877822458546318, i64 -1], [2 x i64] [i64 -4207163098428392644, i64 -1], [2 x i64] [i64 -4207163098428392644, i64 -1], [2 x i64] [i64 -8348877822458546318, i64 -4207163098428392644]], [4 x [2 x i64]] [[2 x i64] [i64 -1, i64 -1], [2 x i64] [i64 1, i64 1], [2 x i64] [i64 -8348877822458546318, i64 1], [2 x i64] [i64 1, i64 -1]], [4 x [2 x i64]] [[2 x i64] [i64 -1, i64 -4207163098428392644], [2 x i64] [i64 -8348877822458546318, i64 -1], [2 x i64] [i64 -4207163098428392644, i64 -1], [2 x i64] [i64 -4207163098428392644, i64 -1]], [4 x [2 x i64]] [[2 x i64] [i64 -8348877822458546318, i64 -4207163098428392644], [2 x i64] [i64 -1, i64 -1], [2 x i64] [i64 1, i64 1], [2 x i64] [i64 -8348877822458546318, i64 1]], [4 x [2 x i64]] [[2 x i64] [i64 1, i64 -1], [2 x i64] [i64 -1, i64 -4207163098428392644], [2 x i64] [i64 -8348877822458546318, i64 -1], [2 x i64] [i64 -4207163098428392644, i64 -1]], [4 x [2 x i64]] [[2 x i64] [i64 -4207163098428392644, i64 -1], [2 x i64] [i64 -8348877822458546318, i64 -4207163098428392644], [2 x i64] [i64 -1, i64 -1], [2 x i64] [i64 1, i64 1]], [4 x [2 x i64]] [[2 x i64] [i64 -8348877822458546318, i64 1], [2 x i64] [i64 1, i64 -1], [2 x i64] [i64 -1, i64 -4207163098428392644], [2 x i64] [i64 -8348877822458546318, i64 -1]], [4 x [2 x i64]] [[2 x i64] [i64 -4207163098428392644, i64 -1], [2 x i64] [i64 -4207163098428392644, i64 -1], [2 x i64] [i64 -8348877822458546318, i64 -4207163098428392644], [2 x i64] [i64 -1, i64 -1]], [4 x [2 x i64]] [[2 x i64] [i64 1, i64 1], [2 x i64] [i64 -8348877822458546318, i64 1], [2 x i64] [i64 1, i64 -1], [2 x i64] [i64 -1, i64 -4207163098428392644]]], align 16
@.str.13 = private unnamed_addr constant [15 x i8] c"g_160[i][j][k]\00", align 1
@.str.14 = private unnamed_addr constant [9 x i8] c"g_163.f0\00", align 1
@.str.15 = private unnamed_addr constant [9 x i8] c"g_163.f1\00", align 1
@.str.16 = private unnamed_addr constant [9 x i8] c"g_163.f2\00", align 1
@.str.17 = private unnamed_addr constant [9 x i8] c"g_163.f3\00", align 1
@.str.18 = private unnamed_addr constant [9 x i8] c"g_163.f4\00", align 1
@g_199 = internal global [1 x [2 x [2 x i8]]] [[2 x [2 x i8]] [[2 x i8] c"\89\89", [2 x i8] c"\89\89"]], align 1
@.str.19 = private unnamed_addr constant [15 x i8] c"g_199[i][j][k]\00", align 1
@g_220 = internal global i16 0, align 2
@.str.20 = private unnamed_addr constant [6 x i8] c"g_220\00", align 1
@g_235 = internal global i16 -23924, align 2
@.str.21 = private unnamed_addr constant [6 x i8] c"g_235\00", align 1
@g_236 = internal global i32 -3, align 4
@.str.22 = private unnamed_addr constant [6 x i8] c"g_236\00", align 1
@g_237 = internal global i8 -45, align 1
@.str.23 = private unnamed_addr constant [6 x i8] c"g_237\00", align 1
@g_238 = internal global i8 -3, align 1
@.str.24 = private unnamed_addr constant [6 x i8] c"g_238\00", align 1
@.str.25 = private unnamed_addr constant [9 x i8] c"g_248.f0\00", align 1
@.str.26 = private unnamed_addr constant [9 x i8] c"g_248.f1\00", align 1
@.str.27 = private unnamed_addr constant [9 x i8] c"g_248.f2\00", align 1
@.str.28 = private unnamed_addr constant [9 x i8] c"g_248.f3\00", align 1
@.str.29 = private unnamed_addr constant [9 x i8] c"g_248.f4\00", align 1
@g_294 = internal global i64 -8, align 8
@.str.30 = private unnamed_addr constant [6 x i8] c"g_294\00", align 1
@g_303 = internal global [5 x i8] c"\E0\E0\E0\E0\E0", align 1
@.str.31 = private unnamed_addr constant [9 x i8] c"g_303[i]\00", align 1
@.str.32 = private unnamed_addr constant [9 x i8] c"g_310.f0\00", align 1
@.str.33 = private unnamed_addr constant [9 x i8] c"g_310.f1\00", align 1
@.str.34 = private unnamed_addr constant [9 x i8] c"g_310.f2\00", align 1
@.str.35 = private unnamed_addr constant [9 x i8] c"g_310.f3\00", align 1
@.str.36 = private unnamed_addr constant [9 x i8] c"g_310.f4\00", align 1
@g_355 = internal global i64 -1, align 8
@.str.37 = private unnamed_addr constant [6 x i8] c"g_355\00", align 1
@.str.38 = private unnamed_addr constant [12 x i8] c"g_365[i].f0\00", align 1
@.str.39 = private unnamed_addr constant [12 x i8] c"g_365[i].f1\00", align 1
@.str.40 = private unnamed_addr constant [12 x i8] c"g_365[i].f2\00", align 1
@.str.41 = private unnamed_addr constant [12 x i8] c"g_365[i].f3\00", align 1
@.str.42 = private unnamed_addr constant [12 x i8] c"g_365[i].f4\00", align 1
@.str.43 = private unnamed_addr constant [12 x i8] c"g_473[i].f0\00", align 1
@.str.44 = private unnamed_addr constant [12 x i8] c"g_473[i].f2\00", align 1
@.str.45 = private unnamed_addr constant [12 x i8] c"g_473[i].f3\00", align 1
@g_486 = internal global i32 161903464, align 4
@.str.46 = private unnamed_addr constant [6 x i8] c"g_486\00", align 1
@.str.47 = private unnamed_addr constant [9 x i8] c"g_516.f0\00", align 1
@.str.48 = private unnamed_addr constant [9 x i8] c"g_516.f2\00", align 1
@.str.49 = private unnamed_addr constant [9 x i8] c"g_516.f3\00", align 1
@.str.50 = private unnamed_addr constant [9 x i8] c"g_542.f0\00", align 1
@.str.51 = private unnamed_addr constant [9 x i8] c"g_542.f1\00", align 1
@.str.52 = private unnamed_addr constant [9 x i8] c"g_542.f2\00", align 1
@.str.53 = private unnamed_addr constant [9 x i8] c"g_542.f3\00", align 1
@.str.54 = private unnamed_addr constant [9 x i8] c"g_542.f4\00", align 1
@.str.55 = private unnamed_addr constant [9 x i8] c"g_567.f0\00", align 1
@.str.56 = private unnamed_addr constant [9 x i8] c"g_567.f2\00", align 1
@.str.57 = private unnamed_addr constant [9 x i8] c"g_567.f3\00", align 1
@g_584 = internal global i16 7, align 2
@.str.58 = private unnamed_addr constant [6 x i8] c"g_584\00", align 1
@.str.59 = private unnamed_addr constant [9 x i8] c"g_590.f0\00", align 1
@.str.60 = private unnamed_addr constant [9 x i8] c"g_590.f1\00", align 1
@.str.61 = private unnamed_addr constant [9 x i8] c"g_590.f2\00", align 1
@.str.62 = private unnamed_addr constant [9 x i8] c"g_590.f3\00", align 1
@.str.63 = private unnamed_addr constant [9 x i8] c"g_590.f4\00", align 1
@.str.64 = private unnamed_addr constant [6 x i8] c"g_657\00", align 1
@.str.65 = private unnamed_addr constant [9 x i8] c"g_671.f0\00", align 1
@.str.66 = private unnamed_addr constant [9 x i8] c"g_671.f1\00", align 1
@.str.67 = private unnamed_addr constant [9 x i8] c"g_671.f2\00", align 1
@.str.68 = private unnamed_addr constant [9 x i8] c"g_671.f3\00", align 1
@.str.69 = private unnamed_addr constant [9 x i8] c"g_671.f4\00", align 1
@g_723 = internal global i8 0, align 1
@.str.70 = private unnamed_addr constant [6 x i8] c"g_723\00", align 1
@g_782 = internal global i64 0, align 8
@.str.71 = private unnamed_addr constant [6 x i8] c"g_782\00", align 1
@.str.72 = private unnamed_addr constant [6 x i8] c"g_804\00", align 1
@g_833 = internal global i16 6, align 2
@.str.73 = private unnamed_addr constant [6 x i8] c"g_833\00", align 1
@.str.74 = private unnamed_addr constant [9 x i8] c"g_839.f0\00", align 1
@.str.75 = private unnamed_addr constant [9 x i8] c"g_839.f1\00", align 1
@.str.76 = private unnamed_addr constant [9 x i8] c"g_839.f2\00", align 1
@.str.77 = private unnamed_addr constant [9 x i8] c"g_839.f3\00", align 1
@.str.78 = private unnamed_addr constant [9 x i8] c"g_839.f4\00", align 1
@.str.79 = private unnamed_addr constant [9 x i8] c"g_842.f0\00", align 1
@.str.80 = private unnamed_addr constant [9 x i8] c"g_842.f1\00", align 1
@.str.81 = private unnamed_addr constant [9 x i8] c"g_842.f2\00", align 1
@.str.82 = private unnamed_addr constant [9 x i8] c"g_842.f3\00", align 1
@.str.83 = private unnamed_addr constant [9 x i8] c"g_842.f4\00", align 1
@g_880 = internal global [7 x i32] [i32 0, i32 -10, i32 -10, i32 0, i32 -10, i32 -10, i32 0], align 16
@.str.84 = private unnamed_addr constant [9 x i8] c"g_880[i]\00", align 1
@.str.85 = private unnamed_addr constant [9 x i8] c"g_923.f0\00", align 1
@.str.86 = private unnamed_addr constant [9 x i8] c"g_923.f1\00", align 1
@.str.87 = private unnamed_addr constant [9 x i8] c"g_923.f2\00", align 1
@.str.88 = private unnamed_addr constant [9 x i8] c"g_923.f3\00", align 1
@.str.89 = private unnamed_addr constant [9 x i8] c"g_923.f4\00", align 1
@.str.90 = private unnamed_addr constant [9 x i8] c"g_924.f0\00", align 1
@.str.91 = private unnamed_addr constant [9 x i8] c"g_924.f1\00", align 1
@.str.92 = private unnamed_addr constant [9 x i8] c"g_924.f2\00", align 1
@.str.93 = private unnamed_addr constant [9 x i8] c"g_924.f3\00", align 1
@.str.94 = private unnamed_addr constant [9 x i8] c"g_924.f4\00", align 1
@.str.95 = private unnamed_addr constant [9 x i8] c"g_936.f0\00", align 1
@.str.96 = private unnamed_addr constant [9 x i8] c"g_936.f1\00", align 1
@.str.97 = private unnamed_addr constant [9 x i8] c"g_936.f2\00", align 1
@.str.98 = private unnamed_addr constant [9 x i8] c"g_936.f3\00", align 1
@.str.99 = private unnamed_addr constant [9 x i8] c"g_936.f4\00", align 1
@g_1026 = internal global i32 -1, align 4
@.str.100 = private unnamed_addr constant [7 x i8] c"g_1026\00", align 1
@.str.101 = private unnamed_addr constant [10 x i8] c"g_1038.f0\00", align 1
@.str.102 = private unnamed_addr constant [10 x i8] c"g_1038.f1\00", align 1
@.str.103 = private unnamed_addr constant [10 x i8] c"g_1038.f2\00", align 1
@.str.104 = private unnamed_addr constant [10 x i8] c"g_1038.f3\00", align 1
@.str.105 = private unnamed_addr constant [10 x i8] c"g_1038.f4\00", align 1
@.str.106 = private unnamed_addr constant [10 x i8] c"g_1152.f0\00", align 1
@.str.107 = private unnamed_addr constant [10 x i8] c"g_1152.f1\00", align 1
@.str.108 = private unnamed_addr constant [10 x i8] c"g_1152.f2\00", align 1
@.str.109 = private unnamed_addr constant [10 x i8] c"g_1152.f3\00", align 1
@.str.110 = private unnamed_addr constant [10 x i8] c"g_1152.f4\00", align 1
@.str.111 = private unnamed_addr constant [10 x i8] c"g_1191.f0\00", align 1
@.str.112 = private unnamed_addr constant [10 x i8] c"g_1191.f2\00", align 1
@.str.113 = private unnamed_addr constant [10 x i8] c"g_1191.f3\00", align 1
@g_1245 = internal global i8 -1, align 1
@.str.114 = private unnamed_addr constant [7 x i8] c"g_1245\00", align 1
@.str.115 = private unnamed_addr constant [16 x i8] c"g_1263[i][j].f0\00", align 1
@.str.116 = private unnamed_addr constant [16 x i8] c"g_1263[i][j].f2\00", align 1
@.str.117 = private unnamed_addr constant [16 x i8] c"g_1263[i][j].f3\00", align 1
@.str.118 = private unnamed_addr constant [18 x i8] c"index = [%d][%d]\0A\00", align 1
@g_1303 = internal constant i64 8186615698239080203, align 8
@.str.119 = private unnamed_addr constant [7 x i8] c"g_1303\00", align 1
@g_1304 = internal global i64 -2, align 8
@.str.120 = private unnamed_addr constant [7 x i8] c"g_1304\00", align 1
@g_1305 = internal global i64 -1, align 8
@.str.121 = private unnamed_addr constant [7 x i8] c"g_1305\00", align 1
@g_1306 = internal constant i64 -10, align 8
@.str.122 = private unnamed_addr constant [7 x i8] c"g_1306\00", align 1
@g_1307 = internal global i64 0, align 8
@.str.123 = private unnamed_addr constant [7 x i8] c"g_1307\00", align 1
@g_1308 = internal constant i64 -1, align 8
@.str.124 = private unnamed_addr constant [7 x i8] c"g_1308\00", align 1
@g_1309 = internal constant i64 -1134361235615639718, align 8
@.str.125 = private unnamed_addr constant [7 x i8] c"g_1309\00", align 1
@g_1310 = internal constant i64 1, align 8
@.str.126 = private unnamed_addr constant [7 x i8] c"g_1310\00", align 1
@g_1311 = internal constant [6 x i64] [i64 8913906122936537332, i64 8913906122936537332, i64 8913906122936537332, i64 8913906122936537332, i64 8913906122936537332, i64 8913906122936537332], align 16
@.str.127 = private unnamed_addr constant [10 x i8] c"g_1311[i]\00", align 1
@g_1312 = internal constant [8 x i64] [i64 -6157611924979448263, i64 -6157611924979448263, i64 -6157611924979448263, i64 -6157611924979448263, i64 -6157611924979448263, i64 -6157611924979448263, i64 -6157611924979448263, i64 -6157611924979448263], align 16
@.str.128 = private unnamed_addr constant [10 x i8] c"g_1312[i]\00", align 1
@g_1313 = internal global i64 0, align 8
@.str.129 = private unnamed_addr constant [7 x i8] c"g_1313\00", align 1
@g_1314 = internal global i64 -5348207412404180359, align 8
@.str.130 = private unnamed_addr constant [7 x i8] c"g_1314\00", align 1
@g_1315 = internal global i64 1, align 8
@.str.131 = private unnamed_addr constant [7 x i8] c"g_1315\00", align 1
@.str.132 = private unnamed_addr constant [13 x i8] c"g_1412[i].f0\00", align 1
@.str.133 = private unnamed_addr constant [13 x i8] c"g_1412[i].f1\00", align 1
@.str.134 = private unnamed_addr constant [13 x i8] c"g_1412[i].f2\00", align 1
@.str.135 = private unnamed_addr constant [13 x i8] c"g_1412[i].f3\00", align 1
@.str.136 = private unnamed_addr constant [13 x i8] c"g_1412[i].f4\00", align 1
@.str.137 = private unnamed_addr constant [13 x i8] c"g_1438[i].f0\00", align 1
@.str.138 = private unnamed_addr constant [13 x i8] c"g_1438[i].f1\00", align 1
@.str.139 = private unnamed_addr constant [13 x i8] c"g_1438[i].f2\00", align 1
@.str.140 = private unnamed_addr constant [13 x i8] c"g_1438[i].f3\00", align 1
@.str.141 = private unnamed_addr constant [13 x i8] c"g_1438[i].f4\00", align 1
@.str.142 = private unnamed_addr constant [10 x i8] c"g_1484.f0\00", align 1
@.str.143 = private unnamed_addr constant [10 x i8] c"g_1484.f1\00", align 1
@.str.144 = private unnamed_addr constant [10 x i8] c"g_1484.f2\00", align 1
@.str.145 = private unnamed_addr constant [10 x i8] c"g_1484.f3\00", align 1
@.str.146 = private unnamed_addr constant [10 x i8] c"g_1484.f4\00", align 1
@g_1531 = internal global [5 x %union.U1] zeroinitializer, align 16
@.str.147 = private unnamed_addr constant [13 x i8] c"g_1531[i].f0\00", align 1
@.str.148 = private unnamed_addr constant [13 x i8] c"g_1531[i].f2\00", align 1
@.str.149 = private unnamed_addr constant [13 x i8] c"g_1531[i].f3\00", align 1
@.str.150 = private unnamed_addr constant [10 x i8] c"g_1608.f0\00", align 1
@.str.151 = private unnamed_addr constant [10 x i8] c"g_1608.f1\00", align 1
@.str.152 = private unnamed_addr constant [10 x i8] c"g_1608.f2\00", align 1
@.str.153 = private unnamed_addr constant [10 x i8] c"g_1608.f3\00", align 1
@.str.154 = private unnamed_addr constant [10 x i8] c"g_1608.f4\00", align 1
@g_1688 = internal global i32 0, align 4
@.str.155 = private unnamed_addr constant [7 x i8] c"g_1688\00", align 1
@g_1716 = internal global i64 5216439508895471, align 8
@.str.156 = private unnamed_addr constant [7 x i8] c"g_1716\00", align 1
@g_1717 = internal global i64 9199987561907470309, align 8
@.str.157 = private unnamed_addr constant [7 x i8] c"g_1717\00", align 1
@.str.158 = private unnamed_addr constant [10 x i8] c"g_1780.f0\00", align 1
@.str.159 = private unnamed_addr constant [10 x i8] c"g_1780.f1\00", align 1
@.str.160 = private unnamed_addr constant [10 x i8] c"g_1780.f2\00", align 1
@.str.161 = private unnamed_addr constant [10 x i8] c"g_1780.f3\00", align 1
@.str.162 = private unnamed_addr constant [10 x i8] c"g_1780.f4\00", align 1
@.str.163 = private unnamed_addr constant [16 x i8] c"g_1797[i][j].f0\00", align 1
@.str.164 = private unnamed_addr constant [16 x i8] c"g_1797[i][j].f1\00", align 1
@.str.165 = private unnamed_addr constant [16 x i8] c"g_1797[i][j].f2\00", align 1
@.str.166 = private unnamed_addr constant [16 x i8] c"g_1797[i][j].f3\00", align 1
@.str.167 = private unnamed_addr constant [16 x i8] c"g_1797[i][j].f4\00", align 1
@g_1990 = internal global i64 0, align 8
@.str.168 = private unnamed_addr constant [7 x i8] c"g_1990\00", align 1
@.str.169 = private unnamed_addr constant [10 x i8] c"g_1999.f0\00", align 1
@.str.170 = private unnamed_addr constant [10 x i8] c"g_1999.f1\00", align 1
@.str.171 = private unnamed_addr constant [10 x i8] c"g_1999.f2\00", align 1
@.str.172 = private unnamed_addr constant [10 x i8] c"g_1999.f3\00", align 1
@.str.173 = private unnamed_addr constant [10 x i8] c"g_1999.f4\00", align 1
@.str.174 = private unnamed_addr constant [10 x i8] c"g_2067.f0\00", align 1
@.str.175 = private unnamed_addr constant [10 x i8] c"g_2067.f2\00", align 1
@.str.176 = private unnamed_addr constant [10 x i8] c"g_2067.f3\00", align 1
@.str.177 = private unnamed_addr constant [16 x i8] c"g_2093[i][j].f0\00", align 1
@.str.178 = private unnamed_addr constant [16 x i8] c"g_2093[i][j].f1\00", align 1
@.str.179 = private unnamed_addr constant [16 x i8] c"g_2093[i][j].f2\00", align 1
@.str.180 = private unnamed_addr constant [16 x i8] c"g_2093[i][j].f3\00", align 1
@.str.181 = private unnamed_addr constant [16 x i8] c"g_2093[i][j].f4\00", align 1
@.str.182 = private unnamed_addr constant [10 x i8] c"g_2094.f0\00", align 1
@.str.183 = private unnamed_addr constant [10 x i8] c"g_2094.f1\00", align 1
@.str.184 = private unnamed_addr constant [10 x i8] c"g_2094.f2\00", align 1
@.str.185 = private unnamed_addr constant [10 x i8] c"g_2094.f3\00", align 1
@.str.186 = private unnamed_addr constant [10 x i8] c"g_2094.f4\00", align 1
@.str.187 = private unnamed_addr constant [13 x i8] c"g_2109[i].f0\00", align 1
@.str.188 = private unnamed_addr constant [13 x i8] c"g_2109[i].f1\00", align 1
@.str.189 = private unnamed_addr constant [13 x i8] c"g_2109[i].f2\00", align 1
@.str.190 = private unnamed_addr constant [13 x i8] c"g_2109[i].f3\00", align 1
@.str.191 = private unnamed_addr constant [13 x i8] c"g_2109[i].f4\00", align 1
@g_2130 = internal global [7 x [6 x i32]] [[6 x i32] [i32 1, i32 1882149448, i32 1, i32 -2, i32 -3, i32 -6], [6 x i32] [i32 129528944, i32 -1262749534, i32 -1117292445, i32 1, i32 1, i32 -1117292445], [6 x i32] [i32 1, i32 1, i32 8, i32 1, i32 -1262749534, i32 -2], [6 x i32] [i32 129528944, i32 8, i32 -6, i32 -2, i32 -6, i32 8], [6 x i32] [i32 1, i32 129528944, i32 -6, i32 3, i32 1, i32 -2], [6 x i32] [i32 -1117292445, i32 3, i32 8, i32 8, i32 3, i32 -1117292445], [6 x i32] [i32 8, i32 3, i32 -1117292445, i32 -3, i32 1, i32 -6]], align 16
@.str.192 = private unnamed_addr constant [13 x i8] c"g_2130[i][j]\00", align 1
@.str.193 = private unnamed_addr constant [10 x i8] c"g_2157.f0\00", align 1
@.str.194 = private unnamed_addr constant [10 x i8] c"g_2157.f1\00", align 1
@.str.195 = private unnamed_addr constant [10 x i8] c"g_2157.f2\00", align 1
@.str.196 = private unnamed_addr constant [10 x i8] c"g_2157.f3\00", align 1
@.str.197 = private unnamed_addr constant [10 x i8] c"g_2157.f4\00", align 1
@.str.198 = private unnamed_addr constant [10 x i8] c"g_2273.f0\00", align 1
@.str.199 = private unnamed_addr constant [10 x i8] c"g_2273.f2\00", align 1
@.str.200 = private unnamed_addr constant [10 x i8] c"g_2273.f3\00", align 1
@.str.201 = private unnamed_addr constant [10 x i8] c"g_2297.f0\00", align 1
@.str.202 = private unnamed_addr constant [10 x i8] c"g_2297.f1\00", align 1
@.str.203 = private unnamed_addr constant [10 x i8] c"g_2297.f2\00", align 1
@.str.204 = private unnamed_addr constant [10 x i8] c"g_2297.f3\00", align 1
@.str.205 = private unnamed_addr constant [10 x i8] c"g_2297.f4\00", align 1
@crc32_context = internal global i32 -1, align 4
@crc32_tab = internal global [256 x i32] zeroinitializer, align 16
@constinit = private constant [10 x ptr] [ptr @g_1026, ptr @g_147, ptr null, ptr @g_147, ptr @g_1026, ptr null, ptr @g_1026, ptr @g_1026, ptr null, ptr @g_1026], align 8
@g_1584 = internal global ptr @g_296, align 8
@g_2257 = internal global ptr getelementptr (i8, ptr @g_1438, i64 104), align 8
@g_2147 = internal global ptr @g_486, align 8
@g_1015 = internal global ptr @g_804, align 8
@g_2113 = internal global ptr @g_837, align 8
@__const.func_1.l_2310 = private unnamed_addr constant [8 x ptr] [ptr @g_2147, ptr @g_2147, ptr @g_2147, ptr @g_2147, ptr @g_2147, ptr @g_2147, ptr @g_2147, ptr @g_2147], align 16
@g_837 = internal global ptr @g_544, align 8
@g_1013 = internal global ptr @g_1014, align 8
@g_910 = internal global ptr @g_310, align 8
@g_296 = internal global ptr null, align 8
@__const.func_2.l_2208 = private unnamed_addr constant [9 x ptr] [ptr getelementptr (i8, ptr @g_1438, i64 65), ptr getelementptr (i8, ptr @g_1438, i64 65), ptr getelementptr (i8, ptr @g_1438, i64 65), ptr getelementptr (i8, ptr @g_1438, i64 65), ptr getelementptr (i8, ptr @g_1438, i64 65), ptr getelementptr (i8, ptr @g_1438, i64 65), ptr getelementptr (i8, ptr @g_1438, i64 65), ptr getelementptr (i8, ptr @g_1438, i64 65), ptr getelementptr (i8, ptr @g_1438, i64 65)], align 16
@__const.func_2.l_2224 = private unnamed_addr constant [6 x [2 x [8 x i32]]] [[2 x [8 x i32]] [[8 x i32] [i32 322230864, i32 -1, i32 9, i32 1997061839, i32 -617561264, i32 -617561264, i32 1997061839, i32 9], [8 x i32] [i32 294153858, i32 294153858, i32 9, i32 0, i32 1, i32 -447054831, i32 0, i32 -6]], [2 x [8 x i32]] [[8 x i32] [i32 1997061839, i32 0, i32 322230864, i32 9, i32 322230864, i32 0, i32 1997061839, i32 -6], [8 x i32] [i32 0, i32 -617561264, i32 1989786726, i32 -1, i32 454171122, i32 9, i32 9, i32 454171122]], [2 x [8 x i32]] [[8 x i32] [i32 294153858, i32 1997061839, i32 1997061839, i32 294153858, i32 454171122, i32 208018501, i32 -6, i32 9], [8 x i32] [i32 0, i32 1, i32 -447054831, i32 454171122, i32 322230864, i32 454171122, i32 -447054831, i32 1]], [2 x [8 x i32]] [[8 x i32] [i32 1997061839, i32 1, i32 9, i32 -447054831, i32 1989786726, i32 208018501, i32 -1, i32 -1], [8 x i32] [i32 9, i32 1997061839, i32 -617561264, i32 -617561264, i32 1997061839, i32 9, i32 -1, i32 322230864]], [2 x [8 x i32]] [[8 x i32] [i32 -6, i32 -617561264, i32 9, i32 208018501, i32 -447054831, i32 0, i32 -447054831, i32 208018501], [8 x i32] [i32 -447054831, i32 0, i32 -447054831, i32 208018501, i32 9, i32 -617561264, i32 -6, i32 322230864]], [2 x [8 x i32]] [[8 x i32] [i32 -1, i32 9, i32 1997061839, i32 -617561264, i32 -617561264, i32 1997061839, i32 9, i32 -1], [8 x i32] [i32 -1, i32 208018501, i32 1989786726, i32 -447054831, i32 9, i32 1, i32 1997061839, i32 1]]], align 16
@g_2065 = internal global ptr @g_2066, align 8
@g_2112 = internal global ptr @g_2113, align 8
@g_82 = internal global ptr @g_81, align 8
@g_1329 = internal global ptr @g_1330, align 8
@g_2251 = internal global ptr @g_641, align 8
@g_641 = internal global ptr null, align 8
@g_1787 = internal global ptr getelementptr (i8, ptr @g_880, i64 4), align 8
@g_164 = internal global ptr @g_163, align 8
@g_2066 = internal global ptr @g_2067, align 8
@g_544 = internal global ptr @g_238, align 8
@__const.func_52.l_987 = private unnamed_addr constant [7 x [2 x ptr]] [[2 x ptr] [ptr null, ptr @g_723], [2 x ptr] zeroinitializer, [2 x ptr] [ptr @g_723, ptr null], [2 x ptr] [ptr null, ptr @g_723], [2 x ptr] zeroinitializer, [2 x ptr] [ptr @g_723, ptr null], [2 x ptr] [ptr null, ptr @g_723]], align 16
@g_802 = internal global ptr @g_803, align 8
@g_1040 = internal global ptr @g_1041, align 8
@g_1330 = internal global ptr null, align 8
@g_90 = internal global ptr @g_91, align 8
@__const.func_52.l_959 = private unnamed_addr constant [10 x i8] c"\FD\05/\05\FD\059\9B9\05", align 1
@__const.func_52.l_931 = private unnamed_addr constant [7 x [7 x ptr]] [[7 x ptr] [ptr @g_237, ptr @g_303, ptr @g_303, ptr @g_237, ptr @g_237, ptr @g_118, ptr @g_118], [7 x ptr] [ptr @g_118, ptr @g_303, ptr @g_303, ptr @g_303, ptr @g_118, ptr @g_237, ptr @g_303], [7 x ptr] [ptr @g_118, ptr null, ptr @g_118, ptr @g_303, ptr @g_118, ptr null, ptr @g_118], [7 x ptr] [ptr @g_303, ptr @g_303, ptr null, ptr @g_303, ptr @g_118, ptr null, ptr @g_303], [7 x ptr] [ptr @g_723, ptr null, ptr @g_303, ptr @g_118, ptr @g_118, ptr @g_118, ptr @g_118], [7 x ptr] [ptr null, ptr null, ptr null, ptr null, ptr @g_237, ptr @g_723, ptr null], [7 x ptr] [ptr null, ptr @g_303, ptr @g_118, ptr null, ptr @g_303, ptr null, ptr @g_723]], align 16
@__const.func_52.l_941 = private unnamed_addr constant [5 x [1 x ptr]] [[1 x ptr] [ptr @g_833], [1 x ptr] [ptr @g_220], [1 x ptr] [ptr @g_833], [1 x ptr] [ptr @g_220], [1 x ptr] [ptr @g_833]], align 16
@g_980 = internal global ptr @g_837, align 8
@__const.func_52.l_1010 = private unnamed_addr constant [10 x ptr] [ptr @g_833, ptr @g_833, ptr @g_833, ptr @g_833, ptr @g_833, ptr @g_833, ptr @g_833, ptr @g_833, ptr @g_833, ptr @g_833], align 16
@g_490 = internal global ptr @g_491, align 8
@g_491 = internal global ptr null, align 8
@g_674 = internal global [2 x ptr] zeroinitializer, align 16
@g_1025 = internal global ptr @g_1026, align 8
@g_1041 = internal global ptr null, align 8
@__const.func_52.l_1049 = private unnamed_addr constant [5 x [10 x i16]] [[10 x i16] [i16 32503, i16 1, i16 32503, i16 -3809, i16 32503, i16 1, i16 32503, i16 -3809, i16 32503, i16 1], [10 x i16] [i16 -14346, i16 -3809, i16 -9, i16 -3809, i16 -14346, i16 -3809, i16 -9, i16 -3809, i16 -14346, i16 -3809], [10 x i16] [i16 32503, i16 -3809, i16 32503, i16 1, i16 32503, i16 -3809, i16 32503, i16 1, i16 32503, i16 -3809], [10 x i16] [i16 -14346, i16 1, i16 -9, i16 1, i16 -14346, i16 1, i16 -9, i16 1, i16 -14346, i16 1], [10 x i16] [i16 32503, i16 1, i16 32503, i16 -3809, i16 32503, i16 1, i16 32503, i16 -3809, i16 32503, i16 1]], align 16
@g_1379 = internal global ptr null, align 8
@g_1158 = internal global [9 x ptr] [ptr @g_1159, ptr @g_1159, ptr @g_1159, ptr @g_1159, ptr @g_1159, ptr @g_1159, ptr @g_1159, ptr @g_1159, ptr @g_1159], align 16
@g_1159 = internal global ptr null, align 8
@g_803 = internal global ptr @g_804, align 8
@g_804 = internal constant i32 1019426258, align 4
@g_91 = internal global [1 x [5 x ptr]] [[5 x ptr] [ptr getelementptr (i8, ptr @g_17, i64 60), ptr getelementptr (i8, ptr @g_17, i64 60), ptr getelementptr (i8, ptr @g_17, i64 60), ptr getelementptr (i8, ptr @g_17, i64 60), ptr getelementptr (i8, ptr @g_17, i64 60)]], align 16
@__const.func_5.l_1764 = private unnamed_addr constant [6 x [10 x ptr]] [[10 x ptr] [ptr getelementptr (i8, ptr @g_17, i64 40), ptr null, ptr getelementptr (i8, ptr @g_17, i64 168), ptr null, ptr getelementptr (i8, ptr @g_17, i64 180), ptr null, ptr getelementptr (i8, ptr @g_17, i64 168), ptr null, ptr getelementptr (i8, ptr @g_17, i64 40), ptr getelementptr (i8, ptr @g_17, i64 120)], [10 x ptr] [ptr getelementptr (i8, ptr @g_17, i64 32), ptr null, ptr getelementptr (i8, ptr @g_17, i64 120), ptr getelementptr (i8, ptr @g_17, i64 128), ptr getelementptr (i8, ptr @g_17, i64 188), ptr getelementptr (i8, ptr @g_17, i64 120), ptr getelementptr (i8, ptr @g_17, i64 168), ptr getelementptr (i8, ptr @g_17, i64 120), ptr getelementptr (i8, ptr @g_17, i64 188), ptr getelementptr (i8, ptr @g_17, i64 128)], [10 x ptr] [ptr null, ptr null, ptr null, ptr getelementptr (i8, ptr @g_17, i64 128), ptr getelementptr (i8, ptr @g_17, i64 168), ptr getelementptr (i8, ptr @g_17, i64 120), ptr getelementptr (i8, ptr @g_17, i64 120), ptr getelementptr (i8, ptr @g_17, i64 120), ptr getelementptr (i8, ptr @g_17, i64 40), ptr getelementptr (i8, ptr @g_17, i64 120)], [10 x ptr] [ptr null, ptr getelementptr (i8, ptr @g_17, i64 120), ptr getelementptr (i8, ptr @g_17, i64 40), ptr null, ptr getelementptr (i8, ptr @g_17, i64 40), ptr getelementptr (i8, ptr @g_17, i64 120), ptr null, ptr getelementptr (i8, ptr @g_17, i64 120), ptr getelementptr (i8, ptr @g_17, i64 32), ptr null], [10 x ptr] [ptr getelementptr (i8, ptr @g_17, i64 32), ptr getelementptr (i8, ptr @g_17, i64 120), ptr null, ptr getelementptr (i8, ptr @g_17, i64 120), ptr getelementptr (i8, ptr @g_17, i64 40), ptr null, ptr getelementptr (i8, ptr @g_17, i64 40), ptr getelementptr (i8, ptr @g_17, i64 120), ptr null, ptr getelementptr (i8, ptr @g_17, i64 120)], [10 x ptr] [ptr getelementptr (i8, ptr @g_17, i64 40), ptr getelementptr (i8, ptr @g_17, i64 120), ptr getelementptr (i8, ptr @g_17, i64 120), ptr getelementptr (i8, ptr @g_17, i64 120), ptr getelementptr (i8, ptr @g_17, i64 168), ptr getelementptr (i8, ptr @g_17, i64 128), ptr null, ptr null, ptr null, ptr getelementptr (i8, ptr @g_17, i64 128)]], align 16
@__const.func_5.l_1785 = private unnamed_addr constant [8 x i32] [i32 -1, i32 -1, i32 -1, i32 -1, i32 -1, i32 -1, i32 -1, i32 -1], align 16
@__const.func_5.l_2069 = private unnamed_addr constant [5 x [10 x ptr]] [[10 x ptr] [ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr null, ptr null, ptr null, ptr @g_2066, ptr @g_2066, ptr @g_2066], [10 x ptr] [ptr null, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr null, ptr @g_2066, ptr @g_2066], [10 x ptr] [ptr null, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr null], [10 x ptr] [ptr @g_2066, ptr @g_2066, ptr null, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr null, ptr @g_2066, ptr @g_2066, ptr null], [10 x ptr] [ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr @g_2066, ptr null, ptr @g_2066, ptr @g_2066, ptr @g_2066]], align 16
@__const.func_5.l_1796 = private unnamed_addr constant [6 x i32] [i32 1, i32 1748159942, i32 1, i32 1, i32 1748159942, i32 1], align 16
@__const.func_5.l_1851 = private unnamed_addr constant [3 x [6 x [4 x i32]]] [[6 x [4 x i32]] [[4 x i32] [i32 289478719, i32 1, i32 -912406657, i32 0], [4 x i32] [i32 856269333, i32 -1, i32 2, i32 -1], [4 x i32] [i32 0, i32 1026743933, i32 289478719, i32 -1], [4 x i32] [i32 289478719, i32 -1, i32 2, i32 0], [4 x i32] [i32 1, i32 1, i32 2, i32 -6], [4 x i32] [i32 1, i32 1026743933, i32 2, i32 1]], [6 x [4 x i32]] [[4 x i32] [i32 289478719, i32 -6, i32 289478719, i32 0], [4 x i32] [i32 0, i32 -6, i32 2, i32 1], [4 x i32] [i32 856269333, i32 1026743933, i32 -912406657, i32 -6], [4 x i32] [i32 289478719, i32 1, i32 -912406657, i32 0], [4 x i32] [i32 856269333, i32 -1, i32 2, i32 -1], [4 x i32] [i32 0, i32 1026743933, i32 289478719, i32 -1]], [6 x [4 x i32]] [[4 x i32] [i32 289478719, i32 -1, i32 2, i32 0], [4 x i32] [i32 1, i32 1, i32 2, i32 -6], [4 x i32] [i32 1, i32 1026743933, i32 2, i32 1], [4 x i32] [i32 289478719, i32 -6, i32 289478719, i32 0], [4 x i32] [i32 0, i32 -6, i32 2, i32 1], [4 x i32] [i32 856269333, i32 1026743933, i32 -912406657, i32 -6]]], align 16
@g_1117 = internal global ptr @g_1118, align 8
@g_1299 = internal global ptr getelementptr (i8, ptr @g_1300, i64 8), align 8
@g_1074 = internal global [8 x ptr] [ptr @g_490, ptr @g_490, ptr @g_490, ptr @g_490, ptr @g_490, ptr @g_490, ptr @g_490, ptr @g_490], align 16
@__const.func_5.l_1929 = private unnamed_addr constant [5 x [3 x ptr]] [[3 x ptr] [ptr getelementptr (i8, ptr @g_1074, i64 48), ptr getelementptr (i8, ptr @g_1074, i64 40), ptr getelementptr (i8, ptr @g_1074, i64 48)], [3 x ptr] [ptr getelementptr (i8, ptr @g_1074, i64 48), ptr getelementptr (i8, ptr @g_1074, i64 48), ptr getelementptr (i8, ptr @g_1074, i64 48)], [3 x ptr] [ptr getelementptr (i8, ptr @g_1074, i64 48), ptr getelementptr (i8, ptr @g_1074, i64 40), ptr getelementptr (i8, ptr @g_1074, i64 48)], [3 x ptr] [ptr getelementptr (i8, ptr @g_1074, i64 48), ptr getelementptr (i8, ptr @g_1074, i64 48), ptr getelementptr (i8, ptr @g_1074, i64 48)], [3 x ptr] [ptr getelementptr (i8, ptr @g_1074, i64 48), ptr getelementptr (i8, ptr @g_1074, i64 40), ptr getelementptr (i8, ptr @g_1074, i64 48)]], align 16
@__const.func_5.l_1930 = private unnamed_addr constant [9 x [4 x [7 x ptr]]] [[4 x [7 x ptr]] [[7 x ptr] [ptr null, ptr @g_839, ptr getelementptr (i8, ptr @g_1797, i64 26), ptr null, ptr null, ptr @g_163, ptr @g_839], [7 x ptr] [ptr null, ptr @g_248, ptr null, ptr null, ptr @g_1152, ptr getelementptr (i8, ptr @g_1438, i64 91), ptr @g_839], [7 x ptr] [ptr @g_365, ptr getelementptr (i8, ptr @g_1797, i64 26), ptr getelementptr (i8, ptr @g_365, i64 13), ptr @g_1484, ptr @g_1780, ptr @g_1780, ptr null], [7 x ptr] [ptr @g_1152, ptr null, ptr null, ptr getelementptr (i8, ptr @g_1797, i64 26), ptr @g_248, ptr @g_839, ptr @g_839]], [4 x [7 x ptr]] [[7 x ptr] [ptr @g_1152, ptr null, ptr null, ptr null, ptr @g_1152, ptr @g_1152, ptr @g_163], [7 x ptr] [ptr null, ptr @g_310, ptr @g_248, ptr getelementptr (i8, ptr @g_1797, i64 26), ptr null, ptr @g_1412, ptr null], [7 x ptr] [ptr null, ptr @g_248, ptr @g_1780, ptr null, ptr @g_1152, ptr @g_248, ptr @g_1484], [7 x ptr] [ptr null, ptr getelementptr (i8, ptr @g_1797, i64 26), ptr null, ptr null, ptr null, ptr null, ptr @g_1780]], [4 x [7 x ptr]] [[7 x ptr] [ptr @g_1152, ptr null, ptr @g_1152, ptr null, ptr @g_248, ptr null, ptr @g_936], [7 x ptr] [ptr @g_1152, ptr @g_1412, ptr @g_248, ptr null, ptr null, ptr null, ptr @g_936], [7 x ptr] [ptr @g_163, ptr null, ptr @g_1484, ptr @g_1780, ptr @g_936, ptr @g_936, ptr @g_1780], [7 x ptr] [ptr @g_842, ptr @g_1780, ptr @g_842, ptr @g_248, ptr null, ptr getelementptr (i8, ptr @g_1438, i64 91), ptr @g_1484]], [4 x [7 x ptr]] [[7 x ptr] [ptr @g_1412, ptr @g_839, ptr @g_1412, ptr @g_163, ptr @g_842, ptr null, ptr null], [7 x ptr] [ptr @g_1484, ptr @g_163, ptr @g_842, ptr null, ptr getelementptr (i8, ptr @g_1797, i64 26), ptr getelementptr (i8, ptr @g_1438, i64 91), ptr @g_163], [7 x ptr] [ptr null, ptr @g_365, ptr null, ptr @g_1412, ptr @g_163, ptr @g_936, ptr @g_839], [7 x ptr] [ptr @g_365, ptr null, ptr @g_936, ptr null, ptr @g_163, ptr null, ptr null]], [4 x [7 x ptr]] [[7 x ptr] [ptr @g_590, ptr getelementptr (i8, ptr @g_1438, i64 91), ptr null, ptr getelementptr (i8, ptr @g_1797, i64 26), ptr @g_163, ptr null, ptr @g_310], [7 x ptr] [ptr null, ptr null, ptr null, ptr @g_163, ptr @g_163, ptr null, ptr null], [7 x ptr] [ptr @g_1412, ptr @g_1152, ptr @g_1780, ptr null, ptr getelementptr (i8, ptr @g_1797, i64 26), ptr @g_248, ptr null], [7 x ptr] [ptr @g_248, ptr @g_1412, ptr null, ptr @g_248, ptr @g_842, ptr @g_1412, ptr null]], [4 x [7 x ptr]] [[7 x ptr] [ptr null, ptr null, ptr getelementptr (i8, ptr @g_1438, i64 91), ptr null, ptr null, ptr @g_1152, ptr @g_365], [7 x ptr] [ptr @g_365, ptr null, ptr @g_1412, ptr @g_163, ptr @g_936, ptr @g_839, ptr @g_248], [7 x ptr] [ptr null, ptr @g_1412, ptr @g_365, ptr getelementptr (i8, ptr @g_1797, i64 26), ptr null, ptr @g_1780, ptr @g_365], [7 x ptr] [ptr @g_1780, ptr @g_936, ptr @g_365, ptr null, ptr @g_248, ptr getelementptr (i8, ptr @g_1797, i64 26), ptr @g_1484]], [4 x [7 x ptr]] [[7 x ptr] [ptr @g_839, ptr null, ptr @g_1412, ptr @g_1412, ptr null, ptr @g_839, ptr null], [7 x ptr] [ptr getelementptr (i8, ptr @g_1797, i64 26), ptr @g_1780, ptr getelementptr (i8, ptr @g_1438, i64 91), ptr null, ptr @g_1152, ptr getelementptr (i8, ptr @g_1797, i64 26), ptr null], [7 x ptr] [ptr @g_1780, ptr @g_365, ptr null, ptr @g_163, ptr null, ptr null, ptr getelementptr (i8, ptr @g_1797, i64 26)], [7 x ptr] [ptr @g_1484, ptr @g_1780, ptr @g_1780, ptr @g_248, ptr @g_1152, ptr null, ptr @g_1152]], [4 x [7 x ptr]] [[7 x ptr] [ptr null, ptr null, ptr null, ptr @g_1780, ptr @g_248, ptr @g_163, ptr null], [7 x ptr] [ptr null, ptr @g_936, ptr null, ptr @g_936, ptr null, ptr @g_842, ptr getelementptr (i8, ptr @g_1797, i64 26)], [7 x ptr] [ptr null, ptr null, ptr @g_248, ptr null, ptr null, ptr @g_163, ptr null], [7 x ptr] [ptr getelementptr (i8, ptr @g_1438, i64 91), ptr null, ptr null, ptr getelementptr (i8, ptr @g_1438, i64 91), ptr getelementptr (i8, ptr @g_1797, i64 26), ptr @g_1412, ptr @g_248]], [4 x [7 x ptr]] [[7 x ptr] [ptr getelementptr (i8, ptr @g_1797, i64 26), ptr @g_1484, ptr @g_1152, ptr @g_1484, ptr null, ptr @g_839, ptr null], [7 x ptr] [ptr @g_1412, ptr @g_163, ptr null, ptr @g_1780, ptr @g_163, ptr @g_1412, ptr @g_248], [7 x ptr] [ptr @g_248, ptr @g_842, ptr @g_1152, ptr null, ptr @g_310, ptr getelementptr (i8, ptr @g_1797, i64 26), ptr null], [7 x ptr] [ptr null, ptr null, ptr @g_248, ptr @g_248, ptr @g_936, ptr @g_842, ptr getelementptr (i8, ptr @g_1797, i64 26)]]], align 16
@__const.func_5.l_1866 = private unnamed_addr constant [4 x ptr] [ptr @g_294, ptr @g_294, ptr @g_294, ptr @g_294], align 16
@__const.func_5.l_1880 = private unnamed_addr constant [7 x i64] [i64 -4709101565454659157, i64 -4709101565454659157, i64 -4709101565454659157, i64 -4709101565454659157, i64 -4709101565454659157, i64 -4709101565454659157, i64 -4709101565454659157], align 16
@g_840 = internal global [10 x ptr] [ptr @g_365, ptr @g_365, ptr @g_590, ptr @g_310, ptr @g_590, ptr @g_365, ptr @g_365, ptr @g_590, ptr @g_310, ptr @g_590], align 16
@g_1946 = internal global [9 x ptr] [ptr @g_1947, ptr @g_1947, ptr @g_1947, ptr @g_1947, ptr @g_1947, ptr @g_1947, ptr @g_1947, ptr @g_1947, ptr @g_1947], align 16
@g_1947 = internal global [1 x ptr] [ptr @g_1786], align 8
@__const.func_5.l_1964 = private unnamed_addr constant [4 x ptr] [ptr @g_544, ptr @g_544, ptr @g_544, ptr @g_544], align 16
@__const.func_5.l_2006 = private unnamed_addr constant [8 x [1 x [7 x i32]]] [[1 x [7 x i32]] [[7 x i32] [i32 270250128, i32 -1673096068, i32 0, i32 -917919395, i32 -1, i32 -1, i32 -917919395]], [1 x [7 x i32]] [[7 x i32] [i32 -459158364, i32 -1153952745, i32 -459158364, i32 1, i32 -917919395, i32 270250128, i32 1475277351]], [1 x [7 x i32]] [[7 x i32] [i32 0, i32 -1673096068, i32 270250128, i32 1, i32 270250128, i32 -1673096068, i32 0]], [1 x [7 x i32]] [[7 x i32] [i32 -1673096068, i32 -829190589, i32 1475277351, i32 -917919395, i32 -825350700, i32 270250128, i32 -825350700]], [1 x [7 x i32]] [[7 x i32] [i32 -1673096068, i32 -825350700, i32 -825350700, i32 -1673096068, i32 -459158364, i32 -1, i32 1]], [1 x [7 x i32]] [[7 x i32] [i32 0, i32 -1, i32 1475277351, i32 -459158364, i32 -459158364, i32 1475277351, i32 -1]], [1 x [7 x i32]] [[7 x i32] [i32 -459158364, i32 0, i32 270250128, i32 -829190589, i32 -825350700, i32 1, i32 1]], [1 x [7 x i32]] [[7 x i32] [i32 270250128, i32 0, i32 -459158364, i32 0, i32 270250128, i32 -829190589, i32 -825350700]]], align 16
@__const.func_5.l_2197 = private unnamed_addr constant [6 x [1 x [3 x i32]]] [[1 x [3 x i32]] [[3 x i32] [i32 -10, i32 -1769220462, i32 -10]], [1 x [3 x i32]] [[3 x i32] [i32 1, i32 1, i32 1]], [1 x [3 x i32]] [[3 x i32] [i32 -10, i32 -1769220462, i32 -10]], [1 x [3 x i32]] [[3 x i32] [i32 1, i32 1, i32 1]], [1 x [3 x i32]] [[3 x i32] [i32 -10, i32 -1769220462, i32 -10]], [1 x [3 x i32]] [[3 x i32] [i32 1, i32 1, i32 1]]], align 16
@g_1118 = internal global ptr @g_802, align 8
@g_1300 = internal global [10 x ptr] [ptr @g_1301, ptr @g_1301, ptr @g_1301, ptr @g_1301, ptr @g_1301, ptr @g_1301, ptr @g_1301, ptr @g_1301, ptr @g_1301, ptr @g_1301], align 16
@g_1301 = internal global ptr getelementptr (i8, ptr @g_1302, i64 208), align 8
@g_1302 = internal global [9 x [7 x ptr]] [[7 x ptr] [ptr getelementptr (i8, ptr @g_1311, i64 16), ptr null, ptr null, ptr null, ptr null, ptr getelementptr (i8, ptr @g_1311, i64 16), ptr @g_1306], [7 x ptr] [ptr null, ptr @g_1307, ptr @g_1305, ptr getelementptr (i8, ptr @g_1312, i64 48), ptr getelementptr (i8, ptr @g_1312, i64 48), ptr @g_1305, ptr @g_1307], [7 x ptr] [ptr null, ptr @g_1313, ptr null, ptr null, ptr @g_1308, ptr @g_1308, ptr null], [7 x ptr] [ptr @g_1310, ptr @g_1314, ptr @g_1310, ptr @g_1315, ptr @g_1314, ptr @g_1305, ptr @g_1305], [7 x ptr] [ptr @g_1306, ptr null, ptr @g_1304, ptr null, ptr @g_1306, ptr @g_1304, ptr getelementptr (i8, ptr @g_1311, i64 16)], [7 x ptr] [ptr @g_1303, ptr @g_1305, ptr @g_1315, ptr @g_1303, ptr @g_1315, ptr @g_1305, ptr @g_1303], [7 x ptr] [ptr null, ptr getelementptr (i8, ptr @g_1311, i64 16), ptr @g_1313, ptr @g_1308, ptr getelementptr (i8, ptr @g_1311, i64 16), ptr @g_1308, ptr @g_1313], [7 x ptr] [ptr @g_1303, ptr @g_1303, ptr getelementptr (i8, ptr @g_1312, i64 48), ptr @g_1314, ptr @g_1309, ptr getelementptr (i8, ptr @g_1312, i64 48), ptr @g_1309], [7 x ptr] [ptr @g_1306, ptr @g_1313, ptr @g_1313, ptr @g_1306, ptr @g_1308, ptr null, ptr @g_1306]], align 16
@g_1786 = internal global ptr @g_1787, align 8
@g_1014 = internal constant ptr @g_1015, align 8
@.str.206 = private unnamed_addr constant [36 x i8] c"...checksum after hashing %s : %lX\0A\00", align 1
@g_163 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 47, i8 2, i8 -48, i8 -86, i8 -1, i8 95, i8 117, i8 0, i8 -128, i8 -112, i8 2, i8 -1, i8 31 }, align 1
@g_248 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -4, i8 3, i8 -128, i8 126, i8 0, i8 0, i8 18, i8 0, i8 96, i8 -88, i8 91, i8 -1, i8 31 }, align 1
@g_310 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 17, i8 -1, i8 111, i8 119, i8 -1, i8 127, i8 94, i8 0, i8 64, i8 -56, i8 -124, i8 0, i8 0 }, align 1
@g_365 = internal global [4 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }] [{ i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -128, i8 2, i8 112, i8 91, i8 1, i8 64, i8 -123, i8 0, i8 96, i8 -96, i8 91, i8 0, i8 0 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -128, i8 2, i8 112, i8 91, i8 1, i8 64, i8 -123, i8 0, i8 96, i8 -96, i8 91, i8 0, i8 0 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -128, i8 2, i8 112, i8 91, i8 1, i8 64, i8 -123, i8 0, i8 96, i8 -96, i8 91, i8 0, i8 0 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -128, i8 2, i8 112, i8 91, i8 1, i8 64, i8 -123, i8 0, i8 96, i8 -96, i8 91, i8 0, i8 0 }], align 16
@g_473 = internal global [2 x { i32, [4 x i8] }] [{ i32, [4 x i8] } { i32 1720744803, [4 x i8] zeroinitializer }, { i32, [4 x i8] } { i32 1720744803, [4 x i8] zeroinitializer }], align 16
@g_516 = internal global { i32, [4 x i8] } { i32 444536128, [4 x i8] zeroinitializer }, align 8
@g_542 = internal constant { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -17, i8 -4, i8 -65, i8 -9, i8 -1, i8 -33, i8 44, i8 1, i8 32, i8 24, i8 120, i8 -1, i8 31 }, align 1
@g_567 = internal global { i32, [4 x i8] } { i32 -1, [4 x i8] zeroinitializer }, align 8
@g_590 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 84, i8 -4, i8 -1, i8 -29, i8 0, i8 0, i8 80, i8 0, i8 -128, i8 88, i8 93, i8 -1, i8 31 }, align 1
@g_671 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 80, i8 -1, i8 95, i8 127, i8 0, i8 -32, i8 -86, i8 0, i8 16, i8 -48, i8 89, i8 0, i8 0 }, align 1
@g_839 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 39, i8 0, i8 -48, i8 -115, i8 0, i8 -128, i8 -16, i8 0, i8 0, i8 -120, i8 65, i8 0, i8 0 }, align 1
@g_842 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -109, i8 0, i8 -48, i8 34, i8 -1, i8 -97, i8 -1, i8 0, i8 -128, i8 -80, i8 0, i8 -1, i8 31 }, align 1
@g_923 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -7, i8 -4, i8 31, i8 -53, i8 -2, i8 -33, i8 -50, i8 0, i8 0, i8 8, i8 -63, i8 0, i8 0 }, align 1
@g_924 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 113, i8 -4, i8 -33, i8 -118, i8 0, i8 -128, i8 103, i8 0, i8 -112, i8 -120, i8 17, i8 -1, i8 31 }, align 1
@g_936 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 18, i8 -3, i8 -81, i8 83, i8 -1, i8 -33, i8 113, i8 0, i8 16, i8 -48, i8 47, i8 0, i8 0 }, align 1
@g_1038 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -64, i8 -1, i8 -65, i8 -102, i8 0, i8 32, i8 -43, i8 0, i8 32, i8 -40, i8 8, i8 -1, i8 31 }, align 1
@g_1152 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -4, i8 -2, i8 -17, i8 -46, i8 -1, i8 -97, i8 52, i8 0, i8 112, i8 48, i8 -124, i8 -1, i8 31 }, align 1
@g_1191 = internal global { i32, [4 x i8] } { i32 -6, [4 x i8] zeroinitializer }, align 8
@g_1263 = internal constant [1 x [5 x { i32, [4 x i8] }]] [[5 x { i32, [4 x i8] }] [{ i32, [4 x i8] } { i32 3, [4 x i8] zeroinitializer }, { i32, [4 x i8] } { i32 3, [4 x i8] zeroinitializer }, { i32, [4 x i8] } { i32 3, [4 x i8] zeroinitializer }, { i32, [4 x i8] } { i32 3, [4 x i8] zeroinitializer }, { i32, [4 x i8] } { i32 3, [4 x i8] zeroinitializer }]], align 16
@g_1412 = internal global [1 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }] [{ i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 15, i8 2, i8 96, i8 105, i8 1, i8 -32, i8 -33, i8 0, i8 -128, i8 104, i8 -61, i8 0, i8 0 }], align 1
@g_1438 = internal global [10 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }] [{ i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 26, i8 -1, i8 -33, i8 -115, i8 0, i8 -128, i8 101, i8 1, i8 16, i8 -8, i8 117, i8 -1, i8 31 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 26, i8 -1, i8 -33, i8 -115, i8 0, i8 -128, i8 101, i8 1, i8 16, i8 -8, i8 117, i8 -1, i8 31 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 26, i8 -1, i8 -33, i8 -115, i8 0, i8 -128, i8 101, i8 1, i8 16, i8 -8, i8 117, i8 -1, i8 31 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 26, i8 -1, i8 -33, i8 -115, i8 0, i8 -128, i8 101, i8 1, i8 16, i8 -8, i8 117, i8 -1, i8 31 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 26, i8 -1, i8 -33, i8 -115, i8 0, i8 -128, i8 101, i8 1, i8 16, i8 -8, i8 117, i8 -1, i8 31 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 26, i8 -1, i8 -33, i8 -115, i8 0, i8 -128, i8 101, i8 1, i8 16, i8 -8, i8 117, i8 -1, i8 31 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 26, i8 -1, i8 -33, i8 -115, i8 0, i8 -128, i8 101, i8 1, i8 16, i8 -8, i8 117, i8 -1, i8 31 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 26, i8 -1, i8 -33, i8 -115, i8 0, i8 -128, i8 101, i8 1, i8 16, i8 -8, i8 117, i8 -1, i8 31 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 26, i8 -1, i8 -33, i8 -115, i8 0, i8 -128, i8 101, i8 1, i8 16, i8 -8, i8 117, i8 -1, i8 31 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 26, i8 -1, i8 -33, i8 -115, i8 0, i8 -128, i8 101, i8 1, i8 16, i8 -8, i8 117, i8 -1, i8 31 }], align 16
@g_1484 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 103, i8 -3, i8 95, i8 -70, i8 -1, i8 -1, i8 -119, i8 0, i8 16, i8 0, i8 60, i8 -1, i8 31 }, align 1
@g_1608 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -16, i8 -3, i8 -33, i8 -64, i8 -2, i8 -1, i8 -59, i8 0, i8 64, i8 -8, i8 95, i8 0, i8 0 }, align 1
@g_1780 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -110, i8 2, i8 -96, i8 102, i8 -1, i8 31, i8 -127, i8 0, i8 64, i8 88, i8 -55, i8 0, i8 0 }, align 1
@g_1797 = internal global [1 x [4 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }]] [[4 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }] [{ i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -127, i8 3, i8 -16, i8 117, i8 -1, i8 -65, i8 -61, i8 0, i8 0, i8 -96, i8 110, i8 0, i8 0 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -127, i8 3, i8 -16, i8 117, i8 -1, i8 -65, i8 -61, i8 0, i8 0, i8 -96, i8 110, i8 0, i8 0 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -127, i8 3, i8 -16, i8 117, i8 -1, i8 -65, i8 -61, i8 0, i8 0, i8 -96, i8 110, i8 0, i8 0 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -127, i8 3, i8 -16, i8 117, i8 -1, i8 -65, i8 -61, i8 0, i8 0, i8 -96, i8 110, i8 0, i8 0 }]], align 16
@g_1999 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 125, i8 -1, i8 -113, i8 -114, i8 -1, i8 127, i8 68, i8 0, i8 80, i8 -104, i8 -28, i8 -1, i8 31 }, align 1
@g_2067 = internal global { i32, [4 x i8] } { i32 1, [4 x i8] zeroinitializer }, align 8
@g_2093 = internal constant [7 x [1 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }]] [[1 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }] [{ i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 69, i8 2, i8 -48, i8 -77, i8 0, i8 0, i8 -78, i8 0, i8 -96, i8 -8, i8 -112, i8 0, i8 0 }], [1 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }] [{ i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -84, i8 -4, i8 15, i8 -83, i8 -1, i8 127, i8 10, i8 1, i8 64, i8 -120, i8 121, i8 -1, i8 31 }], [1 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }] [{ i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 69, i8 2, i8 -48, i8 -77, i8 0, i8 0, i8 -78, i8 0, i8 -96, i8 -8, i8 -112, i8 0, i8 0 }], [1 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }] [{ i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -84, i8 -4, i8 15, i8 -83, i8 -1, i8 127, i8 10, i8 1, i8 64, i8 -120, i8 121, i8 -1, i8 31 }], [1 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }] [{ i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 69, i8 2, i8 -48, i8 -77, i8 0, i8 0, i8 -78, i8 0, i8 -96, i8 -8, i8 -112, i8 0, i8 0 }], [1 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }] [{ i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -84, i8 -4, i8 15, i8 -83, i8 -1, i8 127, i8 10, i8 1, i8 64, i8 -120, i8 121, i8 -1, i8 31 }], [1 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }] [{ i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 69, i8 2, i8 -48, i8 -77, i8 0, i8 0, i8 -78, i8 0, i8 -96, i8 -8, i8 -112, i8 0, i8 0 }]], align 16
@g_2094 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -99, i8 -2, i8 15, i8 -32, i8 -1, i8 -1, i8 16, i8 0, i8 -96, i8 -32, i8 50, i8 -1, i8 31 }, align 1
@g_2109 = internal global [2 x { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 }] [{ i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 123, i8 -4, i8 63, i8 65, i8 -1, i8 -65, i8 25, i8 1, i8 -112, i8 56, i8 -124, i8 -1, i8 31 }, { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 123, i8 -4, i8 63, i8 65, i8 -1, i8 -65, i8 25, i8 1, i8 -112, i8 56, i8 -124, i8 -1, i8 31 }], align 16
@g_2157 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -51, i8 -3, i8 -1, i8 -12, i8 0, i8 -64, i8 83, i8 1, i8 -96, i8 72, i8 95, i8 -1, i8 31 }, align 1
@g_2273 = internal global { i32, [4 x i8] } { i32 -937680291, [4 x i8] zeroinitializer }, align 8
@g_2297 = internal global { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8 } { i8 -57, i8 2, i8 112, i8 -50, i8 -2, i8 95, i8 80, i8 0, i8 32, i8 -48, i8 110, i8 0, i8 0 }, align 1
@.str.240 = private unnamed_addr constant [15 x i8] c"checksum = %X\0A\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca ptr, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  %9 = alloca i32, align 4
  %10 = alloca %struct.S0, align 1
  %11 = alloca { i64, i40 }, align 8
  store i32 0, ptr %3, align 4
  store i32 %0, ptr %4, align 4
  store ptr %1, ptr %5, align 8
  store i32 0, ptr %9, align 4
  %12 = load i32, ptr %4, align 4
  %13 = icmp eq i32 %12, 2
  br i1 %13, label %14, label %21

14:                                               ; preds = %2
  %15 = load ptr, ptr %5, align 8
  %16 = getelementptr inbounds ptr, ptr %15, i64 1
  %17 = load ptr, ptr %16, align 8
  %18 = call i32 @strcmp(ptr noundef %17, ptr noundef @.str) #5
  %19 = icmp eq i32 %18, 0
  br i1 %19, label %20, label %21

20:                                               ; preds = %14
  store i32 1, ptr %9, align 4
  br label %21

21:                                               ; preds = %20, %14, %2
  call void @platform_main_begin()
  call void @crc32_gentab()
  %22 = call { i64, i40 } @func_1()
  %23 = getelementptr inbounds nuw %struct.S0, ptr %10, i32 0, i32 0
  store { i64, i40 } %22, ptr %11, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %23, ptr align 8 %11, i64 13, i1 false)
  store i32 0, ptr %6, align 4
  br label %24

24:                                               ; preds = %39, %21
  %25 = load i32, ptr %6, align 4
  %26 = icmp slt i32 %25, 8
  br i1 %26, label %27, label %42

27:                                               ; preds = %24
  %28 = load i32, ptr %6, align 4
  %29 = sext i32 %28 to i64
  %30 = getelementptr inbounds [8 x i64], ptr @g_14, i64 0, i64 %29
  %31 = load i64, ptr %30, align 8
  %32 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %31, ptr noundef @.str.1, i32 noundef %32)
  %33 = load i32, ptr %9, align 4
  %34 = icmp ne i32 %33, 0
  br i1 %34, label %35, label %38

35:                                               ; preds = %27
  %36 = load i32, ptr %6, align 4
  %37 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %36)
  br label %38

38:                                               ; preds = %35, %27
  br label %39

39:                                               ; preds = %38
  %40 = load i32, ptr %6, align 4
  %41 = add nsw i32 %40, 1
  store i32 %41, ptr %6, align 4
  br label %24, !llvm.loop !6

42:                                               ; preds = %24
  store i32 0, ptr %6, align 4
  br label %43

43:                                               ; preds = %83, %42
  %44 = load i32, ptr %6, align 4
  %45 = icmp slt i32 %44, 7
  br i1 %45, label %46, label %86

46:                                               ; preds = %43
  store i32 0, ptr %7, align 4
  br label %47

47:                                               ; preds = %79, %46
  %48 = load i32, ptr %7, align 4
  %49 = icmp slt i32 %48, 1
  br i1 %49, label %50, label %82

50:                                               ; preds = %47
  store i32 0, ptr %8, align 4
  br label %51

51:                                               ; preds = %75, %50
  %52 = load i32, ptr %8, align 4
  %53 = icmp slt i32 %52, 8
  br i1 %53, label %54, label %78

54:                                               ; preds = %51
  %55 = load i32, ptr %6, align 4
  %56 = sext i32 %55 to i64
  %57 = getelementptr inbounds [7 x [1 x [8 x i32]]], ptr @g_17, i64 0, i64 %56
  %58 = load i32, ptr %7, align 4
  %59 = sext i32 %58 to i64
  %60 = getelementptr inbounds [1 x [8 x i32]], ptr %57, i64 0, i64 %59
  %61 = load i32, ptr %8, align 4
  %62 = sext i32 %61 to i64
  %63 = getelementptr inbounds [8 x i32], ptr %60, i64 0, i64 %62
  %64 = load i32, ptr %63, align 4
  %65 = sext i32 %64 to i64
  %66 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %65, ptr noundef @.str.3, i32 noundef %66)
  %67 = load i32, ptr %9, align 4
  %68 = icmp ne i32 %67, 0
  br i1 %68, label %69, label %74

69:                                               ; preds = %54
  %70 = load i32, ptr %6, align 4
  %71 = load i32, ptr %7, align 4
  %72 = load i32, ptr %8, align 4
  %73 = call i32 (ptr, ...) @printf(ptr noundef @.str.4, i32 noundef %70, i32 noundef %71, i32 noundef %72)
  br label %74

74:                                               ; preds = %69, %54
  br label %75

75:                                               ; preds = %74
  %76 = load i32, ptr %8, align 4
  %77 = add nsw i32 %76, 1
  store i32 %77, ptr %8, align 4
  br label %51, !llvm.loop !8

78:                                               ; preds = %51
  br label %79

79:                                               ; preds = %78
  %80 = load i32, ptr %7, align 4
  %81 = add nsw i32 %80, 1
  store i32 %81, ptr %7, align 4
  br label %47, !llvm.loop !9

82:                                               ; preds = %47
  br label %83

83:                                               ; preds = %82
  %84 = load i32, ptr %6, align 4
  %85 = add nsw i32 %84, 1
  store i32 %85, ptr %6, align 4
  br label %43, !llvm.loop !10

86:                                               ; preds = %43
  %87 = load i16, ptr @g_45, align 2
  %88 = sext i16 %87 to i64
  %89 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %88, ptr noundef @.str.5, i32 noundef %89)
  %90 = load i32, ptr @g_71, align 4
  %91 = sext i32 %90 to i64
  %92 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %91, ptr noundef @.str.6, i32 noundef %92)
  %93 = load volatile i32, ptr @g_72, align 4
  %94 = zext i32 %93 to i64
  %95 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %94, ptr noundef @.str.7, i32 noundef %95)
  %96 = load i16, ptr @g_81, align 2
  %97 = sext i16 %96 to i64
  %98 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %97, ptr noundef @.str.8, i32 noundef %98)
  %99 = load i8, ptr @g_88, align 1
  %100 = zext i8 %99 to i64
  %101 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %100, ptr noundef @.str.9, i32 noundef %101)
  %102 = load i8, ptr @g_118, align 1
  %103 = sext i8 %102 to i64
  %104 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %103, ptr noundef @.str.10, i32 noundef %104)
  %105 = load i32, ptr @g_123, align 4
  %106 = sext i32 %105 to i64
  %107 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %106, ptr noundef @.str.11, i32 noundef %107)
  store i32 0, ptr %6, align 4
  br label %108

108:                                              ; preds = %124, %86
  %109 = load i32, ptr %6, align 4
  %110 = icmp slt i32 %109, 3
  br i1 %110, label %111, label %127

111:                                              ; preds = %108
  %112 = load i32, ptr %6, align 4
  %113 = sext i32 %112 to i64
  %114 = getelementptr inbounds [3 x i32], ptr @g_147, i64 0, i64 %113
  %115 = load i32, ptr %114, align 4
  %116 = sext i32 %115 to i64
  %117 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %116, ptr noundef @.str.12, i32 noundef %117)
  %118 = load i32, ptr %9, align 4
  %119 = icmp ne i32 %118, 0
  br i1 %119, label %120, label %123

120:                                              ; preds = %111
  %121 = load i32, ptr %6, align 4
  %122 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %121)
  br label %123

123:                                              ; preds = %120, %111
  br label %124

124:                                              ; preds = %123
  %125 = load i32, ptr %6, align 4
  %126 = add nsw i32 %125, 1
  store i32 %126, ptr %6, align 4
  br label %108, !llvm.loop !11

127:                                              ; preds = %108
  store i32 0, ptr %6, align 4
  br label %128

128:                                              ; preds = %167, %127
  %129 = load i32, ptr %6, align 4
  %130 = icmp slt i32 %129, 9
  br i1 %130, label %131, label %170

131:                                              ; preds = %128
  store i32 0, ptr %7, align 4
  br label %132

132:                                              ; preds = %163, %131
  %133 = load i32, ptr %7, align 4
  %134 = icmp slt i32 %133, 4
  br i1 %134, label %135, label %166

135:                                              ; preds = %132
  store i32 0, ptr %8, align 4
  br label %136

136:                                              ; preds = %159, %135
  %137 = load i32, ptr %8, align 4
  %138 = icmp slt i32 %137, 2
  br i1 %138, label %139, label %162

139:                                              ; preds = %136
  %140 = load i32, ptr %6, align 4
  %141 = sext i32 %140 to i64
  %142 = getelementptr inbounds [9 x [4 x [2 x i64]]], ptr @g_160, i64 0, i64 %141
  %143 = load i32, ptr %7, align 4
  %144 = sext i32 %143 to i64
  %145 = getelementptr inbounds [4 x [2 x i64]], ptr %142, i64 0, i64 %144
  %146 = load i32, ptr %8, align 4
  %147 = sext i32 %146 to i64
  %148 = getelementptr inbounds [2 x i64], ptr %145, i64 0, i64 %147
  %149 = load i64, ptr %148, align 8
  %150 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %149, ptr noundef @.str.13, i32 noundef %150)
  %151 = load i32, ptr %9, align 4
  %152 = icmp ne i32 %151, 0
  br i1 %152, label %153, label %158

153:                                              ; preds = %139
  %154 = load i32, ptr %6, align 4
  %155 = load i32, ptr %7, align 4
  %156 = load i32, ptr %8, align 4
  %157 = call i32 (ptr, ...) @printf(ptr noundef @.str.4, i32 noundef %154, i32 noundef %155, i32 noundef %156)
  br label %158

158:                                              ; preds = %153, %139
  br label %159

159:                                              ; preds = %158
  %160 = load i32, ptr %8, align 4
  %161 = add nsw i32 %160, 1
  store i32 %161, ptr %8, align 4
  br label %136, !llvm.loop !12

162:                                              ; preds = %136
  br label %163

163:                                              ; preds = %162
  %164 = load i32, ptr %7, align 4
  %165 = add nsw i32 %164, 1
  store i32 %165, ptr %7, align 4
  br label %132, !llvm.loop !13

166:                                              ; preds = %132
  br label %167

167:                                              ; preds = %166
  %168 = load i32, ptr %6, align 4
  %169 = add nsw i32 %168, 1
  store i32 %169, ptr %6, align 4
  br label %128, !llvm.loop !14

170:                                              ; preds = %128
  %171 = load volatile i104, ptr @g_163, align 1
  %172 = shl i104 %171, 84
  %173 = ashr i104 %172, 84
  %174 = trunc i104 %173 to i32
  %175 = sext i32 %174 to i64
  %176 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %175, ptr noundef @.str.14, i32 noundef %176)
  %177 = load i104, ptr @g_163, align 1
  %178 = shl i104 %177, 59
  %179 = ashr i104 %178, 79
  %180 = trunc i104 %179 to i32
  %181 = sext i32 %180 to i64
  %182 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %181, ptr noundef @.str.15, i32 noundef %182)
  %183 = load i104, ptr @g_163, align 1
  %184 = lshr i104 %183, 45
  %185 = and i104 %184, 8388607
  %186 = trunc i104 %185 to i32
  %187 = zext i32 %186 to i64
  %188 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %187, ptr noundef @.str.16, i32 noundef %188)
  %189 = load volatile i104, ptr @g_163, align 1
  %190 = lshr i104 %189, 68
  %191 = and i104 %190, 127
  %192 = trunc i104 %191 to i32
  %193 = zext i32 %192 to i64
  %194 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %193, ptr noundef @.str.17, i32 noundef %194)
  %195 = load i104, ptr @g_163, align 1
  %196 = shl i104 %195, 3
  %197 = ashr i104 %196, 78
  %198 = trunc i104 %197 to i32
  %199 = sext i32 %198 to i64
  %200 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %199, ptr noundef @.str.18, i32 noundef %200)
  store i32 0, ptr %6, align 4
  br label %201

201:                                              ; preds = %241, %170
  %202 = load i32, ptr %6, align 4
  %203 = icmp slt i32 %202, 1
  br i1 %203, label %204, label %244

204:                                              ; preds = %201
  store i32 0, ptr %7, align 4
  br label %205

205:                                              ; preds = %237, %204
  %206 = load i32, ptr %7, align 4
  %207 = icmp slt i32 %206, 2
  br i1 %207, label %208, label %240

208:                                              ; preds = %205
  store i32 0, ptr %8, align 4
  br label %209

209:                                              ; preds = %233, %208
  %210 = load i32, ptr %8, align 4
  %211 = icmp slt i32 %210, 2
  br i1 %211, label %212, label %236

212:                                              ; preds = %209
  %213 = load i32, ptr %6, align 4
  %214 = sext i32 %213 to i64
  %215 = getelementptr inbounds [1 x [2 x [2 x i8]]], ptr @g_199, i64 0, i64 %214
  %216 = load i32, ptr %7, align 4
  %217 = sext i32 %216 to i64
  %218 = getelementptr inbounds [2 x [2 x i8]], ptr %215, i64 0, i64 %217
  %219 = load i32, ptr %8, align 4
  %220 = sext i32 %219 to i64
  %221 = getelementptr inbounds [2 x i8], ptr %218, i64 0, i64 %220
  %222 = load volatile i8, ptr %221, align 1
  %223 = zext i8 %222 to i64
  %224 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %223, ptr noundef @.str.19, i32 noundef %224)
  %225 = load i32, ptr %9, align 4
  %226 = icmp ne i32 %225, 0
  br i1 %226, label %227, label %232

227:                                              ; preds = %212
  %228 = load i32, ptr %6, align 4
  %229 = load i32, ptr %7, align 4
  %230 = load i32, ptr %8, align 4
  %231 = call i32 (ptr, ...) @printf(ptr noundef @.str.4, i32 noundef %228, i32 noundef %229, i32 noundef %230)
  br label %232

232:                                              ; preds = %227, %212
  br label %233

233:                                              ; preds = %232
  %234 = load i32, ptr %8, align 4
  %235 = add nsw i32 %234, 1
  store i32 %235, ptr %8, align 4
  br label %209, !llvm.loop !15

236:                                              ; preds = %209
  br label %237

237:                                              ; preds = %236
  %238 = load i32, ptr %7, align 4
  %239 = add nsw i32 %238, 1
  store i32 %239, ptr %7, align 4
  br label %205, !llvm.loop !16

240:                                              ; preds = %205
  br label %241

241:                                              ; preds = %240
  %242 = load i32, ptr %6, align 4
  %243 = add nsw i32 %242, 1
  store i32 %243, ptr %6, align 4
  br label %201, !llvm.loop !17

244:                                              ; preds = %201
  %245 = load i16, ptr @g_220, align 2
  %246 = zext i16 %245 to i64
  %247 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %246, ptr noundef @.str.20, i32 noundef %247)
  %248 = load i16, ptr @g_235, align 2
  %249 = sext i16 %248 to i64
  %250 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %249, ptr noundef @.str.21, i32 noundef %250)
  %251 = load i32, ptr @g_236, align 4
  %252 = sext i32 %251 to i64
  %253 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %252, ptr noundef @.str.22, i32 noundef %253)
  %254 = load i8, ptr @g_237, align 1
  %255 = sext i8 %254 to i64
  %256 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %255, ptr noundef @.str.23, i32 noundef %256)
  %257 = load i8, ptr @g_238, align 1
  %258 = zext i8 %257 to i64
  %259 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %258, ptr noundef @.str.24, i32 noundef %259)
  %260 = load volatile i104, ptr @g_248, align 1
  %261 = shl i104 %260, 84
  %262 = ashr i104 %261, 84
  %263 = trunc i104 %262 to i32
  %264 = sext i32 %263 to i64
  %265 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %264, ptr noundef @.str.25, i32 noundef %265)
  %266 = load i104, ptr @g_248, align 1
  %267 = shl i104 %266, 59
  %268 = ashr i104 %267, 79
  %269 = trunc i104 %268 to i32
  %270 = sext i32 %269 to i64
  %271 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %270, ptr noundef @.str.26, i32 noundef %271)
  %272 = load i104, ptr @g_248, align 1
  %273 = lshr i104 %272, 45
  %274 = and i104 %273, 8388607
  %275 = trunc i104 %274 to i32
  %276 = zext i32 %275 to i64
  %277 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %276, ptr noundef @.str.27, i32 noundef %277)
  %278 = load volatile i104, ptr @g_248, align 1
  %279 = lshr i104 %278, 68
  %280 = and i104 %279, 127
  %281 = trunc i104 %280 to i32
  %282 = zext i32 %281 to i64
  %283 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %282, ptr noundef @.str.28, i32 noundef %283)
  %284 = load i104, ptr @g_248, align 1
  %285 = shl i104 %284, 3
  %286 = ashr i104 %285, 78
  %287 = trunc i104 %286 to i32
  %288 = sext i32 %287 to i64
  %289 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %288, ptr noundef @.str.29, i32 noundef %289)
  %290 = load i64, ptr @g_294, align 8
  %291 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %290, ptr noundef @.str.30, i32 noundef %291)
  store i32 0, ptr %6, align 4
  br label %292

292:                                              ; preds = %308, %244
  %293 = load i32, ptr %6, align 4
  %294 = icmp slt i32 %293, 5
  br i1 %294, label %295, label %311

295:                                              ; preds = %292
  %296 = load i32, ptr %6, align 4
  %297 = sext i32 %296 to i64
  %298 = getelementptr inbounds [5 x i8], ptr @g_303, i64 0, i64 %297
  %299 = load i8, ptr %298, align 1
  %300 = sext i8 %299 to i64
  %301 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %300, ptr noundef @.str.31, i32 noundef %301)
  %302 = load i32, ptr %9, align 4
  %303 = icmp ne i32 %302, 0
  br i1 %303, label %304, label %307

304:                                              ; preds = %295
  %305 = load i32, ptr %6, align 4
  %306 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %305)
  br label %307

307:                                              ; preds = %304, %295
  br label %308

308:                                              ; preds = %307
  %309 = load i32, ptr %6, align 4
  %310 = add nsw i32 %309, 1
  store i32 %310, ptr %6, align 4
  br label %292, !llvm.loop !18

311:                                              ; preds = %292
  %312 = load volatile i104, ptr @g_310, align 1
  %313 = shl i104 %312, 84
  %314 = ashr i104 %313, 84
  %315 = trunc i104 %314 to i32
  %316 = sext i32 %315 to i64
  %317 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %316, ptr noundef @.str.32, i32 noundef %317)
  %318 = load i104, ptr @g_310, align 1
  %319 = shl i104 %318, 59
  %320 = ashr i104 %319, 79
  %321 = trunc i104 %320 to i32
  %322 = sext i32 %321 to i64
  %323 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %322, ptr noundef @.str.33, i32 noundef %323)
  %324 = load i104, ptr @g_310, align 1
  %325 = lshr i104 %324, 45
  %326 = and i104 %325, 8388607
  %327 = trunc i104 %326 to i32
  %328 = zext i32 %327 to i64
  %329 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %328, ptr noundef @.str.34, i32 noundef %329)
  %330 = load volatile i104, ptr @g_310, align 1
  %331 = lshr i104 %330, 68
  %332 = and i104 %331, 127
  %333 = trunc i104 %332 to i32
  %334 = zext i32 %333 to i64
  %335 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %334, ptr noundef @.str.35, i32 noundef %335)
  %336 = load i104, ptr @g_310, align 1
  %337 = shl i104 %336, 3
  %338 = ashr i104 %337, 78
  %339 = trunc i104 %338 to i32
  %340 = sext i32 %339 to i64
  %341 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %340, ptr noundef @.str.36, i32 noundef %341)
  %342 = load i64, ptr @g_355, align 8
  %343 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %342, ptr noundef @.str.37, i32 noundef %343)
  store i32 0, ptr %6, align 4
  br label %344

344:                                              ; preds = %399, %311
  %345 = load i32, ptr %6, align 4
  %346 = icmp slt i32 %345, 4
  br i1 %346, label %347, label %402

347:                                              ; preds = %344
  %348 = load i32, ptr %6, align 4
  %349 = sext i32 %348 to i64
  %350 = getelementptr inbounds [4 x %struct.S0], ptr @g_365, i64 0, i64 %349
  %351 = load volatile i104, ptr %350, align 1
  %352 = shl i104 %351, 84
  %353 = ashr i104 %352, 84
  %354 = trunc i104 %353 to i32
  %355 = sext i32 %354 to i64
  %356 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %355, ptr noundef @.str.38, i32 noundef %356)
  %357 = load i32, ptr %6, align 4
  %358 = sext i32 %357 to i64
  %359 = getelementptr inbounds [4 x %struct.S0], ptr @g_365, i64 0, i64 %358
  %360 = load i104, ptr %359, align 1
  %361 = shl i104 %360, 59
  %362 = ashr i104 %361, 79
  %363 = trunc i104 %362 to i32
  %364 = sext i32 %363 to i64
  %365 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %364, ptr noundef @.str.39, i32 noundef %365)
  %366 = load i32, ptr %6, align 4
  %367 = sext i32 %366 to i64
  %368 = getelementptr inbounds [4 x %struct.S0], ptr @g_365, i64 0, i64 %367
  %369 = load i104, ptr %368, align 1
  %370 = lshr i104 %369, 45
  %371 = and i104 %370, 8388607
  %372 = trunc i104 %371 to i32
  %373 = zext i32 %372 to i64
  %374 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %373, ptr noundef @.str.40, i32 noundef %374)
  %375 = load i32, ptr %6, align 4
  %376 = sext i32 %375 to i64
  %377 = getelementptr inbounds [4 x %struct.S0], ptr @g_365, i64 0, i64 %376
  %378 = load volatile i104, ptr %377, align 1
  %379 = lshr i104 %378, 68
  %380 = and i104 %379, 127
  %381 = trunc i104 %380 to i32
  %382 = zext i32 %381 to i64
  %383 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %382, ptr noundef @.str.41, i32 noundef %383)
  %384 = load i32, ptr %6, align 4
  %385 = sext i32 %384 to i64
  %386 = getelementptr inbounds [4 x %struct.S0], ptr @g_365, i64 0, i64 %385
  %387 = load i104, ptr %386, align 1
  %388 = shl i104 %387, 3
  %389 = ashr i104 %388, 78
  %390 = trunc i104 %389 to i32
  %391 = sext i32 %390 to i64
  %392 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %391, ptr noundef @.str.42, i32 noundef %392)
  %393 = load i32, ptr %9, align 4
  %394 = icmp ne i32 %393, 0
  br i1 %394, label %395, label %398

395:                                              ; preds = %347
  %396 = load i32, ptr %6, align 4
  %397 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %396)
  br label %398

398:                                              ; preds = %395, %347
  br label %399

399:                                              ; preds = %398
  %400 = load i32, ptr %6, align 4
  %401 = add nsw i32 %400, 1
  store i32 %401, ptr %6, align 4
  br label %344, !llvm.loop !19

402:                                              ; preds = %344
  store i32 0, ptr %6, align 4
  br label %403

403:                                              ; preds = %431, %402
  %404 = load i32, ptr %6, align 4
  %405 = icmp slt i32 %404, 2
  br i1 %405, label %406, label %434

406:                                              ; preds = %403
  %407 = load i32, ptr %6, align 4
  %408 = sext i32 %407 to i64
  %409 = getelementptr inbounds [2 x %union.U1], ptr @g_473, i64 0, i64 %408
  %410 = load volatile i32, ptr %409, align 8
  %411 = sext i32 %410 to i64
  %412 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %411, ptr noundef @.str.43, i32 noundef %412)
  %413 = load i32, ptr %6, align 4
  %414 = sext i32 %413 to i64
  %415 = getelementptr inbounds [2 x %union.U1], ptr @g_473, i64 0, i64 %414
  %416 = load volatile i8, ptr %415, align 8
  %417 = zext i8 %416 to i64
  %418 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %417, ptr noundef @.str.44, i32 noundef %418)
  %419 = load i32, ptr %6, align 4
  %420 = sext i32 %419 to i64
  %421 = getelementptr inbounds [2 x %union.U1], ptr @g_473, i64 0, i64 %420
  %422 = load volatile i32, ptr %421, align 8
  %423 = sext i32 %422 to i64
  %424 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %423, ptr noundef @.str.45, i32 noundef %424)
  %425 = load i32, ptr %9, align 4
  %426 = icmp ne i32 %425, 0
  br i1 %426, label %427, label %430

427:                                              ; preds = %406
  %428 = load i32, ptr %6, align 4
  %429 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %428)
  br label %430

430:                                              ; preds = %427, %406
  br label %431

431:                                              ; preds = %430
  %432 = load i32, ptr %6, align 4
  %433 = add nsw i32 %432, 1
  store i32 %433, ptr %6, align 4
  br label %403, !llvm.loop !20

434:                                              ; preds = %403
  %435 = load i32, ptr @g_486, align 4
  %436 = zext i32 %435 to i64
  %437 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %436, ptr noundef @.str.46, i32 noundef %437)
  %438 = load i32, ptr @g_516, align 8
  %439 = sext i32 %438 to i64
  %440 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %439, ptr noundef @.str.47, i32 noundef %440)
  %441 = load i8, ptr @g_516, align 8
  %442 = zext i8 %441 to i64
  %443 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %442, ptr noundef @.str.48, i32 noundef %443)
  %444 = load volatile i32, ptr @g_516, align 8
  %445 = sext i32 %444 to i64
  %446 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %445, ptr noundef @.str.49, i32 noundef %446)
  %447 = load volatile i104, ptr @g_542, align 1
  %448 = shl i104 %447, 84
  %449 = ashr i104 %448, 84
  %450 = trunc i104 %449 to i32
  %451 = sext i32 %450 to i64
  %452 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %451, ptr noundef @.str.50, i32 noundef %452)
  %453 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef -133, ptr noundef @.str.51, i32 noundef %453)
  %454 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef 2406, ptr noundef @.str.52, i32 noundef %454)
  %455 = load volatile i104, ptr @g_542, align 1
  %456 = lshr i104 %455, 68
  %457 = and i104 %456, 127
  %458 = trunc i104 %457 to i32
  %459 = zext i32 %458 to i64
  %460 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %459, ptr noundef @.str.53, i32 noundef %460)
  %461 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef -4349, ptr noundef @.str.54, i32 noundef %461)
  %462 = load i32, ptr @g_567, align 8
  %463 = sext i32 %462 to i64
  %464 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %463, ptr noundef @.str.55, i32 noundef %464)
  %465 = load i8, ptr @g_567, align 8
  %466 = zext i8 %465 to i64
  %467 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %466, ptr noundef @.str.56, i32 noundef %467)
  %468 = load volatile i32, ptr @g_567, align 8
  %469 = sext i32 %468 to i64
  %470 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %469, ptr noundef @.str.57, i32 noundef %470)
  %471 = load i16, ptr @g_584, align 2
  %472 = zext i16 %471 to i64
  %473 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %472, ptr noundef @.str.58, i32 noundef %473)
  %474 = load volatile i104, ptr @g_590, align 1
  %475 = shl i104 %474, 84
  %476 = ashr i104 %475, 84
  %477 = trunc i104 %476 to i32
  %478 = sext i32 %477 to i64
  %479 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %478, ptr noundef @.str.59, i32 noundef %479)
  %480 = load i104, ptr @g_590, align 1
  %481 = shl i104 %480, 59
  %482 = ashr i104 %481, 79
  %483 = trunc i104 %482 to i32
  %484 = sext i32 %483 to i64
  %485 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %484, ptr noundef @.str.60, i32 noundef %485)
  %486 = load i104, ptr @g_590, align 1
  %487 = lshr i104 %486, 45
  %488 = and i104 %487, 8388607
  %489 = trunc i104 %488 to i32
  %490 = zext i32 %489 to i64
  %491 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %490, ptr noundef @.str.61, i32 noundef %491)
  %492 = load volatile i104, ptr @g_590, align 1
  %493 = lshr i104 %492, 68
  %494 = and i104 %493, 127
  %495 = trunc i104 %494 to i32
  %496 = zext i32 %495 to i64
  %497 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %496, ptr noundef @.str.62, i32 noundef %497)
  %498 = load i104, ptr @g_590, align 1
  %499 = shl i104 %498, 3
  %500 = ashr i104 %499, 78
  %501 = trunc i104 %500 to i32
  %502 = sext i32 %501 to i64
  %503 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %502, ptr noundef @.str.63, i32 noundef %503)
  %504 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef 217, ptr noundef @.str.64, i32 noundef %504)
  %505 = load volatile i104, ptr @g_671, align 1
  %506 = shl i104 %505, 84
  %507 = ashr i104 %506, 84
  %508 = trunc i104 %507 to i32
  %509 = sext i32 %508 to i64
  %510 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %509, ptr noundef @.str.65, i32 noundef %510)
  %511 = load volatile i104, ptr @g_671, align 1
  %512 = shl i104 %511, 59
  %513 = ashr i104 %512, 79
  %514 = trunc i104 %513 to i32
  %515 = sext i32 %514 to i64
  %516 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %515, ptr noundef @.str.66, i32 noundef %516)
  %517 = load volatile i104, ptr @g_671, align 1
  %518 = lshr i104 %517, 45
  %519 = and i104 %518, 8388607
  %520 = trunc i104 %519 to i32
  %521 = zext i32 %520 to i64
  %522 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %521, ptr noundef @.str.67, i32 noundef %522)
  %523 = load volatile i104, ptr @g_671, align 1
  %524 = lshr i104 %523, 68
  %525 = and i104 %524, 127
  %526 = trunc i104 %525 to i32
  %527 = zext i32 %526 to i64
  %528 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %527, ptr noundef @.str.68, i32 noundef %528)
  %529 = load volatile i104, ptr @g_671, align 1
  %530 = shl i104 %529, 3
  %531 = ashr i104 %530, 78
  %532 = trunc i104 %531 to i32
  %533 = sext i32 %532 to i64
  %534 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %533, ptr noundef @.str.69, i32 noundef %534)
  %535 = load i8, ptr @g_723, align 1
  %536 = sext i8 %535 to i64
  %537 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %536, ptr noundef @.str.70, i32 noundef %537)
  %538 = load i64, ptr @g_782, align 8
  %539 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %538, ptr noundef @.str.71, i32 noundef %539)
  %540 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef 1019426258, ptr noundef @.str.72, i32 noundef %540)
  %541 = load i16, ptr @g_833, align 2
  %542 = zext i16 %541 to i64
  %543 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %542, ptr noundef @.str.73, i32 noundef %543)
  %544 = load volatile i104, ptr @g_839, align 1
  %545 = shl i104 %544, 84
  %546 = ashr i104 %545, 84
  %547 = trunc i104 %546 to i32
  %548 = sext i32 %547 to i64
  %549 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %548, ptr noundef @.str.74, i32 noundef %549)
  %550 = load i104, ptr @g_839, align 1
  %551 = shl i104 %550, 59
  %552 = ashr i104 %551, 79
  %553 = trunc i104 %552 to i32
  %554 = sext i32 %553 to i64
  %555 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %554, ptr noundef @.str.75, i32 noundef %555)
  %556 = load i104, ptr @g_839, align 1
  %557 = lshr i104 %556, 45
  %558 = and i104 %557, 8388607
  %559 = trunc i104 %558 to i32
  %560 = zext i32 %559 to i64
  %561 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %560, ptr noundef @.str.76, i32 noundef %561)
  %562 = load volatile i104, ptr @g_839, align 1
  %563 = lshr i104 %562, 68
  %564 = and i104 %563, 127
  %565 = trunc i104 %564 to i32
  %566 = zext i32 %565 to i64
  %567 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %566, ptr noundef @.str.77, i32 noundef %567)
  %568 = load i104, ptr @g_839, align 1
  %569 = shl i104 %568, 3
  %570 = ashr i104 %569, 78
  %571 = trunc i104 %570 to i32
  %572 = sext i32 %571 to i64
  %573 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %572, ptr noundef @.str.78, i32 noundef %573)
  %574 = load volatile i104, ptr @g_842, align 1
  %575 = shl i104 %574, 84
  %576 = ashr i104 %575, 84
  %577 = trunc i104 %576 to i32
  %578 = sext i32 %577 to i64
  %579 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %578, ptr noundef @.str.79, i32 noundef %579)
  %580 = load i104, ptr @g_842, align 1
  %581 = shl i104 %580, 59
  %582 = ashr i104 %581, 79
  %583 = trunc i104 %582 to i32
  %584 = sext i32 %583 to i64
  %585 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %584, ptr noundef @.str.80, i32 noundef %585)
  %586 = load i104, ptr @g_842, align 1
  %587 = lshr i104 %586, 45
  %588 = and i104 %587, 8388607
  %589 = trunc i104 %588 to i32
  %590 = zext i32 %589 to i64
  %591 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %590, ptr noundef @.str.81, i32 noundef %591)
  %592 = load volatile i104, ptr @g_842, align 1
  %593 = lshr i104 %592, 68
  %594 = and i104 %593, 127
  %595 = trunc i104 %594 to i32
  %596 = zext i32 %595 to i64
  %597 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %596, ptr noundef @.str.82, i32 noundef %597)
  %598 = load i104, ptr @g_842, align 1
  %599 = shl i104 %598, 3
  %600 = ashr i104 %599, 78
  %601 = trunc i104 %600 to i32
  %602 = sext i32 %601 to i64
  %603 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %602, ptr noundef @.str.83, i32 noundef %603)
  store i32 0, ptr %6, align 4
  br label %604

604:                                              ; preds = %620, %434
  %605 = load i32, ptr %6, align 4
  %606 = icmp slt i32 %605, 7
  br i1 %606, label %607, label %623

607:                                              ; preds = %604
  %608 = load i32, ptr %6, align 4
  %609 = sext i32 %608 to i64
  %610 = getelementptr inbounds [7 x i32], ptr @g_880, i64 0, i64 %609
  %611 = load i32, ptr %610, align 4
  %612 = zext i32 %611 to i64
  %613 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %612, ptr noundef @.str.84, i32 noundef %613)
  %614 = load i32, ptr %9, align 4
  %615 = icmp ne i32 %614, 0
  br i1 %615, label %616, label %619

616:                                              ; preds = %607
  %617 = load i32, ptr %6, align 4
  %618 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %617)
  br label %619

619:                                              ; preds = %616, %607
  br label %620

620:                                              ; preds = %619
  %621 = load i32, ptr %6, align 4
  %622 = add nsw i32 %621, 1
  store i32 %622, ptr %6, align 4
  br label %604, !llvm.loop !21

623:                                              ; preds = %604
  %624 = load volatile i104, ptr @g_923, align 1
  %625 = shl i104 %624, 84
  %626 = ashr i104 %625, 84
  %627 = trunc i104 %626 to i32
  %628 = sext i32 %627 to i64
  %629 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %628, ptr noundef @.str.85, i32 noundef %629)
  %630 = load volatile i104, ptr @g_923, align 1
  %631 = shl i104 %630, 59
  %632 = ashr i104 %631, 79
  %633 = trunc i104 %632 to i32
  %634 = sext i32 %633 to i64
  %635 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %634, ptr noundef @.str.86, i32 noundef %635)
  %636 = load volatile i104, ptr @g_923, align 1
  %637 = lshr i104 %636, 45
  %638 = and i104 %637, 8388607
  %639 = trunc i104 %638 to i32
  %640 = zext i32 %639 to i64
  %641 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %640, ptr noundef @.str.87, i32 noundef %641)
  %642 = load volatile i104, ptr @g_923, align 1
  %643 = lshr i104 %642, 68
  %644 = and i104 %643, 127
  %645 = trunc i104 %644 to i32
  %646 = zext i32 %645 to i64
  %647 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %646, ptr noundef @.str.88, i32 noundef %647)
  %648 = load volatile i104, ptr @g_923, align 1
  %649 = shl i104 %648, 3
  %650 = ashr i104 %649, 78
  %651 = trunc i104 %650 to i32
  %652 = sext i32 %651 to i64
  %653 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %652, ptr noundef @.str.89, i32 noundef %653)
  %654 = load volatile i104, ptr @g_924, align 1
  %655 = shl i104 %654, 84
  %656 = ashr i104 %655, 84
  %657 = trunc i104 %656 to i32
  %658 = sext i32 %657 to i64
  %659 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %658, ptr noundef @.str.90, i32 noundef %659)
  %660 = load volatile i104, ptr @g_924, align 1
  %661 = shl i104 %660, 59
  %662 = ashr i104 %661, 79
  %663 = trunc i104 %662 to i32
  %664 = sext i32 %663 to i64
  %665 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %664, ptr noundef @.str.91, i32 noundef %665)
  %666 = load volatile i104, ptr @g_924, align 1
  %667 = lshr i104 %666, 45
  %668 = and i104 %667, 8388607
  %669 = trunc i104 %668 to i32
  %670 = zext i32 %669 to i64
  %671 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %670, ptr noundef @.str.92, i32 noundef %671)
  %672 = load volatile i104, ptr @g_924, align 1
  %673 = lshr i104 %672, 68
  %674 = and i104 %673, 127
  %675 = trunc i104 %674 to i32
  %676 = zext i32 %675 to i64
  %677 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %676, ptr noundef @.str.93, i32 noundef %677)
  %678 = load volatile i104, ptr @g_924, align 1
  %679 = shl i104 %678, 3
  %680 = ashr i104 %679, 78
  %681 = trunc i104 %680 to i32
  %682 = sext i32 %681 to i64
  %683 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %682, ptr noundef @.str.94, i32 noundef %683)
  %684 = load volatile i104, ptr @g_936, align 1
  %685 = shl i104 %684, 84
  %686 = ashr i104 %685, 84
  %687 = trunc i104 %686 to i32
  %688 = sext i32 %687 to i64
  %689 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %688, ptr noundef @.str.95, i32 noundef %689)
  %690 = load i104, ptr @g_936, align 1
  %691 = shl i104 %690, 59
  %692 = ashr i104 %691, 79
  %693 = trunc i104 %692 to i32
  %694 = sext i32 %693 to i64
  %695 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %694, ptr noundef @.str.96, i32 noundef %695)
  %696 = load i104, ptr @g_936, align 1
  %697 = lshr i104 %696, 45
  %698 = and i104 %697, 8388607
  %699 = trunc i104 %698 to i32
  %700 = zext i32 %699 to i64
  %701 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %700, ptr noundef @.str.97, i32 noundef %701)
  %702 = load volatile i104, ptr @g_936, align 1
  %703 = lshr i104 %702, 68
  %704 = and i104 %703, 127
  %705 = trunc i104 %704 to i32
  %706 = zext i32 %705 to i64
  %707 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %706, ptr noundef @.str.98, i32 noundef %707)
  %708 = load i104, ptr @g_936, align 1
  %709 = shl i104 %708, 3
  %710 = ashr i104 %709, 78
  %711 = trunc i104 %710 to i32
  %712 = sext i32 %711 to i64
  %713 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %712, ptr noundef @.str.99, i32 noundef %713)
  %714 = load i32, ptr @g_1026, align 4
  %715 = sext i32 %714 to i64
  %716 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %715, ptr noundef @.str.100, i32 noundef %716)
  %717 = load volatile i104, ptr @g_1038, align 1
  %718 = shl i104 %717, 84
  %719 = ashr i104 %718, 84
  %720 = trunc i104 %719 to i32
  %721 = sext i32 %720 to i64
  %722 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %721, ptr noundef @.str.101, i32 noundef %722)
  %723 = load volatile i104, ptr @g_1038, align 1
  %724 = shl i104 %723, 59
  %725 = ashr i104 %724, 79
  %726 = trunc i104 %725 to i32
  %727 = sext i32 %726 to i64
  %728 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %727, ptr noundef @.str.102, i32 noundef %728)
  %729 = load volatile i104, ptr @g_1038, align 1
  %730 = lshr i104 %729, 45
  %731 = and i104 %730, 8388607
  %732 = trunc i104 %731 to i32
  %733 = zext i32 %732 to i64
  %734 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %733, ptr noundef @.str.103, i32 noundef %734)
  %735 = load volatile i104, ptr @g_1038, align 1
  %736 = lshr i104 %735, 68
  %737 = and i104 %736, 127
  %738 = trunc i104 %737 to i32
  %739 = zext i32 %738 to i64
  %740 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %739, ptr noundef @.str.104, i32 noundef %740)
  %741 = load volatile i104, ptr @g_1038, align 1
  %742 = shl i104 %741, 3
  %743 = ashr i104 %742, 78
  %744 = trunc i104 %743 to i32
  %745 = sext i32 %744 to i64
  %746 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %745, ptr noundef @.str.105, i32 noundef %746)
  %747 = load volatile i104, ptr @g_1152, align 1
  %748 = shl i104 %747, 84
  %749 = ashr i104 %748, 84
  %750 = trunc i104 %749 to i32
  %751 = sext i32 %750 to i64
  %752 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %751, ptr noundef @.str.106, i32 noundef %752)
  %753 = load i104, ptr @g_1152, align 1
  %754 = shl i104 %753, 59
  %755 = ashr i104 %754, 79
  %756 = trunc i104 %755 to i32
  %757 = sext i32 %756 to i64
  %758 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %757, ptr noundef @.str.107, i32 noundef %758)
  %759 = load i104, ptr @g_1152, align 1
  %760 = lshr i104 %759, 45
  %761 = and i104 %760, 8388607
  %762 = trunc i104 %761 to i32
  %763 = zext i32 %762 to i64
  %764 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %763, ptr noundef @.str.108, i32 noundef %764)
  %765 = load volatile i104, ptr @g_1152, align 1
  %766 = lshr i104 %765, 68
  %767 = and i104 %766, 127
  %768 = trunc i104 %767 to i32
  %769 = zext i32 %768 to i64
  %770 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %769, ptr noundef @.str.109, i32 noundef %770)
  %771 = load i104, ptr @g_1152, align 1
  %772 = shl i104 %771, 3
  %773 = ashr i104 %772, 78
  %774 = trunc i104 %773 to i32
  %775 = sext i32 %774 to i64
  %776 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %775, ptr noundef @.str.110, i32 noundef %776)
  %777 = load volatile i32, ptr @g_1191, align 8
  %778 = sext i32 %777 to i64
  %779 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %778, ptr noundef @.str.111, i32 noundef %779)
  %780 = load volatile i8, ptr @g_1191, align 8
  %781 = zext i8 %780 to i64
  %782 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %781, ptr noundef @.str.112, i32 noundef %782)
  %783 = load volatile i32, ptr @g_1191, align 8
  %784 = sext i32 %783 to i64
  %785 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %784, ptr noundef @.str.113, i32 noundef %785)
  %786 = load i8, ptr @g_1245, align 1
  %787 = zext i8 %786 to i64
  %788 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %787, ptr noundef @.str.114, i32 noundef %788)
  store i32 0, ptr %6, align 4
  br label %789

789:                                              ; preds = %835, %623
  %790 = load i32, ptr %6, align 4
  %791 = icmp slt i32 %790, 1
  br i1 %791, label %792, label %838

792:                                              ; preds = %789
  store i32 0, ptr %7, align 4
  br label %793

793:                                              ; preds = %831, %792
  %794 = load i32, ptr %7, align 4
  %795 = icmp slt i32 %794, 5
  br i1 %795, label %796, label %834

796:                                              ; preds = %793
  %797 = load i32, ptr %6, align 4
  %798 = sext i32 %797 to i64
  %799 = getelementptr inbounds [1 x [5 x %union.U1]], ptr @g_1263, i64 0, i64 %798
  %800 = load i32, ptr %7, align 4
  %801 = sext i32 %800 to i64
  %802 = getelementptr inbounds [5 x %union.U1], ptr %799, i64 0, i64 %801
  %803 = load volatile i32, ptr %802, align 8
  %804 = sext i32 %803 to i64
  %805 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %804, ptr noundef @.str.115, i32 noundef %805)
  %806 = load i32, ptr %6, align 4
  %807 = sext i32 %806 to i64
  %808 = getelementptr inbounds [1 x [5 x %union.U1]], ptr @g_1263, i64 0, i64 %807
  %809 = load i32, ptr %7, align 4
  %810 = sext i32 %809 to i64
  %811 = getelementptr inbounds [5 x %union.U1], ptr %808, i64 0, i64 %810
  %812 = load volatile i8, ptr %811, align 8
  %813 = zext i8 %812 to i64
  %814 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %813, ptr noundef @.str.116, i32 noundef %814)
  %815 = load i32, ptr %6, align 4
  %816 = sext i32 %815 to i64
  %817 = getelementptr inbounds [1 x [5 x %union.U1]], ptr @g_1263, i64 0, i64 %816
  %818 = load i32, ptr %7, align 4
  %819 = sext i32 %818 to i64
  %820 = getelementptr inbounds [5 x %union.U1], ptr %817, i64 0, i64 %819
  %821 = load volatile i32, ptr %820, align 8
  %822 = sext i32 %821 to i64
  %823 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %822, ptr noundef @.str.117, i32 noundef %823)
  %824 = load i32, ptr %9, align 4
  %825 = icmp ne i32 %824, 0
  br i1 %825, label %826, label %830

826:                                              ; preds = %796
  %827 = load i32, ptr %6, align 4
  %828 = load i32, ptr %7, align 4
  %829 = call i32 (ptr, ...) @printf(ptr noundef @.str.118, i32 noundef %827, i32 noundef %828)
  br label %830

830:                                              ; preds = %826, %796
  br label %831

831:                                              ; preds = %830
  %832 = load i32, ptr %7, align 4
  %833 = add nsw i32 %832, 1
  store i32 %833, ptr %7, align 4
  br label %793, !llvm.loop !22

834:                                              ; preds = %793
  br label %835

835:                                              ; preds = %834
  %836 = load i32, ptr %6, align 4
  %837 = add nsw i32 %836, 1
  store i32 %837, ptr %6, align 4
  br label %789, !llvm.loop !23

838:                                              ; preds = %789
  %839 = load volatile i64, ptr @g_1303, align 8
  %840 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %839, ptr noundef @.str.119, i32 noundef %840)
  %841 = load volatile i64, ptr @g_1304, align 8
  %842 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %841, ptr noundef @.str.120, i32 noundef %842)
  %843 = load volatile i64, ptr @g_1305, align 8
  %844 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %843, ptr noundef @.str.121, i32 noundef %844)
  %845 = load volatile i64, ptr @g_1306, align 8
  %846 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %845, ptr noundef @.str.122, i32 noundef %846)
  %847 = load volatile i64, ptr @g_1307, align 8
  %848 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %847, ptr noundef @.str.123, i32 noundef %848)
  %849 = load volatile i64, ptr @g_1308, align 8
  %850 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %849, ptr noundef @.str.124, i32 noundef %850)
  %851 = load volatile i64, ptr @g_1309, align 8
  %852 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %851, ptr noundef @.str.125, i32 noundef %852)
  %853 = load volatile i64, ptr @g_1310, align 8
  %854 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %853, ptr noundef @.str.126, i32 noundef %854)
  store i32 0, ptr %6, align 4
  br label %855

855:                                              ; preds = %870, %838
  %856 = load i32, ptr %6, align 4
  %857 = icmp slt i32 %856, 6
  br i1 %857, label %858, label %873

858:                                              ; preds = %855
  %859 = load i32, ptr %6, align 4
  %860 = sext i32 %859 to i64
  %861 = getelementptr inbounds [6 x i64], ptr @g_1311, i64 0, i64 %860
  %862 = load volatile i64, ptr %861, align 8
  %863 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %862, ptr noundef @.str.127, i32 noundef %863)
  %864 = load i32, ptr %9, align 4
  %865 = icmp ne i32 %864, 0
  br i1 %865, label %866, label %869

866:                                              ; preds = %858
  %867 = load i32, ptr %6, align 4
  %868 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %867)
  br label %869

869:                                              ; preds = %866, %858
  br label %870

870:                                              ; preds = %869
  %871 = load i32, ptr %6, align 4
  %872 = add nsw i32 %871, 1
  store i32 %872, ptr %6, align 4
  br label %855, !llvm.loop !24

873:                                              ; preds = %855
  store i32 0, ptr %6, align 4
  br label %874

874:                                              ; preds = %889, %873
  %875 = load i32, ptr %6, align 4
  %876 = icmp slt i32 %875, 8
  br i1 %876, label %877, label %892

877:                                              ; preds = %874
  %878 = load i32, ptr %6, align 4
  %879 = sext i32 %878 to i64
  %880 = getelementptr inbounds [8 x i64], ptr @g_1312, i64 0, i64 %879
  %881 = load volatile i64, ptr %880, align 8
  %882 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %881, ptr noundef @.str.128, i32 noundef %882)
  %883 = load i32, ptr %9, align 4
  %884 = icmp ne i32 %883, 0
  br i1 %884, label %885, label %888

885:                                              ; preds = %877
  %886 = load i32, ptr %6, align 4
  %887 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %886)
  br label %888

888:                                              ; preds = %885, %877
  br label %889

889:                                              ; preds = %888
  %890 = load i32, ptr %6, align 4
  %891 = add nsw i32 %890, 1
  store i32 %891, ptr %6, align 4
  br label %874, !llvm.loop !25

892:                                              ; preds = %874
  %893 = load volatile i64, ptr @g_1313, align 8
  %894 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %893, ptr noundef @.str.129, i32 noundef %894)
  %895 = load volatile i64, ptr @g_1314, align 8
  %896 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %895, ptr noundef @.str.130, i32 noundef %896)
  %897 = load volatile i64, ptr @g_1315, align 8
  %898 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %897, ptr noundef @.str.131, i32 noundef %898)
  store i32 0, ptr %6, align 4
  br label %899

899:                                              ; preds = %954, %892
  %900 = load i32, ptr %6, align 4
  %901 = icmp slt i32 %900, 1
  br i1 %901, label %902, label %957

902:                                              ; preds = %899
  %903 = load i32, ptr %6, align 4
  %904 = sext i32 %903 to i64
  %905 = getelementptr inbounds [1 x %struct.S0], ptr @g_1412, i64 0, i64 %904
  %906 = load volatile i104, ptr %905, align 1
  %907 = shl i104 %906, 84
  %908 = ashr i104 %907, 84
  %909 = trunc i104 %908 to i32
  %910 = sext i32 %909 to i64
  %911 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %910, ptr noundef @.str.132, i32 noundef %911)
  %912 = load i32, ptr %6, align 4
  %913 = sext i32 %912 to i64
  %914 = getelementptr inbounds [1 x %struct.S0], ptr @g_1412, i64 0, i64 %913
  %915 = load i104, ptr %914, align 1
  %916 = shl i104 %915, 59
  %917 = ashr i104 %916, 79
  %918 = trunc i104 %917 to i32
  %919 = sext i32 %918 to i64
  %920 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %919, ptr noundef @.str.133, i32 noundef %920)
  %921 = load i32, ptr %6, align 4
  %922 = sext i32 %921 to i64
  %923 = getelementptr inbounds [1 x %struct.S0], ptr @g_1412, i64 0, i64 %922
  %924 = load i104, ptr %923, align 1
  %925 = lshr i104 %924, 45
  %926 = and i104 %925, 8388607
  %927 = trunc i104 %926 to i32
  %928 = zext i32 %927 to i64
  %929 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %928, ptr noundef @.str.134, i32 noundef %929)
  %930 = load i32, ptr %6, align 4
  %931 = sext i32 %930 to i64
  %932 = getelementptr inbounds [1 x %struct.S0], ptr @g_1412, i64 0, i64 %931
  %933 = load volatile i104, ptr %932, align 1
  %934 = lshr i104 %933, 68
  %935 = and i104 %934, 127
  %936 = trunc i104 %935 to i32
  %937 = zext i32 %936 to i64
  %938 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %937, ptr noundef @.str.135, i32 noundef %938)
  %939 = load i32, ptr %6, align 4
  %940 = sext i32 %939 to i64
  %941 = getelementptr inbounds [1 x %struct.S0], ptr @g_1412, i64 0, i64 %940
  %942 = load i104, ptr %941, align 1
  %943 = shl i104 %942, 3
  %944 = ashr i104 %943, 78
  %945 = trunc i104 %944 to i32
  %946 = sext i32 %945 to i64
  %947 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %946, ptr noundef @.str.136, i32 noundef %947)
  %948 = load i32, ptr %9, align 4
  %949 = icmp ne i32 %948, 0
  br i1 %949, label %950, label %953

950:                                              ; preds = %902
  %951 = load i32, ptr %6, align 4
  %952 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %951)
  br label %953

953:                                              ; preds = %950, %902
  br label %954

954:                                              ; preds = %953
  %955 = load i32, ptr %6, align 4
  %956 = add nsw i32 %955, 1
  store i32 %956, ptr %6, align 4
  br label %899, !llvm.loop !26

957:                                              ; preds = %899
  store i32 0, ptr %6, align 4
  br label %958

958:                                              ; preds = %1013, %957
  %959 = load i32, ptr %6, align 4
  %960 = icmp slt i32 %959, 10
  br i1 %960, label %961, label %1016

961:                                              ; preds = %958
  %962 = load i32, ptr %6, align 4
  %963 = sext i32 %962 to i64
  %964 = getelementptr inbounds [10 x %struct.S0], ptr @g_1438, i64 0, i64 %963
  %965 = load volatile i104, ptr %964, align 1
  %966 = shl i104 %965, 84
  %967 = ashr i104 %966, 84
  %968 = trunc i104 %967 to i32
  %969 = sext i32 %968 to i64
  %970 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %969, ptr noundef @.str.137, i32 noundef %970)
  %971 = load i32, ptr %6, align 4
  %972 = sext i32 %971 to i64
  %973 = getelementptr inbounds [10 x %struct.S0], ptr @g_1438, i64 0, i64 %972
  %974 = load i104, ptr %973, align 1
  %975 = shl i104 %974, 59
  %976 = ashr i104 %975, 79
  %977 = trunc i104 %976 to i32
  %978 = sext i32 %977 to i64
  %979 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %978, ptr noundef @.str.138, i32 noundef %979)
  %980 = load i32, ptr %6, align 4
  %981 = sext i32 %980 to i64
  %982 = getelementptr inbounds [10 x %struct.S0], ptr @g_1438, i64 0, i64 %981
  %983 = load i104, ptr %982, align 1
  %984 = lshr i104 %983, 45
  %985 = and i104 %984, 8388607
  %986 = trunc i104 %985 to i32
  %987 = zext i32 %986 to i64
  %988 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %987, ptr noundef @.str.139, i32 noundef %988)
  %989 = load i32, ptr %6, align 4
  %990 = sext i32 %989 to i64
  %991 = getelementptr inbounds [10 x %struct.S0], ptr @g_1438, i64 0, i64 %990
  %992 = load volatile i104, ptr %991, align 1
  %993 = lshr i104 %992, 68
  %994 = and i104 %993, 127
  %995 = trunc i104 %994 to i32
  %996 = zext i32 %995 to i64
  %997 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %996, ptr noundef @.str.140, i32 noundef %997)
  %998 = load i32, ptr %6, align 4
  %999 = sext i32 %998 to i64
  %1000 = getelementptr inbounds [10 x %struct.S0], ptr @g_1438, i64 0, i64 %999
  %1001 = load i104, ptr %1000, align 1
  %1002 = shl i104 %1001, 3
  %1003 = ashr i104 %1002, 78
  %1004 = trunc i104 %1003 to i32
  %1005 = sext i32 %1004 to i64
  %1006 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1005, ptr noundef @.str.141, i32 noundef %1006)
  %1007 = load i32, ptr %9, align 4
  %1008 = icmp ne i32 %1007, 0
  br i1 %1008, label %1009, label %1012

1009:                                             ; preds = %961
  %1010 = load i32, ptr %6, align 4
  %1011 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %1010)
  br label %1012

1012:                                             ; preds = %1009, %961
  br label %1013

1013:                                             ; preds = %1012
  %1014 = load i32, ptr %6, align 4
  %1015 = add nsw i32 %1014, 1
  store i32 %1015, ptr %6, align 4
  br label %958, !llvm.loop !27

1016:                                             ; preds = %958
  %1017 = load volatile i104, ptr @g_1484, align 1
  %1018 = shl i104 %1017, 84
  %1019 = ashr i104 %1018, 84
  %1020 = trunc i104 %1019 to i32
  %1021 = sext i32 %1020 to i64
  %1022 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1021, ptr noundef @.str.142, i32 noundef %1022)
  %1023 = load i104, ptr @g_1484, align 1
  %1024 = shl i104 %1023, 59
  %1025 = ashr i104 %1024, 79
  %1026 = trunc i104 %1025 to i32
  %1027 = sext i32 %1026 to i64
  %1028 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1027, ptr noundef @.str.143, i32 noundef %1028)
  %1029 = load i104, ptr @g_1484, align 1
  %1030 = lshr i104 %1029, 45
  %1031 = and i104 %1030, 8388607
  %1032 = trunc i104 %1031 to i32
  %1033 = zext i32 %1032 to i64
  %1034 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1033, ptr noundef @.str.144, i32 noundef %1034)
  %1035 = load volatile i104, ptr @g_1484, align 1
  %1036 = lshr i104 %1035, 68
  %1037 = and i104 %1036, 127
  %1038 = trunc i104 %1037 to i32
  %1039 = zext i32 %1038 to i64
  %1040 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1039, ptr noundef @.str.145, i32 noundef %1040)
  %1041 = load i104, ptr @g_1484, align 1
  %1042 = shl i104 %1041, 3
  %1043 = ashr i104 %1042, 78
  %1044 = trunc i104 %1043 to i32
  %1045 = sext i32 %1044 to i64
  %1046 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1045, ptr noundef @.str.146, i32 noundef %1046)
  store i32 0, ptr %6, align 4
  br label %1047

1047:                                             ; preds = %1075, %1016
  %1048 = load i32, ptr %6, align 4
  %1049 = icmp slt i32 %1048, 5
  br i1 %1049, label %1050, label %1078

1050:                                             ; preds = %1047
  %1051 = load i32, ptr %6, align 4
  %1052 = sext i32 %1051 to i64
  %1053 = getelementptr inbounds [5 x %union.U1], ptr @g_1531, i64 0, i64 %1052
  %1054 = load i32, ptr %1053, align 8
  %1055 = sext i32 %1054 to i64
  %1056 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1055, ptr noundef @.str.147, i32 noundef %1056)
  %1057 = load i32, ptr %6, align 4
  %1058 = sext i32 %1057 to i64
  %1059 = getelementptr inbounds [5 x %union.U1], ptr @g_1531, i64 0, i64 %1058
  %1060 = load i8, ptr %1059, align 8
  %1061 = zext i8 %1060 to i64
  %1062 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1061, ptr noundef @.str.148, i32 noundef %1062)
  %1063 = load i32, ptr %6, align 4
  %1064 = sext i32 %1063 to i64
  %1065 = getelementptr inbounds [5 x %union.U1], ptr @g_1531, i64 0, i64 %1064
  %1066 = load volatile i32, ptr %1065, align 8
  %1067 = sext i32 %1066 to i64
  %1068 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1067, ptr noundef @.str.149, i32 noundef %1068)
  %1069 = load i32, ptr %9, align 4
  %1070 = icmp ne i32 %1069, 0
  br i1 %1070, label %1071, label %1074

1071:                                             ; preds = %1050
  %1072 = load i32, ptr %6, align 4
  %1073 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %1072)
  br label %1074

1074:                                             ; preds = %1071, %1050
  br label %1075

1075:                                             ; preds = %1074
  %1076 = load i32, ptr %6, align 4
  %1077 = add nsw i32 %1076, 1
  store i32 %1077, ptr %6, align 4
  br label %1047, !llvm.loop !28

1078:                                             ; preds = %1047
  %1079 = load volatile i104, ptr @g_1608, align 1
  %1080 = shl i104 %1079, 84
  %1081 = ashr i104 %1080, 84
  %1082 = trunc i104 %1081 to i32
  %1083 = sext i32 %1082 to i64
  %1084 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1083, ptr noundef @.str.150, i32 noundef %1084)
  %1085 = load volatile i104, ptr @g_1608, align 1
  %1086 = shl i104 %1085, 59
  %1087 = ashr i104 %1086, 79
  %1088 = trunc i104 %1087 to i32
  %1089 = sext i32 %1088 to i64
  %1090 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1089, ptr noundef @.str.151, i32 noundef %1090)
  %1091 = load volatile i104, ptr @g_1608, align 1
  %1092 = lshr i104 %1091, 45
  %1093 = and i104 %1092, 8388607
  %1094 = trunc i104 %1093 to i32
  %1095 = zext i32 %1094 to i64
  %1096 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1095, ptr noundef @.str.152, i32 noundef %1096)
  %1097 = load volatile i104, ptr @g_1608, align 1
  %1098 = lshr i104 %1097, 68
  %1099 = and i104 %1098, 127
  %1100 = trunc i104 %1099 to i32
  %1101 = zext i32 %1100 to i64
  %1102 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1101, ptr noundef @.str.153, i32 noundef %1102)
  %1103 = load volatile i104, ptr @g_1608, align 1
  %1104 = shl i104 %1103, 3
  %1105 = ashr i104 %1104, 78
  %1106 = trunc i104 %1105 to i32
  %1107 = sext i32 %1106 to i64
  %1108 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1107, ptr noundef @.str.154, i32 noundef %1108)
  %1109 = load volatile i32, ptr @g_1688, align 4
  %1110 = sext i32 %1109 to i64
  %1111 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1110, ptr noundef @.str.155, i32 noundef %1111)
  %1112 = load volatile i64, ptr @g_1716, align 8
  %1113 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1112, ptr noundef @.str.156, i32 noundef %1113)
  %1114 = load volatile i64, ptr @g_1717, align 8
  %1115 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1114, ptr noundef @.str.157, i32 noundef %1115)
  %1116 = load volatile i104, ptr @g_1780, align 1
  %1117 = shl i104 %1116, 84
  %1118 = ashr i104 %1117, 84
  %1119 = trunc i104 %1118 to i32
  %1120 = sext i32 %1119 to i64
  %1121 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1120, ptr noundef @.str.158, i32 noundef %1121)
  %1122 = load i104, ptr @g_1780, align 1
  %1123 = shl i104 %1122, 59
  %1124 = ashr i104 %1123, 79
  %1125 = trunc i104 %1124 to i32
  %1126 = sext i32 %1125 to i64
  %1127 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1126, ptr noundef @.str.159, i32 noundef %1127)
  %1128 = load i104, ptr @g_1780, align 1
  %1129 = lshr i104 %1128, 45
  %1130 = and i104 %1129, 8388607
  %1131 = trunc i104 %1130 to i32
  %1132 = zext i32 %1131 to i64
  %1133 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1132, ptr noundef @.str.160, i32 noundef %1133)
  %1134 = load volatile i104, ptr @g_1780, align 1
  %1135 = lshr i104 %1134, 68
  %1136 = and i104 %1135, 127
  %1137 = trunc i104 %1136 to i32
  %1138 = zext i32 %1137 to i64
  %1139 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1138, ptr noundef @.str.161, i32 noundef %1139)
  %1140 = load i104, ptr @g_1780, align 1
  %1141 = shl i104 %1140, 3
  %1142 = ashr i104 %1141, 78
  %1143 = trunc i104 %1142 to i32
  %1144 = sext i32 %1143 to i64
  %1145 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1144, ptr noundef @.str.162, i32 noundef %1145)
  store i32 0, ptr %6, align 4
  br label %1146

1146:                                             ; preds = %1225, %1078
  %1147 = load i32, ptr %6, align 4
  %1148 = icmp slt i32 %1147, 1
  br i1 %1148, label %1149, label %1228

1149:                                             ; preds = %1146
  store i32 0, ptr %7, align 4
  br label %1150

1150:                                             ; preds = %1221, %1149
  %1151 = load i32, ptr %7, align 4
  %1152 = icmp slt i32 %1151, 4
  br i1 %1152, label %1153, label %1224

1153:                                             ; preds = %1150
  %1154 = load i32, ptr %6, align 4
  %1155 = sext i32 %1154 to i64
  %1156 = getelementptr inbounds [1 x [4 x %struct.S0]], ptr @g_1797, i64 0, i64 %1155
  %1157 = load i32, ptr %7, align 4
  %1158 = sext i32 %1157 to i64
  %1159 = getelementptr inbounds [4 x %struct.S0], ptr %1156, i64 0, i64 %1158
  %1160 = load volatile i104, ptr %1159, align 1
  %1161 = shl i104 %1160, 84
  %1162 = ashr i104 %1161, 84
  %1163 = trunc i104 %1162 to i32
  %1164 = sext i32 %1163 to i64
  %1165 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1164, ptr noundef @.str.163, i32 noundef %1165)
  %1166 = load i32, ptr %6, align 4
  %1167 = sext i32 %1166 to i64
  %1168 = getelementptr inbounds [1 x [4 x %struct.S0]], ptr @g_1797, i64 0, i64 %1167
  %1169 = load i32, ptr %7, align 4
  %1170 = sext i32 %1169 to i64
  %1171 = getelementptr inbounds [4 x %struct.S0], ptr %1168, i64 0, i64 %1170
  %1172 = load i104, ptr %1171, align 1
  %1173 = shl i104 %1172, 59
  %1174 = ashr i104 %1173, 79
  %1175 = trunc i104 %1174 to i32
  %1176 = sext i32 %1175 to i64
  %1177 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1176, ptr noundef @.str.164, i32 noundef %1177)
  %1178 = load i32, ptr %6, align 4
  %1179 = sext i32 %1178 to i64
  %1180 = getelementptr inbounds [1 x [4 x %struct.S0]], ptr @g_1797, i64 0, i64 %1179
  %1181 = load i32, ptr %7, align 4
  %1182 = sext i32 %1181 to i64
  %1183 = getelementptr inbounds [4 x %struct.S0], ptr %1180, i64 0, i64 %1182
  %1184 = load i104, ptr %1183, align 1
  %1185 = lshr i104 %1184, 45
  %1186 = and i104 %1185, 8388607
  %1187 = trunc i104 %1186 to i32
  %1188 = zext i32 %1187 to i64
  %1189 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1188, ptr noundef @.str.165, i32 noundef %1189)
  %1190 = load i32, ptr %6, align 4
  %1191 = sext i32 %1190 to i64
  %1192 = getelementptr inbounds [1 x [4 x %struct.S0]], ptr @g_1797, i64 0, i64 %1191
  %1193 = load i32, ptr %7, align 4
  %1194 = sext i32 %1193 to i64
  %1195 = getelementptr inbounds [4 x %struct.S0], ptr %1192, i64 0, i64 %1194
  %1196 = load volatile i104, ptr %1195, align 1
  %1197 = lshr i104 %1196, 68
  %1198 = and i104 %1197, 127
  %1199 = trunc i104 %1198 to i32
  %1200 = zext i32 %1199 to i64
  %1201 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1200, ptr noundef @.str.166, i32 noundef %1201)
  %1202 = load i32, ptr %6, align 4
  %1203 = sext i32 %1202 to i64
  %1204 = getelementptr inbounds [1 x [4 x %struct.S0]], ptr @g_1797, i64 0, i64 %1203
  %1205 = load i32, ptr %7, align 4
  %1206 = sext i32 %1205 to i64
  %1207 = getelementptr inbounds [4 x %struct.S0], ptr %1204, i64 0, i64 %1206
  %1208 = load i104, ptr %1207, align 1
  %1209 = shl i104 %1208, 3
  %1210 = ashr i104 %1209, 78
  %1211 = trunc i104 %1210 to i32
  %1212 = sext i32 %1211 to i64
  %1213 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1212, ptr noundef @.str.167, i32 noundef %1213)
  %1214 = load i32, ptr %9, align 4
  %1215 = icmp ne i32 %1214, 0
  br i1 %1215, label %1216, label %1220

1216:                                             ; preds = %1153
  %1217 = load i32, ptr %6, align 4
  %1218 = load i32, ptr %7, align 4
  %1219 = call i32 (ptr, ...) @printf(ptr noundef @.str.118, i32 noundef %1217, i32 noundef %1218)
  br label %1220

1220:                                             ; preds = %1216, %1153
  br label %1221

1221:                                             ; preds = %1220
  %1222 = load i32, ptr %7, align 4
  %1223 = add nsw i32 %1222, 1
  store i32 %1223, ptr %7, align 4
  br label %1150, !llvm.loop !29

1224:                                             ; preds = %1150
  br label %1225

1225:                                             ; preds = %1224
  %1226 = load i32, ptr %6, align 4
  %1227 = add nsw i32 %1226, 1
  store i32 %1227, ptr %6, align 4
  br label %1146, !llvm.loop !30

1228:                                             ; preds = %1146
  %1229 = load i64, ptr @g_1990, align 8
  %1230 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1229, ptr noundef @.str.168, i32 noundef %1230)
  %1231 = load volatile i104, ptr @g_1999, align 1
  %1232 = shl i104 %1231, 84
  %1233 = ashr i104 %1232, 84
  %1234 = trunc i104 %1233 to i32
  %1235 = sext i32 %1234 to i64
  %1236 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1235, ptr noundef @.str.169, i32 noundef %1236)
  %1237 = load volatile i104, ptr @g_1999, align 1
  %1238 = shl i104 %1237, 59
  %1239 = ashr i104 %1238, 79
  %1240 = trunc i104 %1239 to i32
  %1241 = sext i32 %1240 to i64
  %1242 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1241, ptr noundef @.str.170, i32 noundef %1242)
  %1243 = load volatile i104, ptr @g_1999, align 1
  %1244 = lshr i104 %1243, 45
  %1245 = and i104 %1244, 8388607
  %1246 = trunc i104 %1245 to i32
  %1247 = zext i32 %1246 to i64
  %1248 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1247, ptr noundef @.str.171, i32 noundef %1248)
  %1249 = load volatile i104, ptr @g_1999, align 1
  %1250 = lshr i104 %1249, 68
  %1251 = and i104 %1250, 127
  %1252 = trunc i104 %1251 to i32
  %1253 = zext i32 %1252 to i64
  %1254 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1253, ptr noundef @.str.172, i32 noundef %1254)
  %1255 = load volatile i104, ptr @g_1999, align 1
  %1256 = shl i104 %1255, 3
  %1257 = ashr i104 %1256, 78
  %1258 = trunc i104 %1257 to i32
  %1259 = sext i32 %1258 to i64
  %1260 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1259, ptr noundef @.str.173, i32 noundef %1260)
  %1261 = load i32, ptr @g_2067, align 8
  %1262 = sext i32 %1261 to i64
  %1263 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1262, ptr noundef @.str.174, i32 noundef %1263)
  %1264 = load i8, ptr @g_2067, align 8
  %1265 = zext i8 %1264 to i64
  %1266 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1265, ptr noundef @.str.175, i32 noundef %1266)
  %1267 = load volatile i32, ptr @g_2067, align 8
  %1268 = sext i32 %1267 to i64
  %1269 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1268, ptr noundef @.str.176, i32 noundef %1269)
  store i32 0, ptr %6, align 4
  br label %1270

1270:                                             ; preds = %1349, %1228
  %1271 = load i32, ptr %6, align 4
  %1272 = icmp slt i32 %1271, 7
  br i1 %1272, label %1273, label %1352

1273:                                             ; preds = %1270
  store i32 0, ptr %7, align 4
  br label %1274

1274:                                             ; preds = %1345, %1273
  %1275 = load i32, ptr %7, align 4
  %1276 = icmp slt i32 %1275, 1
  br i1 %1276, label %1277, label %1348

1277:                                             ; preds = %1274
  %1278 = load i32, ptr %6, align 4
  %1279 = sext i32 %1278 to i64
  %1280 = getelementptr inbounds [7 x [1 x %struct.S0]], ptr @g_2093, i64 0, i64 %1279
  %1281 = load i32, ptr %7, align 4
  %1282 = sext i32 %1281 to i64
  %1283 = getelementptr inbounds [1 x %struct.S0], ptr %1280, i64 0, i64 %1282
  %1284 = load volatile i104, ptr %1283, align 1
  %1285 = shl i104 %1284, 84
  %1286 = ashr i104 %1285, 84
  %1287 = trunc i104 %1286 to i32
  %1288 = sext i32 %1287 to i64
  %1289 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1288, ptr noundef @.str.177, i32 noundef %1289)
  %1290 = load i32, ptr %6, align 4
  %1291 = sext i32 %1290 to i64
  %1292 = getelementptr inbounds [7 x [1 x %struct.S0]], ptr @g_2093, i64 0, i64 %1291
  %1293 = load i32, ptr %7, align 4
  %1294 = sext i32 %1293 to i64
  %1295 = getelementptr inbounds [1 x %struct.S0], ptr %1292, i64 0, i64 %1294
  %1296 = load volatile i104, ptr %1295, align 1
  %1297 = shl i104 %1296, 59
  %1298 = ashr i104 %1297, 79
  %1299 = trunc i104 %1298 to i32
  %1300 = sext i32 %1299 to i64
  %1301 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1300, ptr noundef @.str.178, i32 noundef %1301)
  %1302 = load i32, ptr %6, align 4
  %1303 = sext i32 %1302 to i64
  %1304 = getelementptr inbounds [7 x [1 x %struct.S0]], ptr @g_2093, i64 0, i64 %1303
  %1305 = load i32, ptr %7, align 4
  %1306 = sext i32 %1305 to i64
  %1307 = getelementptr inbounds [1 x %struct.S0], ptr %1304, i64 0, i64 %1306
  %1308 = load volatile i104, ptr %1307, align 1
  %1309 = lshr i104 %1308, 45
  %1310 = and i104 %1309, 8388607
  %1311 = trunc i104 %1310 to i32
  %1312 = zext i32 %1311 to i64
  %1313 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1312, ptr noundef @.str.179, i32 noundef %1313)
  %1314 = load i32, ptr %6, align 4
  %1315 = sext i32 %1314 to i64
  %1316 = getelementptr inbounds [7 x [1 x %struct.S0]], ptr @g_2093, i64 0, i64 %1315
  %1317 = load i32, ptr %7, align 4
  %1318 = sext i32 %1317 to i64
  %1319 = getelementptr inbounds [1 x %struct.S0], ptr %1316, i64 0, i64 %1318
  %1320 = load volatile i104, ptr %1319, align 1
  %1321 = lshr i104 %1320, 68
  %1322 = and i104 %1321, 127
  %1323 = trunc i104 %1322 to i32
  %1324 = zext i32 %1323 to i64
  %1325 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1324, ptr noundef @.str.180, i32 noundef %1325)
  %1326 = load i32, ptr %6, align 4
  %1327 = sext i32 %1326 to i64
  %1328 = getelementptr inbounds [7 x [1 x %struct.S0]], ptr @g_2093, i64 0, i64 %1327
  %1329 = load i32, ptr %7, align 4
  %1330 = sext i32 %1329 to i64
  %1331 = getelementptr inbounds [1 x %struct.S0], ptr %1328, i64 0, i64 %1330
  %1332 = load volatile i104, ptr %1331, align 1
  %1333 = shl i104 %1332, 3
  %1334 = ashr i104 %1333, 78
  %1335 = trunc i104 %1334 to i32
  %1336 = sext i32 %1335 to i64
  %1337 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1336, ptr noundef @.str.181, i32 noundef %1337)
  %1338 = load i32, ptr %9, align 4
  %1339 = icmp ne i32 %1338, 0
  br i1 %1339, label %1340, label %1344

1340:                                             ; preds = %1277
  %1341 = load i32, ptr %6, align 4
  %1342 = load i32, ptr %7, align 4
  %1343 = call i32 (ptr, ...) @printf(ptr noundef @.str.118, i32 noundef %1341, i32 noundef %1342)
  br label %1344

1344:                                             ; preds = %1340, %1277
  br label %1345

1345:                                             ; preds = %1344
  %1346 = load i32, ptr %7, align 4
  %1347 = add nsw i32 %1346, 1
  store i32 %1347, ptr %7, align 4
  br label %1274, !llvm.loop !31

1348:                                             ; preds = %1274
  br label %1349

1349:                                             ; preds = %1348
  %1350 = load i32, ptr %6, align 4
  %1351 = add nsw i32 %1350, 1
  store i32 %1351, ptr %6, align 4
  br label %1270, !llvm.loop !32

1352:                                             ; preds = %1270
  %1353 = load volatile i104, ptr @g_2094, align 1
  %1354 = shl i104 %1353, 84
  %1355 = ashr i104 %1354, 84
  %1356 = trunc i104 %1355 to i32
  %1357 = sext i32 %1356 to i64
  %1358 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1357, ptr noundef @.str.182, i32 noundef %1358)
  %1359 = load volatile i104, ptr @g_2094, align 1
  %1360 = shl i104 %1359, 59
  %1361 = ashr i104 %1360, 79
  %1362 = trunc i104 %1361 to i32
  %1363 = sext i32 %1362 to i64
  %1364 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1363, ptr noundef @.str.183, i32 noundef %1364)
  %1365 = load volatile i104, ptr @g_2094, align 1
  %1366 = lshr i104 %1365, 45
  %1367 = and i104 %1366, 8388607
  %1368 = trunc i104 %1367 to i32
  %1369 = zext i32 %1368 to i64
  %1370 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1369, ptr noundef @.str.184, i32 noundef %1370)
  %1371 = load volatile i104, ptr @g_2094, align 1
  %1372 = lshr i104 %1371, 68
  %1373 = and i104 %1372, 127
  %1374 = trunc i104 %1373 to i32
  %1375 = zext i32 %1374 to i64
  %1376 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1375, ptr noundef @.str.185, i32 noundef %1376)
  %1377 = load volatile i104, ptr @g_2094, align 1
  %1378 = shl i104 %1377, 3
  %1379 = ashr i104 %1378, 78
  %1380 = trunc i104 %1379 to i32
  %1381 = sext i32 %1380 to i64
  %1382 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1381, ptr noundef @.str.186, i32 noundef %1382)
  store i32 0, ptr %6, align 4
  br label %1383

1383:                                             ; preds = %1438, %1352
  %1384 = load i32, ptr %6, align 4
  %1385 = icmp slt i32 %1384, 2
  br i1 %1385, label %1386, label %1441

1386:                                             ; preds = %1383
  %1387 = load i32, ptr %6, align 4
  %1388 = sext i32 %1387 to i64
  %1389 = getelementptr inbounds [2 x %struct.S0], ptr @g_2109, i64 0, i64 %1388
  %1390 = load volatile i104, ptr %1389, align 1
  %1391 = shl i104 %1390, 84
  %1392 = ashr i104 %1391, 84
  %1393 = trunc i104 %1392 to i32
  %1394 = sext i32 %1393 to i64
  %1395 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1394, ptr noundef @.str.187, i32 noundef %1395)
  %1396 = load i32, ptr %6, align 4
  %1397 = sext i32 %1396 to i64
  %1398 = getelementptr inbounds [2 x %struct.S0], ptr @g_2109, i64 0, i64 %1397
  %1399 = load i104, ptr %1398, align 1
  %1400 = shl i104 %1399, 59
  %1401 = ashr i104 %1400, 79
  %1402 = trunc i104 %1401 to i32
  %1403 = sext i32 %1402 to i64
  %1404 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1403, ptr noundef @.str.188, i32 noundef %1404)
  %1405 = load i32, ptr %6, align 4
  %1406 = sext i32 %1405 to i64
  %1407 = getelementptr inbounds [2 x %struct.S0], ptr @g_2109, i64 0, i64 %1406
  %1408 = load i104, ptr %1407, align 1
  %1409 = lshr i104 %1408, 45
  %1410 = and i104 %1409, 8388607
  %1411 = trunc i104 %1410 to i32
  %1412 = zext i32 %1411 to i64
  %1413 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1412, ptr noundef @.str.189, i32 noundef %1413)
  %1414 = load i32, ptr %6, align 4
  %1415 = sext i32 %1414 to i64
  %1416 = getelementptr inbounds [2 x %struct.S0], ptr @g_2109, i64 0, i64 %1415
  %1417 = load volatile i104, ptr %1416, align 1
  %1418 = lshr i104 %1417, 68
  %1419 = and i104 %1418, 127
  %1420 = trunc i104 %1419 to i32
  %1421 = zext i32 %1420 to i64
  %1422 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1421, ptr noundef @.str.190, i32 noundef %1422)
  %1423 = load i32, ptr %6, align 4
  %1424 = sext i32 %1423 to i64
  %1425 = getelementptr inbounds [2 x %struct.S0], ptr @g_2109, i64 0, i64 %1424
  %1426 = load i104, ptr %1425, align 1
  %1427 = shl i104 %1426, 3
  %1428 = ashr i104 %1427, 78
  %1429 = trunc i104 %1428 to i32
  %1430 = sext i32 %1429 to i64
  %1431 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1430, ptr noundef @.str.191, i32 noundef %1431)
  %1432 = load i32, ptr %9, align 4
  %1433 = icmp ne i32 %1432, 0
  br i1 %1433, label %1434, label %1437

1434:                                             ; preds = %1386
  %1435 = load i32, ptr %6, align 4
  %1436 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %1435)
  br label %1437

1437:                                             ; preds = %1434, %1386
  br label %1438

1438:                                             ; preds = %1437
  %1439 = load i32, ptr %6, align 4
  %1440 = add nsw i32 %1439, 1
  store i32 %1440, ptr %6, align 4
  br label %1383, !llvm.loop !33

1441:                                             ; preds = %1383
  store i32 0, ptr %6, align 4
  br label %1442

1442:                                             ; preds = %1470, %1441
  %1443 = load i32, ptr %6, align 4
  %1444 = icmp slt i32 %1443, 7
  br i1 %1444, label %1445, label %1473

1445:                                             ; preds = %1442
  store i32 0, ptr %7, align 4
  br label %1446

1446:                                             ; preds = %1466, %1445
  %1447 = load i32, ptr %7, align 4
  %1448 = icmp slt i32 %1447, 6
  br i1 %1448, label %1449, label %1469

1449:                                             ; preds = %1446
  %1450 = load i32, ptr %6, align 4
  %1451 = sext i32 %1450 to i64
  %1452 = getelementptr inbounds [7 x [6 x i32]], ptr @g_2130, i64 0, i64 %1451
  %1453 = load i32, ptr %7, align 4
  %1454 = sext i32 %1453 to i64
  %1455 = getelementptr inbounds [6 x i32], ptr %1452, i64 0, i64 %1454
  %1456 = load volatile i32, ptr %1455, align 4
  %1457 = sext i32 %1456 to i64
  %1458 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1457, ptr noundef @.str.192, i32 noundef %1458)
  %1459 = load i32, ptr %9, align 4
  %1460 = icmp ne i32 %1459, 0
  br i1 %1460, label %1461, label %1465

1461:                                             ; preds = %1449
  %1462 = load i32, ptr %6, align 4
  %1463 = load i32, ptr %7, align 4
  %1464 = call i32 (ptr, ...) @printf(ptr noundef @.str.118, i32 noundef %1462, i32 noundef %1463)
  br label %1465

1465:                                             ; preds = %1461, %1449
  br label %1466

1466:                                             ; preds = %1465
  %1467 = load i32, ptr %7, align 4
  %1468 = add nsw i32 %1467, 1
  store i32 %1468, ptr %7, align 4
  br label %1446, !llvm.loop !34

1469:                                             ; preds = %1446
  br label %1470

1470:                                             ; preds = %1469
  %1471 = load i32, ptr %6, align 4
  %1472 = add nsw i32 %1471, 1
  store i32 %1472, ptr %6, align 4
  br label %1442, !llvm.loop !35

1473:                                             ; preds = %1442
  %1474 = load volatile i104, ptr @g_2157, align 1
  %1475 = shl i104 %1474, 84
  %1476 = ashr i104 %1475, 84
  %1477 = trunc i104 %1476 to i32
  %1478 = sext i32 %1477 to i64
  %1479 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1478, ptr noundef @.str.193, i32 noundef %1479)
  %1480 = load volatile i104, ptr @g_2157, align 1
  %1481 = shl i104 %1480, 59
  %1482 = ashr i104 %1481, 79
  %1483 = trunc i104 %1482 to i32
  %1484 = sext i32 %1483 to i64
  %1485 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1484, ptr noundef @.str.194, i32 noundef %1485)
  %1486 = load volatile i104, ptr @g_2157, align 1
  %1487 = lshr i104 %1486, 45
  %1488 = and i104 %1487, 8388607
  %1489 = trunc i104 %1488 to i32
  %1490 = zext i32 %1489 to i64
  %1491 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1490, ptr noundef @.str.195, i32 noundef %1491)
  %1492 = load volatile i104, ptr @g_2157, align 1
  %1493 = lshr i104 %1492, 68
  %1494 = and i104 %1493, 127
  %1495 = trunc i104 %1494 to i32
  %1496 = zext i32 %1495 to i64
  %1497 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1496, ptr noundef @.str.196, i32 noundef %1497)
  %1498 = load volatile i104, ptr @g_2157, align 1
  %1499 = shl i104 %1498, 3
  %1500 = ashr i104 %1499, 78
  %1501 = trunc i104 %1500 to i32
  %1502 = sext i32 %1501 to i64
  %1503 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1502, ptr noundef @.str.197, i32 noundef %1503)
  %1504 = load volatile i32, ptr @g_2273, align 8
  %1505 = sext i32 %1504 to i64
  %1506 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1505, ptr noundef @.str.198, i32 noundef %1506)
  %1507 = load volatile i8, ptr @g_2273, align 8
  %1508 = zext i8 %1507 to i64
  %1509 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1508, ptr noundef @.str.199, i32 noundef %1509)
  %1510 = load volatile i32, ptr @g_2273, align 8
  %1511 = sext i32 %1510 to i64
  %1512 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1511, ptr noundef @.str.200, i32 noundef %1512)
  %1513 = load volatile i104, ptr @g_2297, align 1
  %1514 = shl i104 %1513, 84
  %1515 = ashr i104 %1514, 84
  %1516 = trunc i104 %1515 to i32
  %1517 = sext i32 %1516 to i64
  %1518 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1517, ptr noundef @.str.201, i32 noundef %1518)
  %1519 = load volatile i104, ptr @g_2297, align 1
  %1520 = shl i104 %1519, 59
  %1521 = ashr i104 %1520, 79
  %1522 = trunc i104 %1521 to i32
  %1523 = sext i32 %1522 to i64
  %1524 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1523, ptr noundef @.str.202, i32 noundef %1524)
  %1525 = load volatile i104, ptr @g_2297, align 1
  %1526 = lshr i104 %1525, 45
  %1527 = and i104 %1526, 8388607
  %1528 = trunc i104 %1527 to i32
  %1529 = zext i32 %1528 to i64
  %1530 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1529, ptr noundef @.str.203, i32 noundef %1530)
  %1531 = load volatile i104, ptr @g_2297, align 1
  %1532 = lshr i104 %1531, 68
  %1533 = and i104 %1532, 127
  %1534 = trunc i104 %1533 to i32
  %1535 = zext i32 %1534 to i64
  %1536 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1535, ptr noundef @.str.204, i32 noundef %1536)
  %1537 = load volatile i104, ptr @g_2297, align 1
  %1538 = shl i104 %1537, 3
  %1539 = ashr i104 %1538, 78
  %1540 = trunc i104 %1539 to i32
  %1541 = sext i32 %1540 to i64
  %1542 = load i32, ptr %9, align 4
  call void @transparent_crc(i64 noundef %1541, ptr noundef @.str.205, i32 noundef %1542)
  %1543 = load i32, ptr @crc32_context, align 4
  %1544 = zext i32 %1543 to i64
  %1545 = xor i64 %1544, 4294967295
  %1546 = trunc i64 %1545 to i32
  %1547 = load i32, ptr %9, align 4
  call void @platform_main_end(i32 noundef %1546, i32 noundef %1547)
  ret i32 0
}

; Function Attrs: nounwind willreturn memory(read)
declare i32 @strcmp(ptr noundef, ptr noundef) #1

; Function Attrs: noinline nounwind optnone uwtable
define internal void @platform_main_begin() #0 {
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define internal void @crc32_gentab() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 -306674912, ptr %2, align 4
  store i32 0, ptr %3, align 4
  br label %5

5:                                                ; preds = %33, %0
  %6 = load i32, ptr %3, align 4
  %7 = icmp slt i32 %6, 256
  br i1 %7, label %8, label %36

8:                                                ; preds = %5
  %9 = load i32, ptr %3, align 4
  store i32 %9, ptr %1, align 4
  store i32 8, ptr %4, align 4
  br label %10

10:                                               ; preds = %25, %8
  %11 = load i32, ptr %4, align 4
  %12 = icmp sgt i32 %11, 0
  br i1 %12, label %13, label %28

13:                                               ; preds = %10
  %14 = load i32, ptr %1, align 4
  %15 = and i32 %14, 1
  %16 = icmp ne i32 %15, 0
  br i1 %16, label %17, label %21

17:                                               ; preds = %13
  %18 = load i32, ptr %1, align 4
  %19 = lshr i32 %18, 1
  %20 = xor i32 %19, -306674912
  store i32 %20, ptr %1, align 4
  br label %24

21:                                               ; preds = %13
  %22 = load i32, ptr %1, align 4
  %23 = lshr i32 %22, 1
  store i32 %23, ptr %1, align 4
  br label %24

24:                                               ; preds = %21, %17
  br label %25

25:                                               ; preds = %24
  %26 = load i32, ptr %4, align 4
  %27 = add nsw i32 %26, -1
  store i32 %27, ptr %4, align 4
  br label %10, !llvm.loop !36

28:                                               ; preds = %10
  %29 = load i32, ptr %1, align 4
  %30 = load i32, ptr %3, align 4
  %31 = sext i32 %30 to i64
  %32 = getelementptr inbounds [256 x i32], ptr @crc32_tab, i64 0, i64 %31
  store i32 %29, ptr %32, align 4
  br label %33

33:                                               ; preds = %28
  %34 = load i32, ptr %3, align 4
  %35 = add nsw i32 %34, 1
  store i32 %35, ptr %3, align 4
  br label %5, !llvm.loop !37

36:                                               ; preds = %5
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define internal { i64, i40 } @func_1() #0 {
  %1 = alloca %struct.S0, align 1
  %2 = alloca [7 x i8], align 1
  %3 = alloca i32, align 4
  %4 = alloca ptr, align 8
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  %7 = alloca [2 x [10 x ptr]], align 16
  %8 = alloca i16, align 2
  %9 = alloca i16, align 2
  %10 = alloca i32, align 4
  %11 = alloca i16, align 2
  %12 = alloca ptr, align 8
  %13 = alloca ptr, align 8
  %14 = alloca i8, align 1
  %15 = alloca i8, align 1
  %16 = alloca ptr, align 8
  %17 = alloca i32, align 4
  %18 = alloca i32, align 4
  %19 = alloca i32, align 4
  %20 = alloca %struct.S0, align 1
  %21 = alloca { i64, i40 }, align 8
  %22 = alloca %union.U1, align 8
  %23 = alloca i16, align 2
  %24 = alloca [8 x [7 x i32]], align 16
  %25 = alloca ptr, align 8
  %26 = alloca ptr, align 8
  %27 = alloca ptr, align 8
  %28 = alloca ptr, align 8
  %29 = alloca [8 x ptr], align 16
  %30 = alloca i8, align 1
  %31 = alloca i32, align 4
  %32 = alloca i32, align 4
  %33 = alloca i32, align 4
  %34 = alloca %struct.S0, align 1
  %35 = alloca { i64, i40 }, align 8
  store i32 -290979459, ptr %3, align 4
  store ptr null, ptr %4, align 8
  store ptr getelementptr inbounds ([3 x i32], ptr @g_147, i64 0, i64 2), ptr %5, align 8
  store ptr getelementptr inbounds ([3 x i32], ptr @g_147, i64 0, i64 1), ptr %6, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %7, ptr align 8 @constinit, i64 80, i1 false)
  %36 = getelementptr inbounds [10 x ptr], ptr %7, i64 1
  store ptr @g_1026, ptr %36, align 8
  %37 = getelementptr inbounds ptr, ptr %36, i64 1
  store ptr @g_1026, ptr %37, align 8
  %38 = getelementptr inbounds ptr, ptr %36, i64 2
  store ptr @g_1026, ptr %38, align 8
  %39 = getelementptr inbounds ptr, ptr %36, i64 3
  store ptr @g_1026, ptr %39, align 8
  %40 = getelementptr inbounds ptr, ptr %36, i64 4
  store ptr %3, ptr %40, align 8
  %41 = getelementptr inbounds ptr, ptr %36, i64 5
  store ptr @g_1026, ptr %41, align 8
  %42 = getelementptr inbounds ptr, ptr %36, i64 6
  store ptr @g_1026, ptr %42, align 8
  %43 = getelementptr inbounds ptr, ptr %36, i64 7
  store ptr @g_1026, ptr %43, align 8
  %44 = getelementptr inbounds ptr, ptr %36, i64 8
  store ptr %3, ptr %44, align 8
  %45 = getelementptr inbounds ptr, ptr %36, i64 9
  store ptr @g_1026, ptr %45, align 8
  store i16 7266, ptr %8, align 2
  store i16 1, ptr %9, align 2
  store i32 -2000986591, ptr %10, align 4
  store i16 7, ptr %11, align 2
  store ptr @g_355, ptr %12, align 8
  store ptr @g_1990, ptr %13, align 8
  store i8 65, ptr %14, align 1
  store i8 -1, ptr %15, align 1
  store ptr @g_1584, ptr %16, align 8
  store i32 1158800061, ptr %17, align 4
  store i32 0, ptr %18, align 4
  br label %46

46:                                               ; preds = %53, %0
  %47 = load i32, ptr %18, align 4
  %48 = icmp slt i32 %47, 7
  br i1 %48, label %49, label %56

49:                                               ; preds = %46
  %50 = load i32, ptr %18, align 4
  %51 = sext i32 %50 to i64
  %52 = getelementptr inbounds [7 x i8], ptr %2, i64 0, i64 %51
  store i8 102, ptr %52, align 1
  br label %53

53:                                               ; preds = %49
  %54 = load i32, ptr %18, align 4
  %55 = add nsw i32 %54, 1
  store i32 %55, ptr %18, align 4
  br label %46, !llvm.loop !38

56:                                               ; preds = %46
  %57 = load volatile ptr, ptr @g_2257, align 8
  %58 = getelementptr inbounds [7 x i8], ptr %2, i64 0, i64 3
  %59 = load i8, ptr %58, align 1
  %60 = getelementptr inbounds [7 x i8], ptr %2, i64 0, i64 3
  %61 = getelementptr inbounds [7 x i8], ptr %2, i64 0, i64 3
  %62 = call i32 @func_5(i32 noundef 1, i8 noundef signext %59, ptr noundef %60, ptr noundef %61)
  %63 = load i8, ptr getelementptr inbounds ([5 x %union.U1], ptr @g_1531, i64 0, i64 3), align 8
  %64 = call { i64, i40 } @func_2(i32 noundef %62, i8 noundef signext %63)
  %65 = getelementptr inbounds nuw %struct.S0, ptr %20, i32 0, i32 0
  store { i64, i40 } %64, ptr %21, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %65, ptr align 8 %21, i64 13, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %57, ptr align 1 %20, i64 13, i1 true)
  %66 = load i16, ptr %9, align 2
  %67 = add i16 %66, 1
  store i16 %67, ptr %9, align 2
  %68 = load ptr, ptr @g_2147, align 8
  %69 = load i32, ptr %68, align 4
  %70 = load ptr, ptr %6, align 8
  %71 = load i32, ptr %70, align 4
  %72 = sext i32 %71 to i64
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %22, ptr align 8 @g_2273, i64 8, i1 true)
  %73 = load ptr, ptr %5, align 8
  %74 = load i32, ptr %73, align 4
  %75 = icmp ne i32 %74, 0
  %76 = xor i1 %75, true
  %77 = zext i1 %76 to i32
  %78 = call i32 @safe_mod_func_uint32_t_u_u(i32 noundef %77, i32 noundef 1)
  %79 = load i8, ptr %14, align 1
  %80 = sext i8 %79 to i32
  %81 = icmp eq i32 %78, %80
  %82 = zext i1 %81 to i32
  %83 = sext i32 %82 to i64
  %84 = icmp ule i64 %83, 2
  %85 = zext i1 %84 to i32
  %86 = sext i32 %85 to i64
  %87 = icmp slt i64 %86, 1070075341
  %88 = zext i1 %87 to i32
  %89 = sext i32 %88 to i64
  %90 = xor i64 %89, -1
  %91 = icmp ule i64 %72, %90
  %92 = zext i1 %91 to i32
  %93 = trunc i32 %92 to i8
  %94 = load i8, ptr %15, align 1
  %95 = sext i8 %94 to i32
  %96 = call signext i8 @safe_rshift_func_int8_t_s_s(i8 noundef signext %93, i32 noundef %95)
  %97 = sext i8 %96 to i32
  %98 = icmp ule i32 %69, %97
  %99 = zext i1 %98 to i32
  %100 = load ptr, ptr @g_1015, align 8
  %101 = load i32, ptr %100, align 4
  %102 = and i32 %99, %101
  %103 = trunc i32 %102 to i8
  %104 = call zeroext i8 @safe_lshift_func_uint8_t_u_s(i8 noundef zeroext %103, i32 noundef 7)
  %105 = load ptr, ptr @g_2113, align 8
  %106 = load ptr, ptr %105, align 8
  %107 = load ptr, ptr %106, align 8
  %108 = load i8, ptr %107, align 1
  %109 = call zeroext i8 @safe_div_func_uint8_t_u_u(i8 noundef zeroext %104, i8 noundef zeroext %108)
  %110 = load ptr, ptr %6, align 8
  %111 = load i32, ptr %110, align 4
  %112 = icmp ne i32 %111, 0
  br i1 %112, label %113, label %131

113:                                              ; preds = %56
  store i16 -1, ptr %23, align 2
  store i8 0, ptr @g_1245, align 1
  br label %114

114:                                              ; preds = %122, %113
  %115 = load i8, ptr @g_1245, align 1
  %116 = zext i8 %115 to i32
  %117 = icmp slt i32 %116, 5
  br i1 %117, label %118, label %127

118:                                              ; preds = %114
  %119 = load i8, ptr @g_1245, align 1
  %120 = zext i8 %119 to i64
  %121 = getelementptr inbounds nuw [5 x i8], ptr @g_303, i64 0, i64 %120
  store i8 -5, ptr %121, align 1
  br label %122

122:                                              ; preds = %118
  %123 = load i8, ptr @g_1245, align 1
  %124 = zext i8 %123 to i32
  %125 = add nsw i32 %124, 1
  %126 = trunc i32 %125 to i8
  store i8 %126, ptr @g_1245, align 1
  br label %114, !llvm.loop !39

127:                                              ; preds = %114
  %128 = load ptr, ptr %6, align 8
  %129 = load i32, ptr %128, align 4
  %130 = trunc i32 %129 to i16
  store i16 %130, ptr %23, align 2
  br label %234

131:                                              ; preds = %56
  store ptr @g_1584, ptr %25, align 8
  store ptr %25, ptr %26, align 8
  store ptr @g_1584, ptr %27, align 8
  store ptr %27, ptr %28, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %29, ptr align 16 @__const.func_1.l_2310, i64 64, i1 false)
  store i8 -3, ptr %30, align 1
  store i32 1218370560, ptr %31, align 4
  store i32 0, ptr %32, align 4
  br label %132

132:                                              ; preds = %150, %131
  %133 = load i32, ptr %32, align 4
  %134 = icmp slt i32 %133, 8
  br i1 %134, label %135, label %153

135:                                              ; preds = %132
  store i32 0, ptr %33, align 4
  br label %136

136:                                              ; preds = %146, %135
  %137 = load i32, ptr %33, align 4
  %138 = icmp slt i32 %137, 7
  br i1 %138, label %139, label %149

139:                                              ; preds = %136
  %140 = load i32, ptr %32, align 4
  %141 = sext i32 %140 to i64
  %142 = getelementptr inbounds [8 x [7 x i32]], ptr %24, i64 0, i64 %141
  %143 = load i32, ptr %33, align 4
  %144 = sext i32 %143 to i64
  %145 = getelementptr inbounds [7 x i32], ptr %142, i64 0, i64 %144
  store i32 -1, ptr %145, align 4
  br label %146

146:                                              ; preds = %139
  %147 = load i32, ptr %33, align 4
  %148 = add nsw i32 %147, 1
  store i32 %148, ptr %33, align 4
  br label %136, !llvm.loop !40

149:                                              ; preds = %136
  br label %150

150:                                              ; preds = %149
  %151 = load i32, ptr %32, align 4
  %152 = add nsw i32 %151, 1
  store i32 %152, ptr %32, align 4
  br label %132, !llvm.loop !41

153:                                              ; preds = %132
  %154 = load i8, ptr @g_516, align 8
  %155 = zext i8 %154 to i32
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %34, ptr align 1 @g_2297, i64 13, i1 true)
  %156 = getelementptr inbounds [8 x [7 x i32]], ptr %24, i64 0, i64 4
  %157 = getelementptr inbounds [7 x i32], ptr %156, i64 0, i64 4
  %158 = load i32, ptr %157, align 16
  %159 = trunc i32 %158 to i16
  %160 = call signext i16 @safe_sub_func_int16_t_s_s(i16 noundef signext -1, i16 noundef signext %159)
  %161 = sext i16 %160 to i32
  %162 = load ptr, ptr %6, align 8
  %163 = load i32, ptr %162, align 4
  %164 = sext i32 %163 to i64
  %165 = call i64 @safe_div_func_int64_t_s_s(i64 noundef -5827989622995721308, i64 noundef %164)
  %166 = icmp ne i64 %165, 0
  br i1 %166, label %167, label %186

167:                                              ; preds = %153
  %168 = load ptr, ptr %16, align 8
  %169 = load ptr, ptr %16, align 8
  %170 = load ptr, ptr %26, align 8
  store ptr %169, ptr %170, align 8
  %171 = load ptr, ptr %28, align 8
  store ptr %169, ptr %171, align 8
  %172 = icmp eq ptr %168, %169
  %173 = zext i1 %172 to i32
  %174 = trunc i32 %173 to i8
  %175 = call signext i8 @safe_lshift_func_int8_t_s_u(i8 noundef signext %174, i32 noundef 3)
  %176 = getelementptr inbounds [8 x ptr], ptr %29, i64 0, i64 6
  %177 = load ptr, ptr %176, align 16
  %178 = getelementptr inbounds [8 x ptr], ptr %29, i64 0, i64 6
  %179 = load ptr, ptr %178, align 16
  %180 = icmp eq ptr %177, %179
  %181 = zext i1 %180 to i32
  %182 = trunc i32 %181 to i16
  %183 = call zeroext i16 @safe_rshift_func_uint16_t_u_s(i16 noundef zeroext %182, i32 noundef 8)
  %184 = zext i16 %183 to i32
  %185 = icmp ne i32 %184, 0
  br label %186

186:                                              ; preds = %167, %153
  %187 = phi i1 [ false, %153 ], [ %185, %167 ]
  %188 = zext i1 %187 to i32
  %189 = getelementptr inbounds [8 x [7 x i32]], ptr %24, i64 0, i64 4
  %190 = getelementptr inbounds [7 x i32], ptr %189, i64 0, i64 4
  %191 = load i32, ptr %190, align 16
  %192 = or i32 %161, %191
  %193 = load ptr, ptr @g_2147, align 8
  %194 = load i32, ptr %193, align 4
  %195 = icmp ne i32 %192, %194
  %196 = zext i1 %195 to i32
  %197 = getelementptr inbounds [8 x [7 x i32]], ptr %24, i64 0, i64 4
  %198 = getelementptr inbounds [7 x i32], ptr %197, i64 0, i64 4
  %199 = load i32, ptr %198, align 16
  %200 = or i32 %196, %199
  %201 = load ptr, ptr %5, align 8
  %202 = load i32, ptr %201, align 4
  %203 = or i32 %200, %202
  %204 = zext i32 %203 to i64
  %205 = icmp sge i64 %204, 1
  %206 = zext i1 %205 to i32
  %207 = getelementptr inbounds [8 x [7 x i32]], ptr %24, i64 0, i64 4
  %208 = getelementptr inbounds [7 x i32], ptr %207, i64 0, i64 4
  %209 = load i32, ptr %208, align 16
  %210 = and i32 %206, %209
  %211 = zext i32 %210 to i64
  %212 = call i64 @safe_unary_minus_func_uint64_t_u(i64 noundef %211)
  %213 = trunc i64 %212 to i8
  %214 = load ptr, ptr @g_837, align 8
  %215 = load ptr, ptr %214, align 8
  %216 = load i8, ptr %215, align 1
  %217 = zext i8 %216 to i32
  %218 = call signext i8 @safe_lshift_func_int8_t_s_u(i8 noundef signext %213, i32 noundef %217)
  %219 = sext i8 %218 to i32
  %220 = call i32 @safe_mod_func_uint32_t_u_u(i32 noundef %219, i32 noundef 1158800061)
  %221 = load ptr, ptr %6, align 8
  %222 = load i32, ptr %221, align 4
  %223 = icmp sge i32 0, %222
  %224 = zext i1 %223 to i32
  %225 = load i8, ptr %30, align 1
  %226 = sext i8 %225 to i32
  %227 = icmp sle i32 %224, %226
  %228 = zext i1 %227 to i32
  %229 = xor i32 %155, %228
  %230 = load i32, ptr %31, align 4
  %231 = xor i32 %230, %229
  store i32 %231, ptr %31, align 4
  %232 = load ptr, ptr @g_1013, align 8
  %233 = load ptr, ptr %232, align 8
  store ptr null, ptr %233, align 8
  br label %234

234:                                              ; preds = %186, %127
  %235 = load volatile ptr, ptr @g_910, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %1, ptr align 1 %235, i64 13, i1 false)
  %236 = getelementptr inbounds nuw %struct.S0, ptr %1, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %35, ptr align 1 %236, i64 13, i1 false)
  %237 = load { i64, i40 }, ptr %35, align 8
  ret { i64, i40 } %237
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i64, i1 immarg) #2

; Function Attrs: noinline nounwind optnone uwtable
define internal void @transparent_crc(i64 noundef %0, ptr noundef %1, i32 noundef %2) #0 {
  %4 = alloca i64, align 8
  %5 = alloca ptr, align 8
  %6 = alloca i32, align 4
  store i64 %0, ptr %4, align 8
  store ptr %1, ptr %5, align 8
  store i32 %2, ptr %6, align 4
  %7 = load i64, ptr %4, align 8
  call void @crc32_8bytes(i64 noundef %7)
  %8 = load i32, ptr %6, align 4
  %9 = icmp ne i32 %8, 0
  br i1 %9, label %10, label %16

10:                                               ; preds = %3
  %11 = load ptr, ptr %5, align 8
  %12 = load i32, ptr @crc32_context, align 4
  %13 = zext i32 %12 to i64
  %14 = xor i64 %13, 4294967295
  %15 = call i32 (ptr, ...) @printf(ptr noundef @.str.206, ptr noundef %11, i64 noundef %14)
  br label %16

16:                                               ; preds = %10, %3
  ret void
}

declare i32 @printf(ptr noundef, ...) #3

; Function Attrs: noinline nounwind optnone uwtable
define internal void @platform_main_end(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %3, align 4
  %6 = call i32 (ptr, ...) @printf(ptr noundef @.str.240, i32 noundef %5)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define internal { i64, i40 } @func_2(i32 noundef %0, i8 noundef signext %1) #0 {
  %3 = alloca %struct.S0, align 1
  %4 = alloca i32, align 4
  %5 = alloca i8, align 1
  %6 = alloca [9 x ptr], align 16
  %7 = alloca ptr, align 8
  %8 = alloca i32, align 4
  %9 = alloca ptr, align 8
  %10 = alloca [6 x [2 x [8 x i32]]], align 16
  %11 = alloca i8, align 1
  %12 = alloca i32, align 4
  %13 = alloca ptr, align 8
  %14 = alloca ptr, align 8
  %15 = alloca [5 x [3 x ptr]], align 16
  %16 = alloca ptr, align 8
  %17 = alloca ptr, align 8
  %18 = alloca [10 x ptr], align 16
  %19 = alloca i32, align 4
  %20 = alloca i32, align 4
  %21 = alloca ptr, align 8
  %22 = alloca i32, align 4
  %23 = alloca i32, align 4
  %24 = alloca i32, align 4
  %25 = alloca { i64, i40 }, align 8
  store i32 %0, ptr %4, align 4
  store i8 %1, ptr %5, align 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %6, ptr align 16 @__const.func_2.l_2208, i64 72, i1 false)
  %26 = getelementptr inbounds [9 x ptr], ptr %6, i64 0, i64 3
  store ptr %26, ptr %7, align 8
  store i32 -961796145, ptr %8, align 4
  store ptr @g_235, ptr %9, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %10, ptr align 16 @__const.func_2.l_2224, i64 384, i1 false)
  store i8 -78, ptr %11, align 1
  store i32 -54843240, ptr %12, align 4
  store ptr %12, ptr %13, align 8
  store ptr @g_2065, ptr %14, align 8
  %27 = getelementptr inbounds [5 x [3 x ptr]], ptr %15, i64 0, i64 0
  %28 = getelementptr inbounds [3 x ptr], ptr %27, i64 0, i64 2
  store ptr %28, ptr %16, align 8
  store ptr null, ptr %17, align 8
  store ptr %17, ptr %18, align 8
  %29 = getelementptr inbounds ptr, ptr %18, i64 1
  store ptr %17, ptr %29, align 8
  %30 = getelementptr inbounds ptr, ptr %18, i64 2
  store ptr %17, ptr %30, align 8
  %31 = getelementptr inbounds ptr, ptr %18, i64 3
  store ptr %17, ptr %31, align 8
  %32 = getelementptr inbounds ptr, ptr %18, i64 4
  store ptr %17, ptr %32, align 8
  %33 = getelementptr inbounds ptr, ptr %18, i64 5
  store ptr %17, ptr %33, align 8
  %34 = getelementptr inbounds ptr, ptr %18, i64 6
  store ptr %17, ptr %34, align 8
  %35 = getelementptr inbounds ptr, ptr %18, i64 7
  store ptr %17, ptr %35, align 8
  %36 = getelementptr inbounds ptr, ptr %18, i64 8
  store ptr %17, ptr %36, align 8
  %37 = getelementptr inbounds ptr, ptr %18, i64 9
  store ptr %17, ptr %37, align 8
  store i32 -4, ptr %19, align 4
  store i32 -7, ptr %20, align 4
  store ptr @g_2112, ptr %21, align 8
  store i32 0, ptr %22, align 4
  br label %38

38:                                               ; preds = %56, %2
  %39 = load i32, ptr %22, align 4
  %40 = icmp slt i32 %39, 5
  br i1 %40, label %41, label %59

41:                                               ; preds = %38
  store i32 0, ptr %23, align 4
  br label %42

42:                                               ; preds = %52, %41
  %43 = load i32, ptr %23, align 4
  %44 = icmp slt i32 %43, 3
  br i1 %44, label %45, label %55

45:                                               ; preds = %42
  %46 = load i32, ptr %22, align 4
  %47 = sext i32 %46 to i64
  %48 = getelementptr inbounds [5 x [3 x ptr]], ptr %15, i64 0, i64 %47
  %49 = load i32, ptr %23, align 4
  %50 = sext i32 %49 to i64
  %51 = getelementptr inbounds [3 x ptr], ptr %48, i64 0, i64 %50
  store ptr null, ptr %51, align 8
  br label %52

52:                                               ; preds = %45
  %53 = load i32, ptr %23, align 4
  %54 = add nsw i32 %53, 1
  store i32 %54, ptr %23, align 4
  br label %42, !llvm.loop !42

55:                                               ; preds = %42
  br label %56

56:                                               ; preds = %55
  %57 = load i32, ptr %22, align 4
  %58 = add nsw i32 %57, 1
  store i32 %58, ptr %22, align 4
  br label %38, !llvm.loop !43

59:                                               ; preds = %38
  %60 = load ptr, ptr %7, align 8
  %61 = icmp ne ptr null, %60
  %62 = zext i1 %61 to i32
  %63 = load volatile i104, ptr @g_1412, align 1
  %64 = lshr i104 %63, 68
  %65 = and i104 %64, 127
  %66 = trunc i104 %65 to i32
  %67 = icmp ne i32 %66, 0
  br i1 %67, label %145, label %68

68:                                               ; preds = %59
  %69 = load i32, ptr %8, align 4
  %70 = trunc i32 %69 to i8
  %71 = load i8, ptr %5, align 1
  %72 = sext i8 %71 to i32
  %73 = load volatile ptr, ptr @g_82, align 8
  %74 = load i16, ptr %73, align 2
  %75 = sext i16 %74 to i32
  %76 = load i32, ptr %8, align 4
  %77 = icmp ne i32 %76, 0
  br i1 %77, label %81, label %78

78:                                               ; preds = %68
  %79 = load i32, ptr %8, align 4
  %80 = icmp ne i32 %79, 0
  br label %81

81:                                               ; preds = %78, %68
  %82 = phi i1 [ true, %68 ], [ %80, %78 ]
  %83 = zext i1 %82 to i32
  %84 = load i32, ptr %8, align 4
  %85 = icmp ne i32 %84, 0
  br i1 %85, label %86, label %87

86:                                               ; preds = %81
  br label %87

87:                                               ; preds = %86, %81
  %88 = phi i1 [ false, %81 ], [ true, %86 ]
  %89 = zext i1 %88 to i32
  %90 = trunc i32 %89 to i16
  %91 = load ptr, ptr %9, align 8
  store i16 %90, ptr %91, align 2
  %92 = sext i16 %90 to i32
  %93 = getelementptr inbounds [6 x [2 x [8 x i32]]], ptr %10, i64 0, i64 1
  %94 = getelementptr inbounds [2 x [8 x i32]], ptr %93, i64 0, i64 0
  %95 = getelementptr inbounds [8 x i32], ptr %94, i64 0, i64 4
  store i32 %92, ptr %95, align 16
  %96 = and i32 %75, %92
  %97 = load i8, ptr %11, align 1
  %98 = sext i8 %97 to i32
  %99 = icmp eq i32 %96, %98
  %100 = zext i1 %99 to i32
  %101 = load i32, ptr %8, align 4
  %102 = icmp sge i32 %100, %101
  %103 = zext i1 %102 to i32
  %104 = sext i32 %103 to i64
  %105 = and i64 %104, 987744120
  %106 = trunc i64 %105 to i32
  %107 = call i32 @safe_div_func_int32_t_s_s(i32 noundef %72, i32 noundef %106)
  %108 = trunc i32 %107 to i8
  %109 = call signext i8 @safe_div_func_int8_t_s_s(i8 noundef signext %70, i8 noundef signext %108)
  %110 = getelementptr inbounds [6 x [2 x [8 x i32]]], ptr %10, i64 0, i64 3
  %111 = getelementptr inbounds [2 x [8 x i32]], ptr %110, i64 0, i64 1
  %112 = getelementptr inbounds [8 x i32], ptr %111, i64 0, i64 5
  %113 = load i32, ptr %112, align 4
  %114 = icmp ne i32 %113, 0
  br i1 %114, label %115, label %119

115:                                              ; preds = %87
  %116 = load i8, ptr %11, align 1
  %117 = sext i8 %116 to i32
  %118 = icmp ne i32 %117, 0
  br label %119

119:                                              ; preds = %115, %87
  %120 = phi i1 [ false, %87 ], [ %118, %115 ]
  %121 = zext i1 %120 to i32
  %122 = trunc i32 %121 to i16
  %123 = load i32, ptr %8, align 4
  %124 = trunc i32 %123 to i16
  %125 = call zeroext i16 @safe_div_func_uint16_t_u_u(i16 noundef zeroext %122, i16 noundef zeroext %124)
  %126 = zext i16 %125 to i32
  %127 = icmp ne i32 %126, 0
  br i1 %127, label %131, label %128

128:                                              ; preds = %119
  %129 = load i32, ptr %8, align 4
  %130 = icmp ne i32 %129, 0
  br label %131

131:                                              ; preds = %128, %119
  %132 = phi i1 [ true, %119 ], [ %130, %128 ]
  %133 = zext i1 %132 to i32
  %134 = load i104, ptr @g_2109, align 16
  %135 = shl i104 %134, 3
  %136 = ashr i104 %135, 78
  %137 = trunc i104 %136 to i32
  %138 = icmp sgt i32 %133, %137
  %139 = zext i1 %138 to i32
  %140 = call i32 @safe_div_func_int32_t_s_s(i32 noundef %139, i32 noundef 695596817)
  %141 = load ptr, ptr @g_2147, align 8
  store i32 %140, ptr %141, align 4
  %142 = xor i32 %140, -1
  %143 = load i32, ptr %8, align 4
  %144 = icmp uge i32 %142, %143
  br label %145

145:                                              ; preds = %131, %59
  %146 = phi i1 [ true, %59 ], [ %144, %131 ]
  %147 = zext i1 %146 to i32
  %148 = trunc i32 %147 to i8
  %149 = call signext i8 @safe_add_func_int8_t_s_s(i8 noundef signext 18, i8 noundef signext %148)
  %150 = sext i8 %149 to i64
  %151 = icmp sge i64 %150, 247596874
  %152 = zext i1 %151 to i32
  %153 = call i32 @safe_div_func_uint32_t_u_u(i32 noundef %152, i32 noundef -1993657773)
  %154 = load i8, ptr @g_723, align 1
  %155 = sext i8 %154 to i32
  %156 = icmp sle i32 1, %155
  %157 = zext i1 %156 to i32
  %158 = icmp sle i32 %62, %157
  %159 = zext i1 %158 to i32
  %160 = sext i32 %159 to i64
  %161 = icmp eq i64 14, %160
  %162 = zext i1 %161 to i32
  %163 = trunc i32 %162 to i16
  %164 = call signext i16 @safe_lshift_func_int16_t_s_u(i16 noundef signext %163, i32 noundef 1)
  %165 = sext i16 %164 to i32
  %166 = load i8, ptr %11, align 1
  %167 = sext i8 %166 to i32
  %168 = call i32 @safe_add_func_uint32_t_u_u(i32 noundef %165, i32 noundef %167)
  %169 = load i8, ptr %11, align 1
  %170 = sext i8 %169 to i32
  %171 = icmp ule i32 %168, %170
  %172 = zext i1 %171 to i32
  %173 = trunc i32 %172 to i8
  %174 = load i32, ptr %8, align 4
  %175 = trunc i32 %174 to i8
  %176 = call signext i8 @safe_mod_func_int8_t_s_s(i8 noundef signext %173, i8 noundef signext %175)
  %177 = sext i8 %176 to i16
  %178 = call signext i16 @safe_rshift_func_int16_t_s_u(i16 noundef signext %177, i32 noundef 5)
  %179 = sext i16 %178 to i32
  %180 = load i8, ptr %5, align 1
  %181 = sext i8 %180 to i32
  %182 = icmp sge i32 %179, %181
  %183 = zext i1 %182 to i32
  %184 = load i32, ptr %12, align 4
  %185 = or i32 %184, %183
  store i32 %185, ptr %12, align 4
  %186 = load ptr, ptr @g_2147, align 8
  %187 = load i32, ptr %186, align 4
  %188 = getelementptr inbounds [6 x [2 x [8 x i32]]], ptr %10, i64 0, i64 0
  %189 = getelementptr inbounds [2 x [8 x i32]], ptr %188, i64 0, i64 1
  %190 = getelementptr inbounds [8 x i32], ptr %189, i64 0, i64 4
  %191 = load i32, ptr %190, align 16
  %192 = xor i32 %191, %187
  store i32 %192, ptr %190, align 16
  %193 = load i32, ptr %8, align 4
  %194 = trunc i32 %193 to i8
  %195 = load i32, ptr %8, align 4
  %196 = trunc i32 %195 to i16
  %197 = load i32, ptr %4, align 4
  %198 = trunc i32 %197 to i16
  %199 = call zeroext i16 @safe_mod_func_uint16_t_u_u(i16 noundef zeroext %196, i16 noundef zeroext %198)
  %200 = trunc i16 %199 to i8
  %201 = call zeroext i8 @safe_add_func_uint8_t_u_u(i8 noundef zeroext 0, i8 noundef zeroext %200)
  %202 = load i32, ptr %4, align 4
  %203 = icmp ne i32 %202, 0
  %204 = zext i1 %203 to i32
  %205 = trunc i32 %204 to i16
  %206 = call zeroext i16 @safe_rshift_func_uint16_t_u_u(i16 noundef zeroext %205, i32 noundef 0)
  %207 = zext i16 %206 to i32
  %208 = call signext i8 @safe_lshift_func_int8_t_s_u(i8 noundef signext %201, i32 noundef %207)
  %209 = load i8, ptr %5, align 1
  %210 = call signext i8 @safe_mod_func_int8_t_s_s(i8 noundef signext %208, i8 noundef signext %209)
  %211 = sext i8 %210 to i16
  %212 = call ptr @func_52(i32 noundef %192, i8 noundef signext %194, i16 noundef zeroext %211)
  store ptr %212, ptr %13, align 8
  %213 = load ptr, ptr %14, align 8
  %214 = icmp ne ptr null, %213
  %215 = zext i1 %214 to i32
  %216 = trunc i32 %215 to i8
  %217 = call signext i8 @safe_lshift_func_int8_t_s_u(i8 noundef signext %216, i32 noundef 2)
  %218 = sext i8 %217 to i32
  %219 = getelementptr inbounds [5 x [3 x ptr]], ptr %15, i64 0, i64 3
  %220 = getelementptr inbounds [3 x ptr], ptr %219, i64 0, i64 1
  %221 = load ptr, ptr %220, align 8
  %222 = load ptr, ptr %16, align 8
  store ptr %221, ptr %222, align 8
  store ptr %221, ptr @g_2251, align 8
  %223 = icmp ne ptr %221, @g_641
  %224 = zext i1 %223 to i32
  %225 = trunc i32 %224 to i8
  %226 = call signext i8 @safe_mod_func_int8_t_s_s(i8 noundef signext %225, i8 noundef signext 124)
  %227 = sext i8 %226 to i16
  %228 = call signext i16 @safe_lshift_func_int16_t_s_u(i16 noundef signext %227, i32 noundef 1)
  %229 = sext i16 %228 to i32
  %230 = load i32, ptr %20, align 4
  %231 = icmp sgt i32 1, %230
  %232 = zext i1 %231 to i32
  %233 = xor i32 %229, %232
  %234 = load i8, ptr %5, align 1
  %235 = sext i8 %234 to i32
  %236 = icmp eq i32 %233, %235
  %237 = zext i1 %236 to i32
  %238 = trunc i32 %237 to i8
  %239 = call signext i8 @safe_rshift_func_int8_t_s_s(i8 noundef signext %238, i32 noundef 3)
  %240 = sext i8 %239 to i32
  %241 = or i32 %218, %240
  %242 = load ptr, ptr @g_1787, align 8
  %243 = load i32, ptr %242, align 4
  %244 = load i8, ptr %5, align 1
  %245 = sext i8 %244 to i32
  %246 = load i8, ptr %5, align 1
  %247 = sext i8 %246 to i32
  %248 = icmp sgt i32 %245, %247
  %249 = zext i1 %248 to i32
  %250 = load ptr, ptr %13, align 8
  store i32 1, ptr %250, align 4
  %251 = load volatile ptr, ptr @g_164, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %3, ptr align 1 %251, i64 13, i1 false)
  %252 = getelementptr inbounds nuw %struct.S0, ptr %3, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %25, ptr align 1 %252, i64 13, i1 false)
  %253 = load { i64, i40 }, ptr %25, align 8
  ret { i64, i40 } %253
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i32 @func_5(i32 noundef %0, i8 noundef signext %1, ptr noundef %2, ptr noundef %3) #0 {
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i8, align 1
  %8 = alloca ptr, align 8
  %9 = alloca ptr, align 8
  %10 = alloca [8 x i64], align 16
  %11 = alloca [6 x [10 x ptr]], align 16
  %12 = alloca [8 x i32], align 16
  %13 = alloca i16, align 2
  %14 = alloca i64, align 8
  %15 = alloca i32, align 4
  %16 = alloca i64, align 8
  %17 = alloca [5 x [10 x ptr]], align 16
  %18 = alloca i8, align 1
  %19 = alloca i8, align 1
  %20 = alloca i16, align 2
  %21 = alloca ptr, align 8
  %22 = alloca i32, align 4
  %23 = alloca i32, align 4
  %24 = alloca i32, align 4
  %25 = alloca i8, align 1
  %26 = alloca ptr, align 8
  %27 = alloca ptr, align 8
  %28 = alloca ptr, align 8
  %29 = alloca i64, align 8
  %30 = alloca i32, align 4
  %31 = alloca ptr, align 8
  %32 = alloca i32, align 4
  %33 = alloca ptr, align 8
  %34 = alloca ptr, align 8
  %35 = alloca ptr, align 8
  %36 = alloca ptr, align 8
  %37 = alloca [6 x i32], align 16
  %38 = alloca i64, align 8
  %39 = alloca ptr, align 8
  %40 = alloca [3 x [6 x [4 x i32]]], align 16
  %41 = alloca i32, align 4
  %42 = alloca i32, align 4
  %43 = alloca i32, align 4
  %44 = alloca ptr, align 8
  %45 = alloca ptr, align 8
  %46 = alloca ptr, align 8
  %47 = alloca ptr, align 8
  %48 = alloca ptr, align 8
  %49 = alloca [3 x [2 x ptr]], align 16
  %50 = alloca i16, align 2
  %51 = alloca ptr, align 8
  %52 = alloca ptr, align 8
  %53 = alloca [5 x ptr], align 16
  %54 = alloca i32, align 4
  %55 = alloca i32, align 4
  %56 = alloca ptr, align 8
  %57 = alloca i32, align 4
  %58 = alloca [3 x ptr], align 16
  %59 = alloca ptr, align 8
  %60 = alloca i32, align 4
  %61 = alloca ptr, align 8
  %62 = alloca ptr, align 8
  %63 = alloca [10 x [8 x [3 x ptr]]], align 16
  %64 = alloca i32, align 4
  %65 = alloca i32, align 4
  %66 = alloca i32, align 4
  %67 = alloca i16, align 2
  %68 = alloca i32, align 4
  %69 = alloca i32, align 4
  %70 = alloca i8, align 1
  %71 = alloca [5 x [3 x ptr]], align 16
  %72 = alloca i32, align 4
  %73 = alloca i32, align 4
  %74 = alloca ptr, align 8
  %75 = alloca i16, align 2
  %76 = alloca [9 x [4 x [7 x ptr]]], align 16
  %77 = alloca ptr, align 8
  %78 = alloca i32, align 4
  %79 = alloca i32, align 4
  %80 = alloca i32, align 4
  %81 = alloca i32, align 4
  %82 = alloca ptr, align 8
  %83 = alloca [4 x ptr], align 16
  %84 = alloca i32, align 4
  %85 = alloca [7 x i64], align 16
  %86 = alloca ptr, align 8
  %87 = alloca i64, align 8
  %88 = alloca i8, align 1
  %89 = alloca ptr, align 8
  %90 = alloca i32, align 4
  %91 = alloca i32, align 4
  %92 = alloca i32, align 4
  %93 = alloca i32, align 4
  %94 = alloca i16, align 2
  %95 = alloca ptr, align 8
  %96 = alloca i32, align 4
  %97 = alloca [2 x ptr], align 16
  %98 = alloca [4 x i32], align 16
  %99 = alloca i32, align 4
  %100 = alloca ptr, align 8
  %101 = alloca ptr, align 8
  %102 = alloca ptr, align 8
  %103 = alloca ptr, align 8
  %104 = alloca [4 x ptr], align 16
  %105 = alloca ptr, align 8
  %106 = alloca ptr, align 8
  %107 = alloca i32, align 4
  %108 = alloca ptr, align 8
  %109 = alloca ptr, align 8
  %110 = alloca ptr, align 8
  %111 = alloca ptr, align 8
  %112 = alloca i32, align 4
  %113 = alloca i8, align 1
  %114 = alloca ptr, align 8
  %115 = alloca ptr, align 8
  %116 = alloca ptr, align 8
  %117 = alloca ptr, align 8
  %118 = alloca [8 x [1 x [7 x i32]]], align 16
  %119 = alloca ptr, align 8
  %120 = alloca ptr, align 8
  %121 = alloca ptr, align 8
  %122 = alloca i32, align 4
  %123 = alloca i32, align 4
  %124 = alloca i32, align 4
  %125 = alloca i32, align 4
  %126 = alloca %struct.S0, align 1
  %127 = alloca i8, align 1
  %128 = alloca ptr, align 8
  %129 = alloca i32, align 4
  %130 = alloca ptr, align 8
  %131 = alloca ptr, align 8
  %132 = alloca ptr, align 8
  %133 = alloca ptr, align 8
  %134 = alloca i32, align 4
  %135 = alloca i64, align 8
  %136 = alloca i32, align 4
  %137 = alloca i32, align 4
  %138 = alloca ptr, align 8
  %139 = alloca i16, align 2
  %140 = alloca ptr, align 8
  %141 = alloca ptr, align 8
  %142 = alloca ptr, align 8
  %143 = alloca [6 x [1 x [3 x i32]]], align 16
  %144 = alloca i32, align 4
  %145 = alloca i32, align 4
  %146 = alloca i32, align 4
  store i32 %0, ptr %6, align 4
  store i8 %1, ptr %7, align 1
  store ptr %2, ptr %8, align 8
  store ptr %3, ptr %9, align 8
  call void @llvm.memset.p0.i64(ptr align 16 %10, i8 -1, i64 64, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %11, ptr align 16 @__const.func_5.l_1764, i64 480, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %12, ptr align 16 @__const.func_5.l_1785, i64 32, i1 false)
  store i16 0, ptr %13, align 2
  store i64 -1405402269920483136, ptr %14, align 8
  store i32 1, ptr %15, align 4
  store i64 5989975823683365978, ptr %16, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %17, ptr align 16 @__const.func_5.l_2069, i64 400, i1 false)
  store i8 5, ptr %18, align 1
  store i8 -33, ptr %19, align 1
  store i16 30170, ptr %20, align 2
  store ptr @g_544, ptr %21, align 8
  store i32 -1, ptr %22, align 4
  store i8 0, ptr %7, align 1
  br label %147

147:                                              ; preds = %937, %4
  %148 = load i8, ptr %7, align 1
  %149 = sext i8 %148 to i32
  %150 = icmp sle i32 %149, 7
  br i1 %150, label %151, label %942

151:                                              ; preds = %147
  store i8 -124, ptr %25, align 1
  store ptr getelementptr inbounds ([8 x i32], ptr getelementptr inbounds ([7 x [1 x [8 x i32]]], ptr @g_17, i64 0, i64 3), i64 0, i64 6), ptr %26, align 8
  store ptr @g_147, ptr %27, align 8
  store ptr null, ptr %28, align 8
  store i64 9, ptr %29, align 8
  store i32 0, ptr %30, align 4
  store ptr @g_1026, ptr %31, align 8
  store i32 -1720432034, ptr %32, align 4
  store ptr @g_486, ptr %33, align 8
  store ptr %33, ptr %34, align 8
  store i32 1, ptr %6, align 4
  br label %152

152:                                              ; preds = %565, %151
  %153 = load i32, ptr %6, align 4
  %154 = icmp ule i32 %153, 7
  br i1 %154, label %155, label %568

155:                                              ; preds = %152
  store ptr @g_303, ptr %35, align 8
  store ptr getelementptr inbounds ([8 x i32], ptr getelementptr inbounds ([7 x [1 x [8 x i32]]], ptr @g_17, i64 0, i64 3), i64 0, i64 6), ptr %36, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %37, ptr align 16 @__const.func_5.l_1796, i64 24, i1 false)
  store i64 1, ptr %38, align 8
  store ptr @g_296, ptr %39, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %40, ptr align 16 @__const.func_5.l_1851, i64 288, i1 false)
  %156 = load i32, ptr %6, align 4
  %157 = zext i32 %156 to i64
  %158 = getelementptr inbounds nuw [8 x i64], ptr %10, i64 0, i64 %157
  %159 = load i64, ptr %158, align 8
  store i64 %159, ptr getelementptr inbounds ([8 x i64], ptr @g_14, i64 0, i64 1), align 8
  %160 = trunc i64 %159 to i8
  %161 = call zeroext i8 @safe_lshift_func_uint8_t_u_s(i8 noundef zeroext %160, i32 noundef 1)
  %162 = icmp ne i8 %161, 0
  br i1 %162, label %163, label %293

163:                                              ; preds = %155
  store ptr null, ptr %44, align 8
  store ptr getelementptr inbounds ([8 x i32], ptr getelementptr inbounds ([7 x [1 x [8 x i32]]], ptr @g_17, i64 0, i64 3), i64 0, i64 6), ptr %45, align 8
  store ptr getelementptr inbounds ([8 x i32], ptr getelementptr inbounds ([7 x [1 x [8 x i32]]], ptr @g_17, i64 0, i64 3), i64 0, i64 6), ptr %46, align 8
  store ptr getelementptr inbounds ([8 x i32], ptr getelementptr inbounds ([7 x [1 x [8 x i32]]], ptr @g_17, i64 0, i64 3), i64 0, i64 6), ptr %47, align 8
  store ptr getelementptr inbounds ([8 x i32], ptr getelementptr inbounds ([7 x [1 x [8 x i32]]], ptr @g_17, i64 0, i64 3), i64 0, i64 6), ptr %48, align 8
  call void @llvm.memset.p0.i64(ptr align 16 %49, i8 0, i64 48, i1 false)
  %164 = getelementptr inbounds [3 x [2 x ptr]], ptr %49, i32 0, i32 0
  %165 = getelementptr inbounds [2 x ptr], ptr %164, i32 0, i32 0
  store ptr getelementptr (i8, ptr @g_17, i64 120), ptr %165, align 16
  %166 = getelementptr inbounds [2 x ptr], ptr %164, i32 0, i32 1
  store ptr getelementptr (i8, ptr @g_17, i64 120), ptr %166, align 8
  %167 = getelementptr inbounds [3 x [2 x ptr]], ptr %49, i32 0, i32 1
  %168 = getelementptr inbounds [2 x ptr], ptr %167, i32 0, i32 0
  store ptr getelementptr (i8, ptr @g_17, i64 120), ptr %168, align 16
  %169 = getelementptr inbounds [2 x ptr], ptr %167, i32 0, i32 1
  store ptr getelementptr (i8, ptr @g_17, i64 120), ptr %169, align 8
  %170 = getelementptr inbounds [3 x [2 x ptr]], ptr %49, i32 0, i32 2
  %171 = getelementptr inbounds [2 x ptr], ptr %170, i32 0, i32 0
  store ptr getelementptr (i8, ptr @g_17, i64 120), ptr %171, align 16
  %172 = getelementptr inbounds [2 x ptr], ptr %170, i32 0, i32 1
  store ptr getelementptr (i8, ptr @g_17, i64 120), ptr %172, align 8
  store i16 1415, ptr %50, align 2
  store ptr @g_220, ptr %51, align 8
  store ptr null, ptr %52, align 8
  store i32 0, ptr %54, align 4
  br label %173

173:                                              ; preds = %180, %163
  %174 = load i32, ptr %54, align 4
  %175 = icmp slt i32 %174, 5
  br i1 %175, label %176, label %183

176:                                              ; preds = %173
  %177 = load i32, ptr %54, align 4
  %178 = sext i32 %177 to i64
  %179 = getelementptr inbounds [5 x ptr], ptr %53, i64 0, i64 %178
  store ptr @g_88, ptr %179, align 8
  br label %180

180:                                              ; preds = %176
  %181 = load i32, ptr %54, align 4
  %182 = add nsw i32 %181, 1
  store i32 %182, ptr %54, align 4
  br label %173, !llvm.loop !44

183:                                              ; preds = %173
  %184 = load i8, ptr %25, align 1
  %185 = add i8 %184, 1
  store i8 %185, ptr %25, align 1
  store i8 2, ptr %25, align 1
  br label %186

186:                                              ; preds = %204, %183
  %187 = load i8, ptr %25, align 1
  %188 = zext i8 %187 to i32
  %189 = icmp sle i32 %188, 7
  br i1 %189, label %190, label %209

190:                                              ; preds = %186
  store ptr @g_45, ptr %56, align 8
  store i32 1759805616, ptr %57, align 4
  store ptr @g_1117, ptr %59, align 8
  store i32 0, ptr %60, align 4
  br label %191

191:                                              ; preds = %200, %190
  %192 = load i32, ptr %60, align 4
  %193 = icmp slt i32 %192, 3
  br i1 %193, label %194, label %203

194:                                              ; preds = %191
  %195 = getelementptr inbounds [3 x [2 x ptr]], ptr %49, i64 0, i64 0
  %196 = getelementptr inbounds [2 x ptr], ptr %195, i64 0, i64 0
  %197 = load i32, ptr %60, align 4
  %198 = sext i32 %197 to i64
  %199 = getelementptr inbounds [3 x ptr], ptr %58, i64 0, i64 %198
  store ptr %196, ptr %199, align 8
  br label %200

200:                                              ; preds = %194
  %201 = load i32, ptr %60, align 4
  %202 = add nsw i32 %201, 1
  store i32 %202, ptr %60, align 4
  br label %191, !llvm.loop !45

203:                                              ; preds = %191
  br label %204

204:                                              ; preds = %203
  %205 = load i8, ptr %25, align 1
  %206 = zext i8 %205 to i32
  %207 = add nsw i32 %206, 1
  %208 = trunc i32 %207 to i8
  store i8 %208, ptr %25, align 1
  br label %186, !llvm.loop !46

209:                                              ; preds = %186
  %210 = load i16, ptr %13, align 2
  %211 = load ptr, ptr @g_544, align 8
  %212 = load i8, ptr %211, align 1
  %213 = call zeroext i8 @safe_lshift_func_uint8_t_u_u(i8 noundef zeroext %212, i32 noundef 7)
  %214 = zext i8 %213 to i32
  %215 = load ptr, ptr %8, align 8
  %216 = load i8, ptr %215, align 1
  %217 = load ptr, ptr %39, align 8
  %218 = icmp ne ptr null, %217
  %219 = zext i1 %218 to i32
  %220 = trunc i32 %219 to i8
  %221 = call signext i8 @safe_mul_func_int8_t_s_s(i8 noundef signext %216, i8 noundef signext %220)
  %222 = sext i8 %221 to i32
  %223 = load ptr, ptr %26, align 8
  %224 = load i32, ptr %223, align 4
  %225 = load ptr, ptr %36, align 8
  %226 = load i32, ptr %225, align 4
  %227 = icmp eq i32 %224, %226
  %228 = zext i1 %227 to i32
  %229 = or i32 %222, %228
  %230 = icmp ne i32 %229, 0
  br i1 %230, label %239, label %231

231:                                              ; preds = %209
  %232 = load ptr, ptr %26, align 8
  %233 = icmp eq ptr %232, @g_486
  br i1 %233, label %237, label %234

234:                                              ; preds = %231
  %235 = load i32, ptr %6, align 4
  %236 = icmp ne i32 %235, 0
  br label %237

237:                                              ; preds = %234, %231
  %238 = phi i1 [ true, %231 ], [ %236, %234 ]
  br label %239

239:                                              ; preds = %237, %209
  %240 = phi i1 [ true, %209 ], [ %238, %237 ]
  %241 = zext i1 %240 to i32
  %242 = load ptr, ptr %51, align 8
  %243 = load i16, ptr %242, align 2
  %244 = zext i16 %243 to i32
  %245 = and i32 %244, %241
  %246 = trunc i32 %245 to i16
  store i16 %246, ptr %242, align 2
  %247 = zext i16 %246 to i32
  %248 = load volatile ptr, ptr @g_82, align 8
  %249 = load i16, ptr %248, align 2
  %250 = sext i16 %249 to i32
  %251 = icmp sle i32 %247, %250
  %252 = zext i1 %251 to i32
  %253 = load i8, ptr @g_118, align 1
  %254 = sext i8 %253 to i32
  %255 = icmp sge i32 %252, %254
  %256 = zext i1 %255 to i32
  %257 = trunc i32 %256 to i8
  %258 = load ptr, ptr @g_837, align 8
  %259 = load ptr, ptr %258, align 8
  %260 = load i8, ptr %259, align 1
  %261 = call zeroext i8 @safe_div_func_uint8_t_u_u(i8 noundef zeroext %257, i8 noundef zeroext %260)
  %262 = zext i8 %261 to i32
  %263 = xor i32 %262, -1
  %264 = trunc i32 %263 to i16
  %265 = call signext i16 @safe_unary_minus_func_int16_t_s(i16 noundef signext %264)
  %266 = sext i16 %265 to i32
  %267 = icmp sge i32 %214, %266
  %268 = zext i1 %267 to i32
  %269 = trunc i32 %268 to i16
  %270 = load volatile ptr, ptr @g_82, align 8
  store i16 %269, ptr %270, align 2
  %271 = sext i16 %269 to i64
  %272 = icmp sge i64 32735, %271
  %273 = zext i1 %272 to i32
  %274 = sext i32 %273 to i64
  %275 = call i64 @safe_div_func_uint64_t_u_u(i64 noundef %274, i64 noundef 1661603439503331423)
  %276 = trunc i64 %275 to i32
  %277 = load ptr, ptr %36, align 8
  %278 = load i32, ptr %277, align 4
  %279 = call i32 @safe_add_func_int32_t_s_s(i32 noundef %276, i32 noundef %278)
  %280 = load ptr, ptr %27, align 8
  %281 = load i32, ptr %280, align 4
  %282 = or i32 %281, %279
  store i32 %282, ptr %280, align 4
  %283 = getelementptr inbounds [3 x [6 x [4 x i32]]], ptr %40, i64 0, i64 0
  %284 = getelementptr inbounds [6 x [4 x i32]], ptr %283, i64 0, i64 3
  %285 = getelementptr inbounds [4 x i32], ptr %284, i64 0, i64 3
  %286 = load i32, ptr %285, align 4
  %287 = sext i32 %286 to i64
  %288 = icmp ult i64 %287, 65535
  %289 = zext i1 %288 to i32
  %290 = load ptr, ptr @g_1015, align 8
  %291 = load i32, ptr %290, align 4
  %292 = load ptr, ptr %36, align 8
  store i32 %291, ptr %292, align 4
  br label %295

293:                                              ; preds = %155
  %294 = load i32, ptr %6, align 4
  store i32 %294, ptr %5, align 4
  br label %1438

295:                                              ; preds = %239
  %296 = load ptr, ptr %36, align 8
  %297 = load i32, ptr %296, align 4
  %298 = icmp ne i32 %297, 0
  br i1 %298, label %299, label %300

299:                                              ; preds = %295
  br label %568

300:                                              ; preds = %295
  %301 = load i8, ptr %7, align 1
  %302 = sext i8 %301 to i32
  %303 = load ptr, ptr %27, align 8
  store i32 %302, ptr %303, align 4
  store i64 6, ptr %38, align 8
  br label %304

304:                                              ; preds = %561, %300
  %305 = load i64, ptr %38, align 8
  %306 = icmp sge i64 %305, 0
  br i1 %306, label %307, label %564

307:                                              ; preds = %304
  store ptr null, ptr %61, align 8
  store ptr %61, ptr %62, align 8
  store ptr %62, ptr %63, align 8
  %308 = getelementptr inbounds ptr, ptr %63, i64 1
  store ptr %62, ptr %308, align 8
  %309 = getelementptr inbounds ptr, ptr %63, i64 2
  store ptr %62, ptr %309, align 8
  %310 = getelementptr inbounds [3 x ptr], ptr %63, i64 1
  store ptr %62, ptr %310, align 8
  %311 = getelementptr inbounds ptr, ptr %310, i64 1
  store ptr null, ptr %311, align 8
  %312 = getelementptr inbounds ptr, ptr %310, i64 2
  store ptr %62, ptr %312, align 8
  %313 = getelementptr inbounds [3 x ptr], ptr %63, i64 2
  store ptr %62, ptr %313, align 8
  %314 = getelementptr inbounds ptr, ptr %313, i64 1
  store ptr %62, ptr %314, align 8
  %315 = getelementptr inbounds ptr, ptr %313, i64 2
  store ptr %62, ptr %315, align 8
  %316 = getelementptr inbounds [3 x ptr], ptr %63, i64 3
  store ptr %62, ptr %316, align 8
  %317 = getelementptr inbounds ptr, ptr %316, i64 1
  store ptr %62, ptr %317, align 8
  %318 = getelementptr inbounds ptr, ptr %316, i64 2
  store ptr %62, ptr %318, align 8
  %319 = getelementptr inbounds [3 x ptr], ptr %63, i64 4
  store ptr %62, ptr %319, align 8
  %320 = getelementptr inbounds ptr, ptr %319, i64 1
  store ptr null, ptr %320, align 8
  %321 = getelementptr inbounds ptr, ptr %319, i64 2
  store ptr %62, ptr %321, align 8
  %322 = getelementptr inbounds [3 x ptr], ptr %63, i64 5
  store ptr %62, ptr %322, align 8
  %323 = getelementptr inbounds ptr, ptr %322, i64 1
  store ptr null, ptr %323, align 8
  %324 = getelementptr inbounds ptr, ptr %322, i64 2
  store ptr %62, ptr %324, align 8
  %325 = getelementptr inbounds [3 x ptr], ptr %63, i64 6
  store ptr %62, ptr %325, align 8
  %326 = getelementptr inbounds ptr, ptr %325, i64 1
  store ptr %62, ptr %326, align 8
  %327 = getelementptr inbounds ptr, ptr %325, i64 2
  store ptr null, ptr %327, align 8
  %328 = getelementptr inbounds [3 x ptr], ptr %63, i64 7
  store ptr %62, ptr %328, align 8
  %329 = getelementptr inbounds ptr, ptr %328, i64 1
  store ptr %62, ptr %329, align 8
  %330 = getelementptr inbounds ptr, ptr %328, i64 2
  store ptr %62, ptr %330, align 8
  %331 = getelementptr inbounds [8 x [3 x ptr]], ptr %63, i64 1
  store ptr %62, ptr %331, align 8
  %332 = getelementptr inbounds ptr, ptr %331, i64 1
  store ptr %62, ptr %332, align 8
  %333 = getelementptr inbounds ptr, ptr %331, i64 2
  store ptr %62, ptr %333, align 8
  %334 = getelementptr inbounds [3 x ptr], ptr %331, i64 1
  store ptr %62, ptr %334, align 8
  %335 = getelementptr inbounds ptr, ptr %334, i64 1
  store ptr %62, ptr %335, align 8
  %336 = getelementptr inbounds ptr, ptr %334, i64 2
  store ptr %62, ptr %336, align 8
  %337 = getelementptr inbounds [3 x ptr], ptr %331, i64 2
  store ptr null, ptr %337, align 8
  %338 = getelementptr inbounds ptr, ptr %337, i64 1
  store ptr %62, ptr %338, align 8
  %339 = getelementptr inbounds ptr, ptr %337, i64 2
  store ptr %62, ptr %339, align 8
  %340 = getelementptr inbounds [3 x ptr], ptr %331, i64 3
  store ptr %62, ptr %340, align 8
  %341 = getelementptr inbounds ptr, ptr %340, i64 1
  store ptr %62, ptr %341, align 8
  %342 = getelementptr inbounds ptr, ptr %340, i64 2
  store ptr %62, ptr %342, align 8
  %343 = getelementptr inbounds [3 x ptr], ptr %331, i64 4
  store ptr null, ptr %343, align 8
  %344 = getelementptr inbounds ptr, ptr %343, i64 1
  store ptr null, ptr %344, align 8
  %345 = getelementptr inbounds ptr, ptr %343, i64 2
  store ptr %62, ptr %345, align 8
  %346 = getelementptr inbounds [3 x ptr], ptr %331, i64 5
  store ptr %62, ptr %346, align 8
  %347 = getelementptr inbounds ptr, ptr %346, i64 1
  store ptr %62, ptr %347, align 8
  %348 = getelementptr inbounds ptr, ptr %346, i64 2
  store ptr %62, ptr %348, align 8
  %349 = getelementptr inbounds [3 x ptr], ptr %331, i64 6
  store ptr %62, ptr %349, align 8
  %350 = getelementptr inbounds ptr, ptr %349, i64 1
  store ptr null, ptr %350, align 8
  %351 = getelementptr inbounds ptr, ptr %349, i64 2
  store ptr %62, ptr %351, align 8
  %352 = getelementptr inbounds [3 x ptr], ptr %331, i64 7
  store ptr %62, ptr %352, align 8
  %353 = getelementptr inbounds ptr, ptr %352, i64 1
  store ptr %62, ptr %353, align 8
  %354 = getelementptr inbounds ptr, ptr %352, i64 2
  store ptr %62, ptr %354, align 8
  %355 = getelementptr inbounds [8 x [3 x ptr]], ptr %63, i64 2
  store ptr %62, ptr %355, align 8
  %356 = getelementptr inbounds ptr, ptr %355, i64 1
  store ptr %62, ptr %356, align 8
  %357 = getelementptr inbounds ptr, ptr %355, i64 2
  store ptr null, ptr %357, align 8
  %358 = getelementptr inbounds [3 x ptr], ptr %355, i64 1
  store ptr null, ptr %358, align 8
  %359 = getelementptr inbounds ptr, ptr %358, i64 1
  store ptr %62, ptr %359, align 8
  %360 = getelementptr inbounds ptr, ptr %358, i64 2
  store ptr null, ptr %360, align 8
  %361 = getelementptr inbounds [3 x ptr], ptr %355, i64 2
  store ptr %62, ptr %361, align 8
  %362 = getelementptr inbounds ptr, ptr %361, i64 1
  store ptr null, ptr %362, align 8
  %363 = getelementptr inbounds ptr, ptr %361, i64 2
  store ptr null, ptr %363, align 8
  %364 = getelementptr inbounds [3 x ptr], ptr %355, i64 3
  store ptr %62, ptr %364, align 8
  %365 = getelementptr inbounds ptr, ptr %364, i64 1
  store ptr %62, ptr %365, align 8
  %366 = getelementptr inbounds ptr, ptr %364, i64 2
  store ptr %62, ptr %366, align 8
  %367 = getelementptr inbounds [3 x ptr], ptr %355, i64 4
  store ptr %62, ptr %367, align 8
  %368 = getelementptr inbounds ptr, ptr %367, i64 1
  store ptr %62, ptr %368, align 8
  %369 = getelementptr inbounds ptr, ptr %367, i64 2
  store ptr %62, ptr %369, align 8
  %370 = getelementptr inbounds [3 x ptr], ptr %355, i64 5
  store ptr %62, ptr %370, align 8
  %371 = getelementptr inbounds ptr, ptr %370, i64 1
  store ptr %62, ptr %371, align 8
  %372 = getelementptr inbounds ptr, ptr %370, i64 2
  store ptr %62, ptr %372, align 8
  %373 = getelementptr inbounds [3 x ptr], ptr %355, i64 6
  store ptr %62, ptr %373, align 8
  %374 = getelementptr inbounds ptr, ptr %373, i64 1
  store ptr null, ptr %374, align 8
  %375 = getelementptr inbounds ptr, ptr %373, i64 2
  store ptr %62, ptr %375, align 8
  %376 = getelementptr inbounds [3 x ptr], ptr %355, i64 7
  store ptr %62, ptr %376, align 8
  %377 = getelementptr inbounds ptr, ptr %376, i64 1
  store ptr %62, ptr %377, align 8
  %378 = getelementptr inbounds ptr, ptr %376, i64 2
  store ptr null, ptr %378, align 8
  %379 = getelementptr inbounds [8 x [3 x ptr]], ptr %63, i64 3
  store ptr %62, ptr %379, align 8
  %380 = getelementptr inbounds ptr, ptr %379, i64 1
  store ptr null, ptr %380, align 8
  %381 = getelementptr inbounds ptr, ptr %379, i64 2
  store ptr null, ptr %381, align 8
  %382 = getelementptr inbounds [3 x ptr], ptr %379, i64 1
  store ptr %62, ptr %382, align 8
  %383 = getelementptr inbounds ptr, ptr %382, i64 1
  store ptr %62, ptr %383, align 8
  %384 = getelementptr inbounds ptr, ptr %382, i64 2
  store ptr null, ptr %384, align 8
  %385 = getelementptr inbounds [3 x ptr], ptr %379, i64 2
  store ptr %62, ptr %385, align 8
  %386 = getelementptr inbounds ptr, ptr %385, i64 1
  store ptr %62, ptr %386, align 8
  %387 = getelementptr inbounds ptr, ptr %385, i64 2
  store ptr %62, ptr %387, align 8
  %388 = getelementptr inbounds [3 x ptr], ptr %379, i64 3
  store ptr null, ptr %388, align 8
  %389 = getelementptr inbounds ptr, ptr %388, i64 1
  store ptr %62, ptr %389, align 8
  %390 = getelementptr inbounds ptr, ptr %388, i64 2
  store ptr %62, ptr %390, align 8
  %391 = getelementptr inbounds [3 x ptr], ptr %379, i64 4
  store ptr null, ptr %391, align 8
  %392 = getelementptr inbounds ptr, ptr %391, i64 1
  store ptr %62, ptr %392, align 8
  %393 = getelementptr inbounds ptr, ptr %391, i64 2
  store ptr %62, ptr %393, align 8
  %394 = getelementptr inbounds [3 x ptr], ptr %379, i64 5
  store ptr %62, ptr %394, align 8
  %395 = getelementptr inbounds ptr, ptr %394, i64 1
  store ptr %62, ptr %395, align 8
  %396 = getelementptr inbounds ptr, ptr %394, i64 2
  store ptr %62, ptr %396, align 8
  %397 = getelementptr inbounds [3 x ptr], ptr %379, i64 6
  store ptr %62, ptr %397, align 8
  %398 = getelementptr inbounds ptr, ptr %397, i64 1
  store ptr %62, ptr %398, align 8
  %399 = getelementptr inbounds ptr, ptr %397, i64 2
  store ptr %62, ptr %399, align 8
  %400 = getelementptr inbounds [3 x ptr], ptr %379, i64 7
  store ptr %62, ptr %400, align 8
  %401 = getelementptr inbounds ptr, ptr %400, i64 1
  store ptr %62, ptr %401, align 8
  %402 = getelementptr inbounds ptr, ptr %400, i64 2
  store ptr %62, ptr %402, align 8
  %403 = getelementptr inbounds [8 x [3 x ptr]], ptr %63, i64 4
  store ptr %62, ptr %403, align 8
  %404 = getelementptr inbounds ptr, ptr %403, i64 1
  store ptr %62, ptr %404, align 8
  %405 = getelementptr inbounds ptr, ptr %403, i64 2
  store ptr %62, ptr %405, align 8
  %406 = getelementptr inbounds [3 x ptr], ptr %403, i64 1
  store ptr %62, ptr %406, align 8
  %407 = getelementptr inbounds ptr, ptr %406, i64 1
  store ptr %62, ptr %407, align 8
  %408 = getelementptr inbounds ptr, ptr %406, i64 2
  store ptr %62, ptr %408, align 8
  %409 = getelementptr inbounds [3 x ptr], ptr %403, i64 2
  store ptr %62, ptr %409, align 8
  %410 = getelementptr inbounds ptr, ptr %409, i64 1
  store ptr %62, ptr %410, align 8
  %411 = getelementptr inbounds ptr, ptr %409, i64 2
  store ptr %62, ptr %411, align 8
  %412 = getelementptr inbounds [3 x ptr], ptr %403, i64 3
  store ptr %62, ptr %412, align 8
  %413 = getelementptr inbounds ptr, ptr %412, i64 1
  store ptr %62, ptr %413, align 8
  %414 = getelementptr inbounds ptr, ptr %412, i64 2
  store ptr null, ptr %414, align 8
  %415 = getelementptr inbounds [3 x ptr], ptr %403, i64 4
  store ptr null, ptr %415, align 8
  %416 = getelementptr inbounds ptr, ptr %415, i64 1
  store ptr %62, ptr %416, align 8
  %417 = getelementptr inbounds ptr, ptr %415, i64 2
  store ptr %62, ptr %417, align 8
  %418 = getelementptr inbounds [3 x ptr], ptr %403, i64 5
  store ptr %62, ptr %418, align 8
  %419 = getelementptr inbounds ptr, ptr %418, i64 1
  store ptr %62, ptr %419, align 8
  %420 = getelementptr inbounds ptr, ptr %418, i64 2
  store ptr %62, ptr %420, align 8
  %421 = getelementptr inbounds [3 x ptr], ptr %403, i64 6
  store ptr %62, ptr %421, align 8
  %422 = getelementptr inbounds ptr, ptr %421, i64 1
  store ptr %62, ptr %422, align 8
  %423 = getelementptr inbounds ptr, ptr %421, i64 2
  store ptr %62, ptr %423, align 8
  %424 = getelementptr inbounds [3 x ptr], ptr %403, i64 7
  store ptr %62, ptr %424, align 8
  %425 = getelementptr inbounds ptr, ptr %424, i64 1
  store ptr %62, ptr %425, align 8
  %426 = getelementptr inbounds ptr, ptr %424, i64 2
  store ptr %62, ptr %426, align 8
  %427 = getelementptr inbounds [8 x [3 x ptr]], ptr %63, i64 5
  store ptr %62, ptr %427, align 8
  %428 = getelementptr inbounds ptr, ptr %427, i64 1
  store ptr %62, ptr %428, align 8
  %429 = getelementptr inbounds ptr, ptr %427, i64 2
  store ptr %62, ptr %429, align 8
  %430 = getelementptr inbounds [3 x ptr], ptr %427, i64 1
  store ptr %62, ptr %430, align 8
  %431 = getelementptr inbounds ptr, ptr %430, i64 1
  store ptr %62, ptr %431, align 8
  %432 = getelementptr inbounds ptr, ptr %430, i64 2
  store ptr %62, ptr %432, align 8
  %433 = getelementptr inbounds [3 x ptr], ptr %427, i64 2
  store ptr %62, ptr %433, align 8
  %434 = getelementptr inbounds ptr, ptr %433, i64 1
  store ptr %62, ptr %434, align 8
  %435 = getelementptr inbounds ptr, ptr %433, i64 2
  store ptr %62, ptr %435, align 8
  %436 = getelementptr inbounds [3 x ptr], ptr %427, i64 3
  store ptr null, ptr %436, align 8
  %437 = getelementptr inbounds ptr, ptr %436, i64 1
  store ptr %62, ptr %437, align 8
  %438 = getelementptr inbounds ptr, ptr %436, i64 2
  store ptr %62, ptr %438, align 8
  %439 = getelementptr inbounds [3 x ptr], ptr %427, i64 4
  store ptr %62, ptr %439, align 8
  %440 = getelementptr inbounds ptr, ptr %439, i64 1
  store ptr %62, ptr %440, align 8
  %441 = getelementptr inbounds ptr, ptr %439, i64 2
  store ptr null, ptr %441, align 8
  %442 = getelementptr inbounds [3 x ptr], ptr %427, i64 5
  store ptr %62, ptr %442, align 8
  %443 = getelementptr inbounds ptr, ptr %442, i64 1
  store ptr %62, ptr %443, align 8
  %444 = getelementptr inbounds ptr, ptr %442, i64 2
  store ptr %62, ptr %444, align 8
  %445 = getelementptr inbounds [3 x ptr], ptr %427, i64 6
  store ptr %62, ptr %445, align 8
  %446 = getelementptr inbounds ptr, ptr %445, i64 1
  store ptr %62, ptr %446, align 8
  %447 = getelementptr inbounds ptr, ptr %445, i64 2
  store ptr %62, ptr %447, align 8
  %448 = getelementptr inbounds [3 x ptr], ptr %427, i64 7
  store ptr %62, ptr %448, align 8
  %449 = getelementptr inbounds ptr, ptr %448, i64 1
  store ptr %62, ptr %449, align 8
  %450 = getelementptr inbounds ptr, ptr %448, i64 2
  store ptr %62, ptr %450, align 8
  %451 = getelementptr inbounds [8 x [3 x ptr]], ptr %63, i64 6
  store ptr %62, ptr %451, align 8
  %452 = getelementptr inbounds ptr, ptr %451, i64 1
  store ptr %62, ptr %452, align 8
  %453 = getelementptr inbounds ptr, ptr %451, i64 2
  store ptr %62, ptr %453, align 8
  %454 = getelementptr inbounds [3 x ptr], ptr %451, i64 1
  store ptr %62, ptr %454, align 8
  %455 = getelementptr inbounds ptr, ptr %454, i64 1
  store ptr %62, ptr %455, align 8
  %456 = getelementptr inbounds ptr, ptr %454, i64 2
  store ptr %62, ptr %456, align 8
  %457 = getelementptr inbounds [3 x ptr], ptr %451, i64 2
  store ptr null, ptr %457, align 8
  %458 = getelementptr inbounds ptr, ptr %457, i64 1
  store ptr %62, ptr %458, align 8
  %459 = getelementptr inbounds ptr, ptr %457, i64 2
  store ptr %62, ptr %459, align 8
  %460 = getelementptr inbounds [3 x ptr], ptr %451, i64 3
  store ptr null, ptr %460, align 8
  %461 = getelementptr inbounds ptr, ptr %460, i64 1
  store ptr %62, ptr %461, align 8
  %462 = getelementptr inbounds ptr, ptr %460, i64 2
  store ptr %62, ptr %462, align 8
  %463 = getelementptr inbounds [3 x ptr], ptr %451, i64 4
  store ptr %62, ptr %463, align 8
  %464 = getelementptr inbounds ptr, ptr %463, i64 1
  store ptr %62, ptr %464, align 8
  %465 = getelementptr inbounds ptr, ptr %463, i64 2
  store ptr %62, ptr %465, align 8
  %466 = getelementptr inbounds [3 x ptr], ptr %451, i64 5
  store ptr null, ptr %466, align 8
  %467 = getelementptr inbounds ptr, ptr %466, i64 1
  store ptr %62, ptr %467, align 8
  %468 = getelementptr inbounds ptr, ptr %466, i64 2
  store ptr null, ptr %468, align 8
  %469 = getelementptr inbounds [3 x ptr], ptr %451, i64 6
  store ptr null, ptr %469, align 8
  %470 = getelementptr inbounds ptr, ptr %469, i64 1
  store ptr %62, ptr %470, align 8
  %471 = getelementptr inbounds ptr, ptr %469, i64 2
  store ptr %62, ptr %471, align 8
  %472 = getelementptr inbounds [3 x ptr], ptr %451, i64 7
  store ptr %62, ptr %472, align 8
  %473 = getelementptr inbounds ptr, ptr %472, i64 1
  store ptr null, ptr %473, align 8
  %474 = getelementptr inbounds ptr, ptr %472, i64 2
  store ptr %62, ptr %474, align 8
  %475 = getelementptr inbounds [8 x [3 x ptr]], ptr %63, i64 7
  store ptr %62, ptr %475, align 8
  %476 = getelementptr inbounds ptr, ptr %475, i64 1
  store ptr %62, ptr %476, align 8
  %477 = getelementptr inbounds ptr, ptr %475, i64 2
  store ptr %62, ptr %477, align 8
  %478 = getelementptr inbounds [3 x ptr], ptr %475, i64 1
  store ptr %62, ptr %478, align 8
  %479 = getelementptr inbounds ptr, ptr %478, i64 1
  store ptr %62, ptr %479, align 8
  %480 = getelementptr inbounds ptr, ptr %478, i64 2
  store ptr %62, ptr %480, align 8
  %481 = getelementptr inbounds [3 x ptr], ptr %475, i64 2
  store ptr %62, ptr %481, align 8
  %482 = getelementptr inbounds ptr, ptr %481, i64 1
  store ptr %62, ptr %482, align 8
  %483 = getelementptr inbounds ptr, ptr %481, i64 2
  store ptr %62, ptr %483, align 8
  %484 = getelementptr inbounds [3 x ptr], ptr %475, i64 3
  store ptr null, ptr %484, align 8
  %485 = getelementptr inbounds ptr, ptr %484, i64 1
  store ptr %62, ptr %485, align 8
  %486 = getelementptr inbounds ptr, ptr %484, i64 2
  store ptr %62, ptr %486, align 8
  %487 = getelementptr inbounds [3 x ptr], ptr %475, i64 4
  store ptr null, ptr %487, align 8
  %488 = getelementptr inbounds ptr, ptr %487, i64 1
  store ptr %62, ptr %488, align 8
  %489 = getelementptr inbounds ptr, ptr %487, i64 2
  store ptr %62, ptr %489, align 8
  %490 = getelementptr inbounds [3 x ptr], ptr %475, i64 5
  store ptr %62, ptr %490, align 8
  %491 = getelementptr inbounds ptr, ptr %490, i64 1
  store ptr %62, ptr %491, align 8
  %492 = getelementptr inbounds ptr, ptr %490, i64 2
  store ptr %62, ptr %492, align 8
  %493 = getelementptr inbounds [3 x ptr], ptr %475, i64 6
  store ptr %62, ptr %493, align 8
  %494 = getelementptr inbounds ptr, ptr %493, i64 1
  store ptr %62, ptr %494, align 8
  %495 = getelementptr inbounds ptr, ptr %493, i64 2
  store ptr %62, ptr %495, align 8
  %496 = getelementptr inbounds [3 x ptr], ptr %475, i64 7
  store ptr %62, ptr %496, align 8
  %497 = getelementptr inbounds ptr, ptr %496, i64 1
  store ptr %62, ptr %497, align 8
  %498 = getelementptr inbounds ptr, ptr %496, i64 2
  store ptr %62, ptr %498, align 8
  %499 = getelementptr inbounds [8 x [3 x ptr]], ptr %63, i64 8
  store ptr %62, ptr %499, align 8
  %500 = getelementptr inbounds ptr, ptr %499, i64 1
  store ptr %62, ptr %500, align 8
  %501 = getelementptr inbounds ptr, ptr %499, i64 2
  store ptr null, ptr %501, align 8
  %502 = getelementptr inbounds [3 x ptr], ptr %499, i64 1
  store ptr null, ptr %502, align 8
  %503 = getelementptr inbounds ptr, ptr %502, i64 1
  store ptr %62, ptr %503, align 8
  %504 = getelementptr inbounds ptr, ptr %502, i64 2
  store ptr %62, ptr %504, align 8
  %505 = getelementptr inbounds [3 x ptr], ptr %499, i64 2
  store ptr null, ptr %505, align 8
  %506 = getelementptr inbounds ptr, ptr %505, i64 1
  store ptr null, ptr %506, align 8
  %507 = getelementptr inbounds ptr, ptr %505, i64 2
  store ptr %62, ptr %507, align 8
  %508 = getelementptr inbounds [3 x ptr], ptr %499, i64 3
  store ptr %62, ptr %508, align 8
  %509 = getelementptr inbounds ptr, ptr %508, i64 1
  store ptr %62, ptr %509, align 8
  %510 = getelementptr inbounds ptr, ptr %508, i64 2
  store ptr null, ptr %510, align 8
  %511 = getelementptr inbounds [3 x ptr], ptr %499, i64 4
  store ptr null, ptr %511, align 8
  %512 = getelementptr inbounds ptr, ptr %511, i64 1
  store ptr %62, ptr %512, align 8
  %513 = getelementptr inbounds ptr, ptr %511, i64 2
  store ptr %62, ptr %513, align 8
  %514 = getelementptr inbounds [3 x ptr], ptr %499, i64 5
  store ptr null, ptr %514, align 8
  %515 = getelementptr inbounds ptr, ptr %514, i64 1
  store ptr %62, ptr %515, align 8
  %516 = getelementptr inbounds ptr, ptr %514, i64 2
  store ptr null, ptr %516, align 8
  %517 = getelementptr inbounds [3 x ptr], ptr %499, i64 6
  store ptr %62, ptr %517, align 8
  %518 = getelementptr inbounds ptr, ptr %517, i64 1
  store ptr null, ptr %518, align 8
  %519 = getelementptr inbounds ptr, ptr %517, i64 2
  store ptr %62, ptr %519, align 8
  %520 = getelementptr inbounds [3 x ptr], ptr %499, i64 7
  store ptr %62, ptr %520, align 8
  %521 = getelementptr inbounds ptr, ptr %520, i64 1
  store ptr %62, ptr %521, align 8
  %522 = getelementptr inbounds ptr, ptr %520, i64 2
  store ptr %62, ptr %522, align 8
  %523 = getelementptr inbounds [8 x [3 x ptr]], ptr %63, i64 9
  store ptr %62, ptr %523, align 8
  %524 = getelementptr inbounds ptr, ptr %523, i64 1
  store ptr %62, ptr %524, align 8
  %525 = getelementptr inbounds ptr, ptr %523, i64 2
  store ptr %62, ptr %525, align 8
  %526 = getelementptr inbounds [3 x ptr], ptr %523, i64 1
  store ptr %62, ptr %526, align 8
  %527 = getelementptr inbounds ptr, ptr %526, i64 1
  store ptr %62, ptr %527, align 8
  %528 = getelementptr inbounds ptr, ptr %526, i64 2
  store ptr %62, ptr %528, align 8
  %529 = getelementptr inbounds [3 x ptr], ptr %523, i64 2
  store ptr %62, ptr %529, align 8
  %530 = getelementptr inbounds ptr, ptr %529, i64 1
  store ptr %62, ptr %530, align 8
  %531 = getelementptr inbounds ptr, ptr %529, i64 2
  store ptr %62, ptr %531, align 8
  %532 = getelementptr inbounds [3 x ptr], ptr %523, i64 3
  store ptr %62, ptr %532, align 8
  %533 = getelementptr inbounds ptr, ptr %532, i64 1
  store ptr %62, ptr %533, align 8
  %534 = getelementptr inbounds ptr, ptr %532, i64 2
  store ptr null, ptr %534, align 8
  %535 = getelementptr inbounds [3 x ptr], ptr %523, i64 4
  store ptr null, ptr %535, align 8
  %536 = getelementptr inbounds ptr, ptr %535, i64 1
  store ptr %62, ptr %536, align 8
  %537 = getelementptr inbounds ptr, ptr %535, i64 2
  store ptr %62, ptr %537, align 8
  %538 = getelementptr inbounds [3 x ptr], ptr %523, i64 5
  store ptr %62, ptr %538, align 8
  %539 = getelementptr inbounds ptr, ptr %538, i64 1
  store ptr %62, ptr %539, align 8
  %540 = getelementptr inbounds ptr, ptr %538, i64 2
  store ptr %62, ptr %540, align 8
  %541 = getelementptr inbounds [3 x ptr], ptr %523, i64 6
  store ptr %62, ptr %541, align 8
  %542 = getelementptr inbounds ptr, ptr %541, i64 1
  store ptr %62, ptr %542, align 8
  %543 = getelementptr inbounds ptr, ptr %541, i64 2
  store ptr %62, ptr %543, align 8
  %544 = getelementptr inbounds [3 x ptr], ptr %523, i64 7
  store ptr %62, ptr %544, align 8
  %545 = getelementptr inbounds ptr, ptr %544, i64 1
  store ptr %62, ptr %545, align 8
  %546 = getelementptr inbounds ptr, ptr %544, i64 2
  store ptr %62, ptr %546, align 8
  %547 = getelementptr inbounds [10 x [8 x [3 x ptr]]], ptr %63, i64 0, i64 9
  %548 = getelementptr inbounds [8 x [3 x ptr]], ptr %547, i64 0, i64 5
  %549 = getelementptr inbounds [3 x ptr], ptr %548, i64 0, i64 1
  %550 = load ptr, ptr %549, align 8
  %551 = icmp ne ptr null, %550
  %552 = zext i1 %551 to i32
  %553 = load ptr, ptr @g_1299, align 8
  %554 = load volatile ptr, ptr %553, align 8
  %555 = icmp ne ptr null, %554
  %556 = zext i1 %555 to i32
  %557 = and i32 %552, %556
  %558 = load ptr, ptr %26, align 8
  %559 = load i32, ptr %558, align 4
  %560 = or i32 %559, %557
  store i32 %560, ptr %558, align 4
  br label %561

561:                                              ; preds = %307
  %562 = load i64, ptr %38, align 8
  %563 = sub nsw i64 %562, 1
  store i64 %563, ptr %38, align 8
  br label %304, !llvm.loop !47

564:                                              ; preds = %304
  br label %565

565:                                              ; preds = %564
  %566 = load i32, ptr %6, align 4
  %567 = add i32 %566, 1
  store i32 %567, ptr %6, align 4
  br label %152, !llvm.loop !48

568:                                              ; preds = %299, %152
  store i16 0, ptr @g_833, align 2
  br label %569

569:                                              ; preds = %931, %568
  %570 = load i16, ptr @g_833, align 2
  %571 = zext i16 %570 to i32
  %572 = icmp sle i32 %571, 7
  br i1 %572, label %573, label %936

573:                                              ; preds = %569
  store i16 -10, ptr %67, align 2
  store i32 -3, ptr %68, align 4
  store i32 -1, ptr %69, align 4
  store i8 -1, ptr %70, align 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %71, ptr align 16 @__const.func_5.l_1929, i64 120, i1 false)
  store i32 8, ptr @g_1026, align 4
  br label %574

574:                                              ; preds = %927, %573
  %575 = load i32, ptr @g_1026, align 4
  %576 = icmp sge i32 %575, 2
  br i1 %576, label %577, label %930

577:                                              ; preds = %574
  store ptr null, ptr %74, align 8
  store i16 30794, ptr %75, align 2
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %76, ptr align 16 @__const.func_5.l_1930, i64 2016, i1 false)
  store ptr @g_220, ptr %77, align 8
  store i64 0, ptr @g_355, align 8
  br label %578

578:                                              ; preds = %880, %577
  %579 = load i64, ptr @g_355, align 8
  %580 = icmp ule i64 %579, 7
  br i1 %580, label %581, label %883

581:                                              ; preds = %578
  store i32 5, ptr %81, align 4
  store ptr %29, ptr %82, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %83, ptr align 16 @__const.func_5.l_1866, i64 32, i1 false)
  %582 = load i32, ptr %6, align 4
  %583 = load i8, ptr %7, align 1
  %584 = sext i8 %583 to i32
  %585 = load i32, ptr %81, align 4
  %586 = load i32, ptr %6, align 4
  %587 = load volatile ptr, ptr @g_82, align 8
  %588 = load i16, ptr %587, align 2
  %589 = call signext i8 @safe_rshift_func_int8_t_s_s(i8 noundef signext -127, i32 noundef 2)
  %590 = sext i8 %589 to i64
  %591 = and i64 %590, 3
  %592 = icmp ne i64 %591, 0
  br i1 %592, label %593, label %611

593:                                              ; preds = %581
  %594 = load ptr, ptr %74, align 8
  %595 = icmp eq ptr null, %594
  %596 = zext i1 %595 to i32
  %597 = load i104, ptr @g_839, align 1
  %598 = shl i104 %597, 3
  %599 = ashr i104 %598, 78
  %600 = trunc i104 %599 to i32
  %601 = icmp slt i32 %596, %600
  %602 = zext i1 %601 to i32
  %603 = sext i32 %602 to i64
  %604 = load ptr, ptr %82, align 8
  store i64 %603, ptr %604, align 8
  br i1 true, label %605, label %606

605:                                              ; preds = %593
  br label %606

606:                                              ; preds = %605, %593
  %607 = phi i1 [ false, %593 ], [ true, %605 ]
  %608 = zext i1 %607 to i32
  %609 = sext i32 %608 to i64
  store i64 %609, ptr @g_294, align 8
  %610 = icmp ne i64 %609, 0
  br label %611

611:                                              ; preds = %606, %581
  %612 = phi i1 [ false, %581 ], [ %610, %606 ]
  %613 = zext i1 %612 to i32
  %614 = trunc i32 %613 to i8
  %615 = load i104, ptr @g_1780, align 1
  %616 = lshr i104 %615, 45
  %617 = and i104 %616, 8388607
  %618 = trunc i104 %617 to i32
  %619 = trunc i32 %618 to i8
  %620 = call signext i8 @safe_mod_func_int8_t_s_s(i8 noundef signext %614, i8 noundef signext %619)
  %621 = sext i8 %620 to i16
  %622 = call signext i16 @safe_sub_func_int16_t_s_s(i16 noundef signext %588, i16 noundef signext %621)
  %623 = sext i16 %622 to i32
  %624 = xor i32 %586, %623
  %625 = load ptr, ptr %26, align 8
  store i32 %624, ptr %625, align 4
  %626 = load ptr, ptr @g_803, align 8
  %627 = load i32, ptr %626, align 4
  %628 = icmp sge i32 %624, %627
  %629 = zext i1 %628 to i32
  %630 = icmp sge i32 %585, %629
  %631 = zext i1 %630 to i32
  %632 = trunc i32 %631 to i16
  %633 = load i8, ptr %7, align 1
  %634 = sext i8 %633 to i16
  %635 = call zeroext i16 @safe_mul_func_uint16_t_u_u(i16 noundef zeroext %632, i16 noundef zeroext %634)
  %636 = zext i16 %635 to i32
  %637 = load i16, ptr %67, align 2
  %638 = sext i16 %637 to i32
  %639 = xor i32 %636, %638
  %640 = xor i32 %584, %639
  %641 = icmp ne i32 %640, 0
  br i1 %641, label %642, label %643

642:                                              ; preds = %611
  br label %643

643:                                              ; preds = %642, %611
  %644 = phi i1 [ false, %611 ], [ true, %642 ]
  %645 = zext i1 %644 to i32
  %646 = icmp ule i32 %582, %645
  %647 = zext i1 %646 to i32
  %648 = load i16, ptr %75, align 2
  %649 = sext i16 %648 to i32
  %650 = icmp slt i32 %647, %649
  %651 = zext i1 %650 to i32
  store i32 %651, ptr %68, align 4
  %652 = load ptr, ptr %27, align 8
  store i32 %651, ptr %652, align 4
  br i1 %650, label %653, label %860

653:                                              ; preds = %643
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %85, ptr align 16 @__const.func_5.l_1880, i64 56, i1 false)
  store ptr %67, ptr %86, align 8
  store i64 -648168820299086620, ptr %87, align 8
  store i8 -71, ptr %88, align 1
  store ptr %88, ptr %89, align 8
  store i32 0, ptr %90, align 4
  %654 = load ptr, ptr %26, align 8
  store i32 3, ptr %654, align 4
  %655 = load i16, ptr %67, align 2
  %656 = load i32, ptr %81, align 4
  %657 = trunc i32 %656 to i8
  %658 = load ptr, ptr @g_837, align 8
  %659 = load ptr, ptr %658, align 8
  store i8 %657, ptr %659, align 1
  %660 = load volatile ptr, ptr @g_82, align 8
  %661 = load i16, ptr %660, align 2
  %662 = sext i16 %661 to i64
  %663 = icmp sge i64 31861, %662
  %664 = zext i1 %663 to i32
  %665 = getelementptr inbounds [7 x i64], ptr %85, i64 0, i64 6
  %666 = load i64, ptr %665, align 16
  %667 = load ptr, ptr %27, align 8
  %668 = load i32, ptr %667, align 4
  %669 = load i16, ptr %67, align 2
  %670 = sext i16 %669 to i32
  %671 = icmp sle i32 %668, %670
  %672 = zext i1 %671 to i32
  %673 = trunc i32 %672 to i8
  %674 = load i104, ptr @g_590, align 1
  %675 = shl i104 %674, 59
  %676 = ashr i104 %675, 79
  %677 = trunc i104 %676 to i32
  %678 = load i104, ptr @g_248, align 1
  %679 = lshr i104 %678, 45
  %680 = and i104 %679, 8388607
  %681 = trunc i104 %680 to i32
  %682 = load ptr, ptr %26, align 8
  store i32 %681, ptr %682, align 4
  %683 = trunc i32 %681 to i8
  %684 = call signext i8 @safe_add_func_int8_t_s_s(i8 noundef signext %683, i8 noundef signext -64)
  %685 = sext i8 %684 to i32
  %686 = call i32 @safe_mod_func_uint32_t_u_u(i32 noundef %677, i32 noundef %685)
  %687 = trunc i32 %686 to i8
  %688 = call signext i8 @safe_mul_func_int8_t_s_s(i8 noundef signext %673, i8 noundef signext %687)
  %689 = load i32, ptr %6, align 4
  %690 = trunc i32 %689 to i16
  %691 = load i8, ptr %7, align 1
  %692 = sext i8 %691 to i16
  %693 = call zeroext i16 @safe_div_func_uint16_t_u_u(i16 noundef zeroext %690, i16 noundef zeroext %692)
  %694 = zext i16 %693 to i64
  %695 = load i8, ptr %7, align 1
  %696 = sext i8 %695 to i64
  %697 = call i64 @safe_div_func_int64_t_s_s(i64 noundef %694, i64 noundef %696)
  %698 = icmp ne i64 %697, 0
  br i1 %698, label %700, label %699

699:                                              ; preds = %653
  br label %700

700:                                              ; preds = %699, %653
  %701 = phi i1 [ true, %653 ], [ true, %699 ]
  %702 = zext i1 %701 to i32
  %703 = load i16, ptr %67, align 2
  %704 = sext i16 %703 to i32
  %705 = xor i32 %702, %704
  %706 = icmp ne i32 %705, 0
  br i1 %706, label %707, label %708

707:                                              ; preds = %700
  br label %708

708:                                              ; preds = %707, %700
  %709 = phi i1 [ false, %700 ], [ true, %707 ]
  %710 = zext i1 %709 to i32
  %711 = sext i32 %710 to i64
  %712 = icmp ne i64 %666, %711
  %713 = zext i1 %712 to i32
  %714 = trunc i32 %713 to i8
  %715 = load i32, ptr %6, align 4
  %716 = call zeroext i8 @safe_lshift_func_uint8_t_u_u(i8 noundef zeroext %714, i32 noundef %715)
  %717 = load i16, ptr @g_45, align 2
  %718 = load i32, ptr %6, align 4
  %719 = load i32, ptr %6, align 4
  %720 = call i32 @safe_add_func_uint32_t_u_u(i32 noundef %718, i32 noundef %719)
  %721 = trunc i32 %720 to i16
  %722 = load i32, ptr %6, align 4
  %723 = trunc i32 %722 to i16
  %724 = call zeroext i16 @safe_mod_func_uint16_t_u_u(i16 noundef zeroext %721, i16 noundef zeroext %723)
  %725 = zext i16 %724 to i32
  %726 = icmp ne i32 %664, %725
  %727 = zext i1 %726 to i32
  %728 = trunc i32 %727 to i16
  %729 = load i32, ptr getelementptr inbounds ([7 x i32], ptr @g_880, i64 0, i64 2), align 8
  %730 = trunc i32 %729 to i16
  %731 = call signext i16 @safe_add_func_int16_t_s_s(i16 noundef signext %728, i16 noundef signext %730)
  %732 = sext i16 %731 to i32
  %733 = icmp ne i32 %732, 0
  br i1 %733, label %734, label %735

734:                                              ; preds = %708
  br label %735

735:                                              ; preds = %734, %708
  %736 = phi i1 [ false, %708 ], [ true, %734 ]
  %737 = zext i1 %736 to i32
  %738 = trunc i32 %737 to i8
  %739 = call zeroext i8 @safe_sub_func_uint8_t_u_u(i8 noundef zeroext %657, i8 noundef zeroext %738)
  %740 = zext i8 %739 to i32
  store i32 %740, ptr %68, align 4
  %741 = load i32, ptr %6, align 4
  %742 = load ptr, ptr %26, align 8
  store i32 %741, ptr %742, align 4
  %743 = load i8, ptr %7, align 1
  %744 = sext i8 %743 to i32
  %745 = icmp ne i32 %744, 0
  br i1 %745, label %846, label %746

746:                                              ; preds = %735
  %747 = load ptr, ptr @g_837, align 8
  %748 = load ptr, ptr %747, align 8
  %749 = load i8, ptr %748, align 1
  %750 = zext i8 %749 to i32
  %751 = load i32, ptr %30, align 4
  %752 = load ptr, ptr %86, align 8
  store i16 15444, ptr %752, align 2
  %753 = load volatile ptr, ptr @g_82, align 8
  store i16 15444, ptr %753, align 2
  store i32 15444, ptr %68, align 4
  %754 = load i32, ptr %69, align 4
  %755 = trunc i32 %754 to i8
  %756 = call zeroext i8 @safe_mul_func_uint8_t_u_u(i8 noundef zeroext 46, i8 noundef zeroext %755)
  %757 = zext i8 %756 to i32
  %758 = load i32, ptr %81, align 4
  %759 = trunc i32 %758 to i16
  %760 = call zeroext i16 @safe_lshift_func_uint16_t_u_u(i16 noundef zeroext %759, i32 noundef 6)
  %761 = zext i16 %760 to i32
  %762 = load ptr, ptr %9, align 8
  %763 = load i8, ptr %762, align 1
  %764 = load ptr, ptr @g_1013, align 8
  %765 = load ptr, ptr %764, align 8
  %766 = load ptr, ptr %765, align 8
  %767 = icmp eq ptr %766, @g_1026
  %768 = zext i1 %767 to i32
  %769 = trunc i32 %768 to i8
  %770 = load i64, ptr %87, align 8
  %771 = trunc i64 %770 to i8
  %772 = call signext i8 @safe_mul_func_int8_t_s_s(i8 noundef signext %769, i8 noundef signext %771)
  %773 = load i8, ptr %88, align 1
  br i1 true, label %774, label %777

774:                                              ; preds = %746
  %775 = load i32, ptr %81, align 4
  %776 = icmp ne i32 %775, 0
  br label %777

777:                                              ; preds = %774, %746
  %778 = phi i1 [ false, %746 ], [ %776, %774 ]
  %779 = zext i1 %778 to i32
  %780 = load i104, ptr @g_839, align 1
  %781 = lshr i104 %780, 45
  %782 = and i104 %781, 8388607
  %783 = trunc i104 %782 to i32
  %784 = xor i32 %779, %783
  %785 = icmp ne i32 %761, %784
  %786 = zext i1 %785 to i32
  %787 = sext i32 %786 to i64
  %788 = load i8, ptr @g_118, align 1
  %789 = sext i8 %788 to i64
  %790 = call i64 @safe_mod_func_uint64_t_u_u(i64 noundef %787, i64 noundef %789)
  %791 = trunc i64 %790 to i8
  %792 = load i32, ptr @g_147, align 4
  %793 = trunc i32 %792 to i8
  %794 = call signext i8 @safe_sub_func_int8_t_s_s(i8 noundef signext %791, i8 noundef signext %793)
  %795 = sext i8 %794 to i16
  %796 = load i32, ptr %6, align 4
  %797 = call signext i16 @safe_rshift_func_int16_t_s_s(i16 noundef signext %795, i32 noundef %796)
  %798 = call zeroext i16 @safe_rshift_func_uint16_t_u_s(i16 noundef zeroext %797, i32 noundef 2)
  %799 = zext i16 %798 to i32
  %800 = load ptr, ptr @g_837, align 8
  %801 = load ptr, ptr %800, align 8
  %802 = load i8, ptr %801, align 1
  %803 = zext i8 %802 to i32
  %804 = icmp slt i32 %799, %803
  %805 = zext i1 %804 to i32
  %806 = sext i32 %805 to i64
  %807 = icmp sgt i64 %806, -7
  %808 = zext i1 %807 to i32
  %809 = trunc i32 %808 to i8
  %810 = load ptr, ptr %89, align 8
  store i8 %809, ptr %810, align 1
  %811 = load ptr, ptr %8, align 8
  %812 = load i8, ptr %811, align 1
  %813 = call zeroext i8 @safe_mul_func_uint8_t_u_u(i8 noundef zeroext %809, i8 noundef zeroext %812)
  %814 = zext i8 %813 to i32
  %815 = load i32, ptr %81, align 4
  %816 = icmp sge i32 %814, %815
  %817 = zext i1 %816 to i32
  %818 = call i32 @safe_add_func_uint32_t_u_u(i32 noundef %757, i32 noundef %817)
  %819 = trunc i32 %818 to i16
  %820 = call signext i16 @safe_sub_func_int16_t_s_s(i16 noundef signext 15444, i16 noundef signext %819)
  %821 = getelementptr inbounds [7 x i64], ptr %85, i64 0, i64 3
  %822 = load i64, ptr %821, align 8
  %823 = load i32, ptr @g_1026, align 4
  %824 = sext i32 %823 to i64
  %825 = and i64 %822, %824
  %826 = xor i64 %825, 0
  %827 = and i64 %826, 1
  %828 = load i8, ptr %70, align 1
  %829 = sext i8 %828 to i64
  %830 = icmp eq i64 %827, %829
  %831 = zext i1 %830 to i32
  %832 = sext i32 %831 to i64
  %833 = icmp sge i64 %832, 1
  %834 = zext i1 %833 to i32
  %835 = load i32, ptr %6, align 4
  %836 = call i32 @safe_div_func_int32_t_s_s(i32 noundef %834, i32 noundef %835)
  store i32 %836, ptr %69, align 4
  %837 = load i104, ptr @g_310, align 1
  %838 = shl i104 %837, 59
  %839 = ashr i104 %838, 79
  %840 = trunc i104 %839 to i32
  %841 = call i32 @safe_mod_func_int32_t_s_s(i32 noundef %836, i32 noundef %840)
  %842 = load ptr, ptr %9, align 8
  %843 = load i8, ptr %842, align 1
  %844 = sext i8 %843 to i32
  %845 = icmp sge i32 %750, %844
  br label %846

846:                                              ; preds = %777, %735
  %847 = phi i1 [ true, %735 ], [ %845, %777 ]
  %848 = zext i1 %847 to i32
  %849 = sext i32 %848 to i64
  %850 = load i64, ptr @g_355, align 8
  %851 = getelementptr inbounds nuw [8 x i64], ptr %10, i64 0, i64 %850
  store i64 %849, ptr %851, align 8
  %852 = icmp ne i64 %849, 0
  br i1 %852, label %854, label %853

853:                                              ; preds = %846
  br label %854

854:                                              ; preds = %853, %846
  %855 = phi i1 [ true, %846 ], [ true, %853 ]
  %856 = zext i1 %855 to i32
  %857 = sext i32 %856 to i64
  %858 = or i64 %857, -1
  %859 = trunc i64 %858 to i32
  store i32 %859, ptr %90, align 4
  br label %879

860:                                              ; preds = %643
  store i32 1, ptr %92, align 4
  %861 = load i32, ptr %92, align 4
  %862 = add i32 %861, -1
  store i32 %862, ptr %92, align 4
  %863 = getelementptr inbounds [5 x [3 x ptr]], ptr %71, i64 0, i64 2
  %864 = getelementptr inbounds [3 x ptr], ptr %863, i64 0, i64 2
  %865 = load ptr, ptr %864, align 16
  %866 = icmp ne ptr null, %865
  %867 = zext i1 %866 to i32
  %868 = load ptr, ptr %27, align 8
  %869 = load i32, ptr %868, align 4
  %870 = or i32 %869, %867
  store i32 %870, ptr %868, align 4
  %871 = getelementptr inbounds [9 x [4 x [7 x ptr]]], ptr %76, i64 0, i64 5
  %872 = getelementptr inbounds [4 x [7 x ptr]], ptr %871, i64 0, i64 2
  %873 = getelementptr inbounds [7 x ptr], ptr %872, i64 0, i64 0
  %874 = load ptr, ptr %873, align 16
  %875 = load i32, ptr @g_1026, align 4
  %876 = add nsw i32 %875, 1
  %877 = sext i32 %876 to i64
  %878 = getelementptr inbounds [10 x ptr], ptr @g_840, i64 0, i64 %877
  store volatile ptr %874, ptr %878, align 8
  br label %879

879:                                              ; preds = %860, %854
  br label %880

880:                                              ; preds = %879
  %881 = load i64, ptr @g_355, align 8
  %882 = add i64 %881, 1
  store i64 %882, ptr @g_355, align 8
  br label %578, !llvm.loop !49

883:                                              ; preds = %578
  %884 = load ptr, ptr @g_837, align 8
  %885 = load ptr, ptr %884, align 8
  %886 = load i8, ptr %885, align 1
  %887 = load ptr, ptr %8, align 8
  %888 = load i8, ptr %887, align 1
  %889 = sext i8 %888 to i32
  %890 = call zeroext i8 @safe_lshift_func_uint8_t_u_s(i8 noundef zeroext %886, i32 noundef %889)
  %891 = zext i8 %890 to i64
  %892 = or i64 %891, -1
  %893 = load ptr, ptr %34, align 8
  %894 = icmp eq ptr null, %893
  %895 = zext i1 %894 to i32
  %896 = sext i32 %895 to i64
  %897 = call i64 @safe_sub_func_uint64_t_u_u(i64 noundef 1, i64 noundef %896)
  %898 = load i8, ptr %7, align 1
  %899 = sext i8 %898 to i32
  %900 = load ptr, ptr %8, align 8
  %901 = load i8, ptr %900, align 1
  %902 = load ptr, ptr @g_837, align 8
  %903 = load ptr, ptr %902, align 8
  %904 = load i8, ptr %903, align 1
  %905 = zext i8 %904 to i32
  %906 = call signext i8 @safe_lshift_func_int8_t_s_u(i8 noundef signext %901, i32 noundef %905)
  %907 = sext i8 %906 to i32
  %908 = icmp sle i32 %899, %907
  %909 = zext i1 %908 to i32
  %910 = sext i32 %909 to i64
  %911 = xor i64 %897, %910
  %912 = icmp ne i64 %911, 0
  br i1 %912, label %916, label %913

913:                                              ; preds = %883
  %914 = load i32, ptr %6, align 4
  %915 = icmp ne i32 %914, 0
  br label %916

916:                                              ; preds = %913, %883
  %917 = phi i1 [ true, %883 ], [ %915, %913 ]
  %918 = zext i1 %917 to i32
  %919 = sext i32 %918 to i64
  %920 = call i64 @safe_add_func_int64_t_s_s(i64 noundef %892, i64 noundef %919)
  %921 = trunc i64 %920 to i16
  %922 = load ptr, ptr %77, align 8
  store i16 %921, ptr %922, align 2
  %923 = zext i16 %921 to i64
  %924 = and i64 %923, 5489
  %925 = trunc i64 %924 to i32
  %926 = load ptr, ptr %27, align 8
  store i32 %925, ptr %926, align 4
  br label %927

927:                                              ; preds = %916
  %928 = load i32, ptr @g_1026, align 4
  %929 = sub nsw i32 %928, 1
  store i32 %929, ptr @g_1026, align 4
  br label %574, !llvm.loop !50

930:                                              ; preds = %574
  store i32 1019426258, ptr %5, align 4
  br label %1438

931:                                              ; No predecessors!
  %932 = load i16, ptr @g_833, align 2
  %933 = zext i16 %932 to i32
  %934 = add nsw i32 %933, 1
  %935 = trunc i32 %934 to i16
  store i16 %935, ptr @g_833, align 2
  br label %569, !llvm.loop !51

936:                                              ; preds = %569
  br label %937

937:                                              ; preds = %936
  %938 = load i8, ptr %7, align 1
  %939 = sext i8 %938 to i32
  %940 = add nsw i32 %939, 1
  %941 = trunc i32 %940 to i8
  store i8 %941, ptr %7, align 1
  br label %147, !llvm.loop !52

942:                                              ; preds = %147
  store i8 2, ptr @g_723, align 1
  br label %943

943:                                              ; preds = %1136, %942
  %944 = load i8, ptr @g_723, align 1
  %945 = sext i8 %944 to i32
  %946 = icmp eq i32 %945, 1
  br i1 %946, label %947, label %1139

947:                                              ; preds = %943
  store i16 28308, ptr %94, align 2
  store ptr @g_220, ptr %95, align 8
  store i32 -10, ptr %96, align 4
  store i32 0, ptr @g_486, align 4
  br label %948

948:                                              ; preds = %1053, %947
  %949 = load i32, ptr @g_486, align 4
  %950 = icmp uge i32 %949, 58
  br i1 %950, label %951, label %1056

951:                                              ; preds = %948
  store i32 0, ptr %99, align 4
  br label %952

952:                                              ; preds = %959, %951
  %953 = load i32, ptr %99, align 4
  %954 = icmp slt i32 %953, 2
  br i1 %954, label %955, label %962

955:                                              ; preds = %952
  %956 = load i32, ptr %99, align 4
  %957 = sext i32 %956 to i64
  %958 = getelementptr inbounds [2 x ptr], ptr %97, i64 0, i64 %957
  store ptr getelementptr inbounds ([9 x ptr], ptr @g_1946, i64 0, i64 2), ptr %958, align 8
  br label %959

959:                                              ; preds = %955
  %960 = load i32, ptr %99, align 4
  %961 = add nsw i32 %960, 1
  store i32 %961, ptr %99, align 4
  br label %952, !llvm.loop !53

962:                                              ; preds = %952
  store i32 0, ptr %99, align 4
  br label %963

963:                                              ; preds = %970, %962
  %964 = load i32, ptr %99, align 4
  %965 = icmp slt i32 %964, 4
  br i1 %965, label %966, label %973

966:                                              ; preds = %963
  %967 = load i32, ptr %99, align 4
  %968 = sext i32 %967 to i64
  %969 = getelementptr inbounds [4 x i32], ptr %98, i64 0, i64 %968
  store i32 -1, ptr %969, align 4
  br label %970

970:                                              ; preds = %966
  %971 = load i32, ptr %99, align 4
  %972 = add nsw i32 %971, 1
  store i32 %972, ptr %99, align 4
  br label %963, !llvm.loop !54

973:                                              ; preds = %963
  %974 = load ptr, ptr getelementptr inbounds ([9 x ptr], ptr @g_1946, i64 0, i64 2), align 16
  store ptr %974, ptr getelementptr inbounds ([9 x ptr], ptr @g_1946, i64 0, i64 5), align 8
  %975 = icmp ne ptr %974, @g_1947
  br i1 %975, label %976, label %980

976:                                              ; preds = %973
  %977 = load ptr, ptr @g_1117, align 8
  %978 = load ptr, ptr %977, align 8
  %979 = load ptr, ptr %978, align 8
  store ptr null, ptr %979, align 8
  br label %1045

980:                                              ; preds = %973
  store ptr null, ptr %100, align 8
  store ptr @g_88, ptr %101, align 8
  store ptr %100, ptr %102, align 8
  store ptr %102, ptr %103, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %104, ptr align 16 @__const.func_5.l_1964, i64 32, i1 false)
  %981 = getelementptr inbounds [4 x ptr], ptr %104, i64 0, i64 0
  store ptr %981, ptr %105, align 8
  store ptr @g_81, ptr %106, align 8
  %982 = load i16, ptr %94, align 2
  %983 = zext i16 %982 to i32
  %984 = call zeroext i8 @safe_rshift_func_uint8_t_u_s(i8 noundef zeroext -26, i32 noundef 3)
  %985 = zext i8 %984 to i32
  %986 = load ptr, ptr @g_837, align 8
  %987 = load ptr, ptr %986, align 8
  %988 = load i8, ptr %987, align 1
  %989 = zext i8 %988 to i32
  %990 = or i32 %989, %985
  %991 = trunc i32 %990 to i8
  store i8 %991, ptr %987, align 1
  %992 = zext i8 %991 to i32
  %993 = load i32, ptr %6, align 4
  %994 = trunc i32 %993 to i8
  %995 = call zeroext i8 @safe_sub_func_uint8_t_u_u(i8 noundef zeroext %994, i8 noundef zeroext 114)
  %996 = zext i8 %995 to i32
  %997 = icmp ne i32 %996, 0
  br i1 %997, label %1014, label %998

998:                                              ; preds = %980
  %999 = load ptr, ptr %101, align 8
  %1000 = load i8, ptr %999, align 1
  %1001 = add i8 %1000, 1
  store i8 %1001, ptr %999, align 1
  %1002 = zext i8 %1000 to i32
  %1003 = load ptr, ptr %103, align 8
  store ptr null, ptr %1003, align 8
  %1004 = load volatile ptr, ptr @g_980, align 8
  %1005 = load ptr, ptr %1004, align 8
  %1006 = load ptr, ptr %105, align 8
  store ptr %1005, ptr %1006, align 8
  %1007 = icmp eq ptr null, %1005
  %1008 = zext i1 %1007 to i32
  %1009 = getelementptr inbounds [4 x i32], ptr %98, i64 0, i64 0
  %1010 = load i32, ptr %1009, align 16
  %1011 = or i32 %1008, %1010
  %1012 = and i32 %1002, %1011
  %1013 = icmp ne i32 %1012, 0
  br label %1014

1014:                                             ; preds = %998, %980
  %1015 = phi i1 [ true, %980 ], [ %1013, %998 ]
  %1016 = zext i1 %1015 to i32
  %1017 = icmp eq i32 %992, %1016
  %1018 = zext i1 %1017 to i32
  %1019 = or i32 %983, %1018
  %1020 = load ptr, ptr %9, align 8
  %1021 = load i8, ptr %1020, align 1
  %1022 = sext i8 %1021 to i32
  %1023 = xor i32 %1022, %1019
  %1024 = trunc i32 %1023 to i8
  store i8 %1024, ptr %1020, align 1
  %1025 = call signext i8 @safe_add_func_int8_t_s_s(i8 noundef signext %1024, i8 noundef signext 1)
  %1026 = sext i8 %1025 to i32
  %1027 = load i32, ptr %6, align 4
  %1028 = icmp ule i32 %1026, %1027
  %1029 = zext i1 %1028 to i32
  %1030 = load i16, ptr %94, align 2
  %1031 = trunc i16 %1030 to i8
  %1032 = load i32, ptr %15, align 4
  %1033 = trunc i32 %1032 to i16
  %1034 = call ptr @func_52(i32 noundef %1029, i8 noundef signext %1031, i16 noundef zeroext %1033)
  store ptr %1034, ptr @g_1015, align 8
  %1035 = load i32, ptr %6, align 4
  %1036 = zext i32 %1035 to i64
  %1037 = call i64 @safe_mod_func_uint64_t_u_u(i64 noundef %1036, i64 noundef -3474228455904487669)
  %1038 = getelementptr inbounds [4 x i32], ptr %98, i64 0, i64 0
  %1039 = load i32, ptr %1038, align 16
  %1040 = sext i32 %1039 to i64
  %1041 = and i64 %1040, %1037
  %1042 = trunc i64 %1041 to i32
  store i32 %1042, ptr %1038, align 16
  %1043 = load volatile i64, ptr @g_1313, align 8
  %1044 = trunc i64 %1043 to i32
  store i32 %1044, ptr %5, align 4
  br label %1438

1045:                                             ; preds = %976
  %1046 = load ptr, ptr @g_1013, align 8
  %1047 = load ptr, ptr %1046, align 8
  %1048 = load ptr, ptr %1047, align 8
  %1049 = load i32, ptr %1048, align 4
  %1050 = getelementptr inbounds [4 x i32], ptr %98, i64 0, i64 2
  %1051 = load i32, ptr %1050, align 8
  %1052 = xor i32 %1051, %1049
  store i32 %1052, ptr %1050, align 8
  br label %1053

1053:                                             ; preds = %1045
  %1054 = load i32, ptr @g_486, align 4
  %1055 = add i32 %1054, 1
  store i32 %1055, ptr @g_486, align 4
  br label %948, !llvm.loop !55

1056:                                             ; preds = %948
  store i64 0, ptr @g_782, align 8
  br label %1057

1057:                                             ; preds = %1130, %1056
  %1058 = load i64, ptr @g_782, align 8
  %1059 = icmp sgt i64 %1058, 6
  br i1 %1059, label %1060, label %1135

1060:                                             ; preds = %1057
  store ptr null, ptr %108, align 8
  store ptr @g_294, ptr %109, align 8
  store ptr @g_1990, ptr %110, align 8
  store ptr getelementptr inbounds ([8 x i64], ptr @g_14, i64 0, i64 1), ptr %111, align 8
  store i32 -4, ptr %112, align 4
  %1061 = load ptr, ptr %95, align 8
  %1062 = icmp eq ptr null, %1061
  %1063 = zext i1 %1062 to i32
  %1064 = sext i32 %1063 to i64
  %1065 = load i16, ptr %94, align 2
  %1066 = zext i16 %1065 to i32
  %1067 = load ptr, ptr %9, align 8
  %1068 = load i8, ptr %1067, align 1
  %1069 = sext i8 %1068 to i32
  %1070 = xor i32 %1069, %1066
  %1071 = trunc i32 %1070 to i8
  store i8 %1071, ptr %1067, align 1
  %1072 = sext i8 %1071 to i32
  %1073 = load i8, ptr %7, align 1
  %1074 = sext i8 %1073 to i32
  %1075 = icmp sge i32 %1072, %1074
  %1076 = zext i1 %1075 to i32
  %1077 = load ptr, ptr @g_544, align 8
  store i8 1, ptr %1077, align 1
  br i1 true, label %1106, label %1078

1078:                                             ; preds = %1060
  %1079 = load ptr, ptr %109, align 8
  %1080 = load i64, ptr %1079, align 8
  %1081 = add i64 %1080, -1
  store i64 %1081, ptr %1079, align 8
  %1082 = load i32, ptr %6, align 4
  %1083 = load i64, ptr @g_355, align 8
  %1084 = add i64 %1083, -1
  store i64 %1084, ptr @g_355, align 8
  %1085 = load i8, ptr %7, align 1
  %1086 = sext i8 %1085 to i64
  %1087 = icmp ult i64 %1086, 4
  %1088 = zext i1 %1087 to i32
  %1089 = sext i32 %1088 to i64
  %1090 = icmp ugt i64 %1084, %1089
  %1091 = zext i1 %1090 to i32
  %1092 = and i32 %1082, %1091
  %1093 = load i32, ptr %6, align 4
  %1094 = or i32 %1092, %1093
  %1095 = load i32, ptr %6, align 4
  %1096 = icmp ult i32 %1094, %1095
  %1097 = zext i1 %1096 to i32
  %1098 = trunc i32 %1097 to i16
  %1099 = load volatile ptr, ptr @g_82, align 8
  %1100 = load i16, ptr %1099, align 2
  %1101 = sext i16 %1100 to i32
  %1102 = call signext i16 @safe_rshift_func_int16_t_s_s(i16 noundef signext %1098, i32 noundef %1101)
  %1103 = sext i16 %1102 to i64
  %1104 = load ptr, ptr %110, align 8
  store i64 %1103, ptr %1104, align 8
  %1105 = icmp ule i64 %1081, %1103
  br label %1106

1106:                                             ; preds = %1078, %1060
  %1107 = phi i1 [ true, %1060 ], [ %1105, %1078 ]
  %1108 = zext i1 %1107 to i32
  %1109 = load i32, ptr %6, align 4
  %1110 = xor i32 %1108, %1109
  %1111 = or i32 %1076, %1110
  %1112 = zext i32 %1111 to i64
  %1113 = load ptr, ptr %111, align 8
  %1114 = load i64, ptr %1113, align 8
  %1115 = and i64 %1114, %1112
  store i64 %1115, ptr %1113, align 8
  %1116 = trunc i64 %1115 to i32
  store i32 %1116, ptr %112, align 4
  %1117 = sext i32 %1116 to i64
  %1118 = call i64 @safe_sub_func_int64_t_s_s(i64 noundef %1117, i64 noundef 5577882538839176966)
  %1119 = icmp uge i64 %1064, -3078218897780116012
  %1120 = zext i1 %1119 to i32
  %1121 = trunc i32 %1120 to i8
  %1122 = load i32, ptr @g_71, align 4
  %1123 = trunc i32 %1122 to i8
  %1124 = call signext i8 @safe_mul_func_int8_t_s_s(i8 noundef signext %1121, i8 noundef signext %1123)
  %1125 = call signext i8 @safe_mul_func_int8_t_s_s(i8 noundef signext %1124, i8 noundef signext 118)
  store i32 -167097063, ptr %96, align 4
  %1126 = load i32, ptr %96, align 4
  %1127 = icmp ne i32 %1126, 0
  br i1 %1127, label %1128, label %1129

1128:                                             ; preds = %1106
  br label %1135

1129:                                             ; preds = %1106
  br label %1130

1130:                                             ; preds = %1129
  %1131 = load i64, ptr @g_782, align 8
  %1132 = trunc i64 %1131 to i8
  %1133 = call signext i8 @safe_add_func_int8_t_s_s(i8 noundef signext %1132, i8 noundef signext 7)
  %1134 = sext i8 %1133 to i64
  store i64 %1134, ptr @g_782, align 8
  br label %1057, !llvm.loop !56

1135:                                             ; preds = %1128, %1057
  br label %1136

1136:                                             ; preds = %1135
  %1137 = load i8, ptr @g_723, align 1
  %1138 = add i8 %1137, -1
  store i8 %1138, ptr @g_723, align 1
  br label %943, !llvm.loop !57

1139:                                             ; preds = %943
  store i64 0, ptr @g_1990, align 8
  br label %1140

1140:                                             ; preds = %1433, %1139
  %1141 = load i64, ptr @g_1990, align 8
  %1142 = icmp ule i64 %1141, 0
  br i1 %1142, label %1143, label %1436

1143:                                             ; preds = %1140
  store i8 6, ptr %113, align 1
  store ptr @g_355, ptr %114, align 8
  store ptr getelementptr inbounds ([8 x i64], ptr @g_14, i64 0, i64 2), ptr %115, align 8
  %1144 = getelementptr inbounds [8 x i64], ptr %10, i64 0, i64 0
  store ptr %1144, ptr %116, align 8
  store ptr @g_237, ptr %117, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %118, ptr align 16 @__const.func_5.l_2006, i64 224, i1 false)
  store ptr @g_486, ptr %119, align 8
  store ptr @g_1040, ptr %120, align 8
  store ptr @g_544, ptr %121, align 8
  store i32 1272743369, ptr %122, align 4
  %1145 = load i8, ptr %7, align 1
  %1146 = sext i8 %1145 to i32
  %1147 = load i8, ptr %113, align 1
  %1148 = zext i8 %1147 to i64
  %1149 = load ptr, ptr %114, align 8
  %1150 = load i64, ptr %1149, align 8
  %1151 = and i64 %1150, %1148
  store i64 %1151, ptr %1149, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %126, ptr align 1 @g_1999, i64 13, i1 true)
  %1152 = load i64, ptr %16, align 8
  %1153 = load i32, ptr %6, align 4
  %1154 = zext i32 %1153 to i64
  %1155 = icmp uge i64 %1152, %1154
  %1156 = zext i1 %1155 to i32
  %1157 = load ptr, ptr %9, align 8
  %1158 = load i8, ptr %1157, align 1
  %1159 = load ptr, ptr %8, align 8
  store i8 %1158, ptr %1159, align 1
  %1160 = sext i8 %1158 to i32
  %1161 = load volatile i104, ptr @g_839, align 1
  %1162 = shl i104 %1161, 84
  %1163 = ashr i104 %1162, 84
  %1164 = trunc i104 %1163 to i32
  %1165 = sext i32 %1164 to i64
  %1166 = load ptr, ptr %115, align 8
  store i64 %1165, ptr %1166, align 8
  %1167 = load ptr, ptr %116, align 8
  %1168 = load i64, ptr %1167, align 8
  %1169 = or i64 %1168, %1165
  store i64 %1169, ptr %1167, align 8
  %1170 = load i8, ptr %7, align 1
  %1171 = sext i8 %1170 to i64
  %1172 = icmp ne i64 %1169, %1171
  %1173 = zext i1 %1172 to i32
  %1174 = load i8, ptr %7, align 1
  %1175 = sext i8 %1174 to i32
  %1176 = xor i32 %1173, %1175
  %1177 = trunc i32 %1176 to i8
  %1178 = load i8, ptr %113, align 1
  %1179 = zext i8 %1178 to i32
  %1180 = call signext i8 @safe_lshift_func_int8_t_s_s(i8 noundef signext %1177, i32 noundef %1179)
  %1181 = sext i8 %1180 to i32
  %1182 = and i32 %1160, %1181
  %1183 = sext i32 %1182 to i64
  %1184 = icmp sgt i64 %1183, 4013443885
  %1185 = zext i1 %1184 to i32
  %1186 = load ptr, ptr %117, align 8
  store i8 -2, ptr %1186, align 1
  %1187 = load i32, ptr @g_236, align 4
  %1188 = icmp slt i32 -2, %1187
  %1189 = zext i1 %1188 to i32
  %1190 = sext i32 %1189 to i64
  %1191 = load i104, ptr @g_1780, align 1
  %1192 = shl i104 %1191, 3
  %1193 = ashr i104 %1192, 78
  %1194 = trunc i104 %1193 to i32
  %1195 = sext i32 %1194 to i64
  %1196 = call i64 @safe_sub_func_int64_t_s_s(i64 noundef %1190, i64 noundef %1195)
  %1197 = icmp eq i64 %1151, %1196
  %1198 = zext i1 %1197 to i32
  %1199 = sext i32 %1198 to i64
  %1200 = icmp uge i64 -1403031610562490107, %1199
  %1201 = zext i1 %1200 to i32
  %1202 = trunc i32 %1201 to i8
  %1203 = load i32, ptr %6, align 4
  %1204 = trunc i32 %1203 to i8
  %1205 = call zeroext i8 @safe_mul_func_uint8_t_u_u(i8 noundef zeroext %1202, i8 noundef zeroext %1204)
  %1206 = zext i8 %1205 to i32
  %1207 = icmp ne i32 %1206, 0
  br i1 %1207, label %1208, label %1209

1208:                                             ; preds = %1143
  br label %1209

1209:                                             ; preds = %1208, %1143
  %1210 = phi i1 [ false, %1143 ], [ true, %1208 ]
  %1211 = zext i1 %1210 to i32
  %1212 = xor i32 %1146, %1211
  %1213 = getelementptr inbounds [8 x [1 x [7 x i32]]], ptr %118, i64 0, i64 4
  %1214 = getelementptr inbounds [1 x [7 x i32]], ptr %1213, i64 0, i64 0
  %1215 = getelementptr inbounds [7 x i32], ptr %1214, i64 0, i64 4
  %1216 = load i32, ptr %1215, align 16
  %1217 = or i32 %1216, %1212
  store i32 %1217, ptr %1215, align 16
  %1218 = load ptr, ptr %9, align 8
  %1219 = load i8, ptr %1218, align 1
  %1220 = call signext i8 @safe_rshift_func_int8_t_s_s(i8 noundef signext %1219, i32 noundef 3)
  %1221 = load volatile i104, ptr @g_1412, align 1
  %1222 = lshr i104 %1221, 68
  %1223 = and i104 %1222, 127
  %1224 = trunc i104 %1223 to i32
  %1225 = load ptr, ptr %119, align 8
  %1226 = load i32, ptr %1225, align 4
  %1227 = and i32 %1226, %1224
  store i32 %1227, ptr %1225, align 4
  %1228 = load i32, ptr %6, align 4
  %1229 = icmp ne i32 %1228, 0
  br i1 %1229, label %1230, label %1235

1230:                                             ; preds = %1209
  %1231 = load i8, ptr %7, align 1
  %1232 = load i8, ptr %7, align 1
  %1233 = sext i8 %1232 to i32
  %1234 = icmp ne i32 %1233, 0
  br label %1235

1235:                                             ; preds = %1230, %1209
  %1236 = phi i1 [ false, %1209 ], [ %1234, %1230 ]
  %1237 = zext i1 %1236 to i32
  %1238 = load i64, ptr @g_1990, align 8
  %1239 = getelementptr inbounds nuw [1 x ptr], ptr @g_1947, i64 0, i64 %1238
  %1240 = icmp ne ptr null, %1239
  %1241 = zext i1 %1240 to i32
  %1242 = icmp eq i32 %1237, %1241
  %1243 = zext i1 %1242 to i32
  %1244 = xor i32 %1227, %1243
  %1245 = getelementptr inbounds [8 x [1 x [7 x i32]]], ptr %118, i64 0, i64 3
  %1246 = getelementptr inbounds [1 x [7 x i32]], ptr %1245, i64 0, i64 0
  %1247 = getelementptr inbounds [7 x i32], ptr %1246, i64 0, i64 2
  %1248 = load i32, ptr %1247, align 4
  %1249 = trunc i32 %1248 to i8
  %1250 = load i32, ptr %6, align 4
  %1251 = load i32, ptr getelementptr inbounds ([7 x i32], ptr @g_880, i64 0, i64 2), align 8
  %1252 = or i32 %1250, %1251
  %1253 = trunc i32 %1252 to i16
  %1254 = call zeroext i16 @safe_rshift_func_uint16_t_u_u(i16 noundef zeroext %1253, i32 noundef 5)
  %1255 = zext i16 %1254 to i64
  %1256 = xor i64 %1255, -7748406960759202257
  %1257 = trunc i64 %1256 to i8
  %1258 = call zeroext i8 @safe_mod_func_uint8_t_u_u(i8 noundef zeroext %1249, i8 noundef zeroext %1257)
  %1259 = zext i8 %1258 to i32
  %1260 = load i8, ptr %113, align 1
  %1261 = zext i8 %1260 to i32
  %1262 = xor i32 %1259, %1261
  %1263 = icmp ne i32 %1244, %1262
  %1264 = zext i1 %1263 to i32
  %1265 = trunc i32 %1264 to i16
  %1266 = call signext i16 @safe_mod_func_int16_t_s_s(i16 noundef signext %1265, i16 noundef signext 6089)
  %1267 = sext i16 %1266 to i64
  %1268 = icmp slt i64 %1267, 0
  %1269 = zext i1 %1268 to i32
  %1270 = trunc i32 %1269 to i8
  %1271 = call zeroext i8 @safe_mul_func_uint8_t_u_u(i8 noundef zeroext %1220, i8 noundef zeroext %1270)
  %1272 = zext i8 %1271 to i32
  %1273 = icmp ne i32 %1272, 0
  br i1 %1273, label %1279, label %1274

1274:                                             ; preds = %1235
  %1275 = load ptr, ptr %8, align 8
  %1276 = load i8, ptr %1275, align 1
  %1277 = sext i8 %1276 to i32
  %1278 = icmp ne i32 %1277, 0
  br label %1279

1279:                                             ; preds = %1274, %1235
  %1280 = phi i1 [ true, %1235 ], [ %1278, %1274 ]
  %1281 = zext i1 %1280 to i32
  %1282 = load volatile i104, ptr @g_842, align 1
  %1283 = shl i104 %1282, 84
  %1284 = ashr i104 %1283, 84
  %1285 = trunc i104 %1284 to i32
  %1286 = sext i32 %1285 to i64
  %1287 = getelementptr inbounds [8 x [1 x [7 x i32]]], ptr %118, i64 0, i64 1
  %1288 = getelementptr inbounds [1 x [7 x i32]], ptr %1287, i64 0, i64 0
  %1289 = getelementptr inbounds [7 x i32], ptr %1288, i64 0, i64 0
  %1290 = load i32, ptr %1289, align 4
  %1291 = sext i32 %1290 to i64
  %1292 = call i64 @safe_div_func_int64_t_s_s(i64 noundef %1286, i64 noundef %1291)
  %1293 = icmp sgt i64 %1292, 234
  %1294 = zext i1 %1293 to i32
  %1295 = sext i32 %1294 to i64
  %1296 = icmp uge i64 %1295, 1
  %1297 = zext i1 %1296 to i32
  %1298 = load ptr, ptr @g_1329, align 8
  store ptr null, ptr %1298, align 8
  store i8 0, ptr @g_88, align 1
  br label %1299

1299:                                             ; preds = %1304, %1279
  %1300 = load i8, ptr @g_88, align 1
  %1301 = zext i8 %1300 to i32
  %1302 = icmp sle i32 %1301, 0
  br i1 %1302, label %1303, label %1309

1303:                                             ; preds = %1299
  store i8 -1, ptr %127, align 1
  store ptr null, ptr %128, align 8
  store i32 0, ptr %129, align 4
  store ptr null, ptr %130, align 8
  store ptr %130, ptr %131, align 8
  store ptr @g_544, ptr %132, align 8
  store ptr @g_2113, ptr %133, align 8
  store i32 1, ptr %134, align 4
  store i64 1, ptr %135, align 8
  store i32 -5, ptr %136, align 4
  store i32 0, ptr %137, align 4
  store ptr @g_516, ptr %138, align 8
  br label %1304

1304:                                             ; preds = %1303
  %1305 = load i8, ptr @g_88, align 1
  %1306 = zext i8 %1305 to i32
  %1307 = add nsw i32 %1306, 1
  %1308 = trunc i32 %1307 to i8
  store i8 %1308, ptr @g_88, align 1
  br label %1299, !llvm.loop !58

1309:                                             ; preds = %1299
  store i32 0, ptr %15, align 4
  br label %1310

1310:                                             ; preds = %1429, %1309
  %1311 = load i32, ptr %15, align 4
  %1312 = icmp ule i32 %1311, 8
  br i1 %1312, label %1313, label %1432

1313:                                             ; preds = %1310
  store i16 5240, ptr %139, align 2
  store ptr @g_1584, ptr %140, align 8
  store ptr %140, ptr %141, align 8
  store ptr @g_235, ptr %142, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %143, ptr align 16 @__const.func_5.l_2197, i64 72, i1 false)
  %1314 = load i8, ptr %7, align 1
  %1315 = sext i8 %1314 to i32
  %1316 = load ptr, ptr %9, align 8
  %1317 = load i8, ptr %1316, align 1
  %1318 = sext i8 %1317 to i32
  %1319 = icmp ne i32 %1318, 0
  br i1 %1319, label %1320, label %1324

1320:                                             ; preds = %1313
  %1321 = load i16, ptr %139, align 2
  %1322 = sext i16 %1321 to i32
  %1323 = icmp ne i32 %1322, 0
  br label %1324

1324:                                             ; preds = %1320, %1313
  %1325 = phi i1 [ false, %1313 ], [ %1323, %1320 ]
  %1326 = zext i1 %1325 to i32
  %1327 = load ptr, ptr %141, align 8
  store ptr null, ptr %1327, align 8
  %1328 = load i8, ptr %7, align 1
  %1329 = load i8, ptr %7, align 1
  %1330 = load i8, ptr %7, align 1
  %1331 = sext i8 %1330 to i64
  %1332 = xor i64 1455433689312738264, %1331
  %1333 = trunc i64 %1332 to i8
  %1334 = call zeroext i8 @safe_rshift_func_uint8_t_u_s(i8 noundef zeroext %1333, i32 noundef 1)
  %1335 = zext i8 %1334 to i32
  %1336 = load i8, ptr %7, align 1
  %1337 = sext i8 %1336 to i32
  %1338 = icmp sle i32 %1335, %1337
  %1339 = zext i1 %1338 to i32
  %1340 = trunc i32 %1339 to i16
  %1341 = load ptr, ptr %142, align 8
  store i16 %1340, ptr %1341, align 2
  %1342 = load i8, ptr %113, align 1
  %1343 = zext i8 %1342 to i32
  %1344 = getelementptr inbounds [8 x [1 x [7 x i32]]], ptr %118, i64 0, i64 2
  %1345 = getelementptr inbounds [1 x [7 x i32]], ptr %1344, i64 0, i64 0
  %1346 = getelementptr inbounds [7 x i32], ptr %1345, i64 0, i64 5
  store i32 %1343, ptr %1346, align 4
  %1347 = load volatile i104, ptr @g_1484, align 1
  %1348 = shl i104 %1347, 84
  %1349 = ashr i104 %1348, 84
  %1350 = trunc i104 %1349 to i32
  %1351 = icmp sle i32 %1343, %1350
  br i1 %1351, label %1352, label %1356

1352:                                             ; preds = %1324
  %1353 = load i8, ptr %7, align 1
  %1354 = sext i8 %1353 to i32
  %1355 = icmp ne i32 %1354, 0
  br label %1356

1356:                                             ; preds = %1352, %1324
  %1357 = phi i1 [ false, %1324 ], [ %1355, %1352 ]
  %1358 = zext i1 %1357 to i32
  %1359 = icmp slt i32 0, %1358
  %1360 = zext i1 %1359 to i32
  %1361 = trunc i32 %1360 to i16
  %1362 = load i32, ptr %6, align 4
  %1363 = trunc i32 %1362 to i16
  %1364 = call zeroext i16 @safe_add_func_uint16_t_u_u(i16 noundef zeroext %1361, i16 noundef zeroext %1363)
  %1365 = load i16, ptr %139, align 2
  %1366 = sext i16 %1365 to i32
  %1367 = load i8, ptr %7, align 1
  %1368 = sext i8 %1367 to i32
  %1369 = icmp sle i32 %1366, %1368
  %1370 = zext i1 %1369 to i32
  %1371 = getelementptr inbounds [6 x [1 x [3 x i32]]], ptr %143, i64 0, i64 0
  %1372 = getelementptr inbounds [1 x [3 x i32]], ptr %1371, i64 0, i64 0
  %1373 = getelementptr inbounds [3 x i32], ptr %1372, i64 0, i64 0
  store i32 %1370, ptr %1373, align 16
  %1374 = load i32, ptr %6, align 4
  %1375 = icmp eq i32 %1370, %1374
  %1376 = zext i1 %1375 to i32
  %1377 = load i8, ptr %113, align 1
  %1378 = zext i8 %1377 to i32
  %1379 = icmp sle i32 %1376, %1378
  %1380 = zext i1 %1379 to i32
  %1381 = load i8, ptr @g_303, align 1
  %1382 = sext i8 %1381 to i32
  %1383 = xor i32 %1382, %1380
  %1384 = trunc i32 %1383 to i8
  store i8 %1384, ptr @g_303, align 1
  %1385 = sext i8 %1384 to i32
  %1386 = load ptr, ptr %9, align 8
  %1387 = load i8, ptr %1386, align 1
  %1388 = sext i8 %1387 to i32
  %1389 = xor i32 %1385, %1388
  %1390 = load i8, ptr %7, align 1
  %1391 = sext i8 %1390 to i32
  %1392 = icmp slt i32 %1389, %1391
  %1393 = zext i1 %1392 to i32
  %1394 = and i32 %1326, %1393
  %1395 = load i32, ptr %122, align 4
  %1396 = icmp ne i32 %1394, %1395
  %1397 = zext i1 %1396 to i32
  %1398 = trunc i32 %1397 to i8
  %1399 = load ptr, ptr @g_837, align 8
  %1400 = load ptr, ptr %1399, align 8
  %1401 = load i8, ptr %1400, align 1
  %1402 = call zeroext i8 @safe_mul_func_uint8_t_u_u(i8 noundef zeroext %1398, i8 noundef zeroext %1401)
  %1403 = zext i8 %1402 to i32
  %1404 = load ptr, ptr @g_2147, align 8
  %1405 = load i32, ptr %1404, align 4
  %1406 = call i32 @safe_sub_func_uint32_t_u_u(i32 noundef %1403, i32 noundef %1405)
  %1407 = load ptr, ptr %9, align 8
  %1408 = load i8, ptr %1407, align 1
  %1409 = sext i8 %1408 to i32
  %1410 = icmp ugt i32 %1406, %1409
  %1411 = zext i1 %1410 to i32
  %1412 = trunc i32 %1411 to i8
  %1413 = load i8, ptr %7, align 1
  %1414 = call zeroext i8 @safe_add_func_uint8_t_u_u(i8 noundef zeroext %1412, i8 noundef zeroext %1413)
  %1415 = zext i8 %1414 to i64
  %1416 = icmp eq i64 %1415, 1
  %1417 = zext i1 %1416 to i32
  %1418 = sext i32 %1417 to i64
  %1419 = icmp sgt i64 %1418, 151
  %1420 = zext i1 %1419 to i32
  %1421 = trunc i32 %1420 to i8
  %1422 = load ptr, ptr %9, align 8
  store i8 %1421, ptr %1422, align 1
  %1423 = load i8, ptr %7, align 1
  %1424 = sext i8 %1423 to i16
  %1425 = call ptr @func_52(i32 noundef %1315, i8 noundef signext %1421, i16 noundef zeroext %1424)
  %1426 = load ptr, ptr @g_1117, align 8
  %1427 = load ptr, ptr %1426, align 8
  %1428 = load ptr, ptr %1427, align 8
  store ptr %1425, ptr %1428, align 8
  br label %1429

1429:                                             ; preds = %1356
  %1430 = load i32, ptr %15, align 4
  %1431 = add i32 %1430, 1
  store i32 %1431, ptr %15, align 4
  br label %1310, !llvm.loop !59

1432:                                             ; preds = %1310
  br label %1433

1433:                                             ; preds = %1432
  %1434 = load i64, ptr @g_1990, align 8
  %1435 = add i64 %1434, 1
  store i64 %1435, ptr @g_1990, align 8
  br label %1140, !llvm.loop !60

1436:                                             ; preds = %1140
  %1437 = load i32, ptr %6, align 4
  store i32 %1437, ptr %5, align 4
  br label %1438

1438:                                             ; preds = %1436, %1014, %930, %293
  %1439 = load i32, ptr %5, align 4
  ret i32 %1439
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i8 @safe_div_func_uint8_t_u_u(i8 noundef zeroext %0, i8 noundef zeroext %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i8, align 1
  store i8 %0, ptr %3, align 1
  store i8 %1, ptr %4, align 1
  %5 = load i8, ptr %4, align 1
  %6 = zext i8 %5 to i32
  %7 = icmp eq i32 %6, 0
  br i1 %7, label %8, label %11

8:                                                ; preds = %2
  %9 = load i8, ptr %3, align 1
  %10 = zext i8 %9 to i32
  br label %17

11:                                               ; preds = %2
  %12 = load i8, ptr %3, align 1
  %13 = zext i8 %12 to i32
  %14 = load i8, ptr %4, align 1
  %15 = zext i8 %14 to i32
  %16 = sdiv i32 %13, %15
  br label %17

17:                                               ; preds = %11, %8
  %18 = phi i32 [ %10, %8 ], [ %16, %11 ]
  %19 = trunc i32 %18 to i8
  ret i8 %19
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i8 @safe_lshift_func_uint8_t_u_s(i8 noundef zeroext %0, i32 noundef %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i32, align 4
  store i8 %0, ptr %3, align 1
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = icmp slt i32 %5, 0
  br i1 %6, label %16, label %7

7:                                                ; preds = %2
  %8 = load i32, ptr %4, align 4
  %9 = icmp sge i32 %8, 32
  br i1 %9, label %16, label %10

10:                                               ; preds = %7
  %11 = load i8, ptr %3, align 1
  %12 = zext i8 %11 to i32
  %13 = load i32, ptr %4, align 4
  %14 = ashr i32 255, %13
  %15 = icmp sgt i32 %12, %14
  br i1 %15, label %16, label %19

16:                                               ; preds = %10, %7, %2
  %17 = load i8, ptr %3, align 1
  %18 = zext i8 %17 to i32
  br label %24

19:                                               ; preds = %10
  %20 = load i8, ptr %3, align 1
  %21 = zext i8 %20 to i32
  %22 = load i32, ptr %4, align 4
  %23 = shl i32 %21, %22
  br label %24

24:                                               ; preds = %19, %16
  %25 = phi i32 [ %18, %16 ], [ %23, %19 ]
  %26 = trunc i32 %25 to i8
  ret i8 %26
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i8 @safe_rshift_func_int8_t_s_s(i8 noundef signext %0, i32 noundef %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i32, align 4
  store i8 %0, ptr %3, align 1
  store i32 %1, ptr %4, align 4
  %5 = load i8, ptr %3, align 1
  %6 = sext i8 %5 to i32
  %7 = icmp slt i32 %6, 0
  br i1 %7, label %14, label %8

8:                                                ; preds = %2
  %9 = load i32, ptr %4, align 4
  %10 = icmp slt i32 %9, 0
  br i1 %10, label %14, label %11

11:                                               ; preds = %8
  %12 = load i32, ptr %4, align 4
  %13 = icmp sge i32 %12, 32
  br i1 %13, label %14, label %17

14:                                               ; preds = %11, %8, %2
  %15 = load i8, ptr %3, align 1
  %16 = sext i8 %15 to i32
  br label %22

17:                                               ; preds = %11
  %18 = load i8, ptr %3, align 1
  %19 = sext i8 %18 to i32
  %20 = load i32, ptr %4, align 4
  %21 = ashr i32 %19, %20
  br label %22

22:                                               ; preds = %17, %14
  %23 = phi i32 [ %16, %14 ], [ %21, %17 ]
  %24 = trunc i32 %23 to i8
  ret i8 %24
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i32 @safe_mod_func_uint32_t_u_u(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = icmp eq i32 %5, 0
  br i1 %6, label %7, label %9

7:                                                ; preds = %2
  %8 = load i32, ptr %3, align 4
  br label %13

9:                                                ; preds = %2
  %10 = load i32, ptr %3, align 4
  %11 = load i32, ptr %4, align 4
  %12 = urem i32 %10, %11
  br label %13

13:                                               ; preds = %9, %7
  %14 = phi i32 [ %8, %7 ], [ %12, %9 ]
  ret i32 %14
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i8 @safe_lshift_func_int8_t_s_u(i8 noundef signext %0, i32 noundef %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i32, align 4
  store i8 %0, ptr %3, align 1
  store i32 %1, ptr %4, align 4
  %5 = load i8, ptr %3, align 1
  %6 = sext i8 %5 to i32
  %7 = icmp slt i32 %6, 0
  br i1 %7, label %17, label %8

8:                                                ; preds = %2
  %9 = load i32, ptr %4, align 4
  %10 = icmp uge i32 %9, 32
  br i1 %10, label %17, label %11

11:                                               ; preds = %8
  %12 = load i8, ptr %3, align 1
  %13 = sext i8 %12 to i32
  %14 = load i32, ptr %4, align 4
  %15 = ashr i32 127, %14
  %16 = icmp sgt i32 %13, %15
  br i1 %16, label %17, label %20

17:                                               ; preds = %11, %8, %2
  %18 = load i8, ptr %3, align 1
  %19 = sext i8 %18 to i32
  br label %25

20:                                               ; preds = %11
  %21 = load i8, ptr %3, align 1
  %22 = sext i8 %21 to i32
  %23 = load i32, ptr %4, align 4
  %24 = shl i32 %22, %23
  br label %25

25:                                               ; preds = %20, %17
  %26 = phi i32 [ %19, %17 ], [ %24, %20 ]
  %27 = trunc i32 %26 to i8
  ret i8 %27
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i64 @safe_unary_minus_func_uint64_t_u(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, ptr %2, align 8
  %3 = load i64, ptr %2, align 8
  %4 = sub i64 0, %3
  ret i64 %4
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i16 @safe_sub_func_int16_t_s_s(i16 noundef signext %0, i16 noundef signext %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i16, align 2
  store i16 %0, ptr %3, align 2
  store i16 %1, ptr %4, align 2
  %5 = load i16, ptr %3, align 2
  %6 = sext i16 %5 to i32
  %7 = load i16, ptr %4, align 2
  %8 = sext i16 %7 to i32
  %9 = sub nsw i32 %6, %8
  %10 = trunc i32 %9 to i16
  ret i16 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i64 @safe_div_func_int64_t_s_s(i64 noundef %0, i64 noundef %1) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, ptr %3, align 8
  store i64 %1, ptr %4, align 8
  %5 = load i64, ptr %4, align 8
  %6 = icmp eq i64 %5, 0
  br i1 %6, label %13, label %7

7:                                                ; preds = %2
  %8 = load i64, ptr %3, align 8
  %9 = icmp eq i64 %8, -9223372036854775808
  br i1 %9, label %10, label %15

10:                                               ; preds = %7
  %11 = load i64, ptr %4, align 8
  %12 = icmp eq i64 %11, -1
  br i1 %12, label %13, label %15

13:                                               ; preds = %10, %2
  %14 = load i64, ptr %3, align 8
  br label %19

15:                                               ; preds = %10, %7
  %16 = load i64, ptr %3, align 8
  %17 = load i64, ptr %4, align 8
  %18 = sdiv i64 %16, %17
  br label %19

19:                                               ; preds = %15, %13
  %20 = phi i64 [ %14, %13 ], [ %18, %15 ]
  ret i64 %20
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i16 @safe_rshift_func_uint16_t_u_s(i16 noundef zeroext %0, i32 noundef %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i32, align 4
  store i16 %0, ptr %3, align 2
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = icmp slt i32 %5, 0
  br i1 %6, label %10, label %7

7:                                                ; preds = %2
  %8 = load i32, ptr %4, align 4
  %9 = icmp sge i32 %8, 32
  br i1 %9, label %10, label %13

10:                                               ; preds = %7, %2
  %11 = load i16, ptr %3, align 2
  %12 = zext i16 %11 to i32
  br label %18

13:                                               ; preds = %7
  %14 = load i16, ptr %3, align 2
  %15 = zext i16 %14 to i32
  %16 = load i32, ptr %4, align 4
  %17 = ashr i32 %15, %16
  br label %18

18:                                               ; preds = %13, %10
  %19 = phi i32 [ %12, %10 ], [ %17, %13 ]
  %20 = trunc i32 %19 to i16
  ret i16 %20
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i16 @safe_rshift_func_int16_t_s_u(i16 noundef signext %0, i32 noundef %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i32, align 4
  store i16 %0, ptr %3, align 2
  store i32 %1, ptr %4, align 4
  %5 = load i16, ptr %3, align 2
  %6 = sext i16 %5 to i32
  %7 = icmp slt i32 %6, 0
  br i1 %7, label %11, label %8

8:                                                ; preds = %2
  %9 = load i32, ptr %4, align 4
  %10 = icmp uge i32 %9, 32
  br i1 %10, label %11, label %14

11:                                               ; preds = %8, %2
  %12 = load i16, ptr %3, align 2
  %13 = sext i16 %12 to i32
  br label %19

14:                                               ; preds = %8
  %15 = load i16, ptr %3, align 2
  %16 = sext i16 %15 to i32
  %17 = load i32, ptr %4, align 4
  %18 = ashr i32 %16, %17
  br label %19

19:                                               ; preds = %14, %11
  %20 = phi i32 [ %13, %11 ], [ %18, %14 ]
  %21 = trunc i32 %20 to i16
  ret i16 %21
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i8 @safe_mod_func_int8_t_s_s(i8 noundef signext %0, i8 noundef signext %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i8, align 1
  store i8 %0, ptr %3, align 1
  store i8 %1, ptr %4, align 1
  %5 = load i8, ptr %4, align 1
  %6 = sext i8 %5 to i32
  %7 = icmp eq i32 %6, 0
  br i1 %7, label %16, label %8

8:                                                ; preds = %2
  %9 = load i8, ptr %3, align 1
  %10 = sext i8 %9 to i32
  %11 = icmp eq i32 %10, -128
  br i1 %11, label %12, label %19

12:                                               ; preds = %8
  %13 = load i8, ptr %4, align 1
  %14 = sext i8 %13 to i32
  %15 = icmp eq i32 %14, -1
  br i1 %15, label %16, label %19

16:                                               ; preds = %12, %2
  %17 = load i8, ptr %3, align 1
  %18 = sext i8 %17 to i32
  br label %25

19:                                               ; preds = %12, %8
  %20 = load i8, ptr %3, align 1
  %21 = sext i8 %20 to i32
  %22 = load i8, ptr %4, align 1
  %23 = sext i8 %22 to i32
  %24 = srem i32 %21, %23
  br label %25

25:                                               ; preds = %19, %16
  %26 = phi i32 [ %18, %16 ], [ %24, %19 ]
  %27 = trunc i32 %26 to i8
  ret i8 %27
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i32 @safe_add_func_uint32_t_u_u(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %3, align 4
  %6 = load i32, ptr %4, align 4
  %7 = add i32 %5, %6
  ret i32 %7
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i16 @safe_lshift_func_int16_t_s_u(i16 noundef signext %0, i32 noundef %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i32, align 4
  store i16 %0, ptr %3, align 2
  store i32 %1, ptr %4, align 4
  %5 = load i16, ptr %3, align 2
  %6 = sext i16 %5 to i32
  %7 = icmp slt i32 %6, 0
  br i1 %7, label %17, label %8

8:                                                ; preds = %2
  %9 = load i32, ptr %4, align 4
  %10 = icmp uge i32 %9, 32
  br i1 %10, label %17, label %11

11:                                               ; preds = %8
  %12 = load i16, ptr %3, align 2
  %13 = sext i16 %12 to i32
  %14 = load i32, ptr %4, align 4
  %15 = ashr i32 32767, %14
  %16 = icmp sgt i32 %13, %15
  br i1 %16, label %17, label %20

17:                                               ; preds = %11, %8, %2
  %18 = load i16, ptr %3, align 2
  %19 = sext i16 %18 to i32
  br label %25

20:                                               ; preds = %11
  %21 = load i16, ptr %3, align 2
  %22 = sext i16 %21 to i32
  %23 = load i32, ptr %4, align 4
  %24 = shl i32 %22, %23
  br label %25

25:                                               ; preds = %20, %17
  %26 = phi i32 [ %19, %17 ], [ %24, %20 ]
  %27 = trunc i32 %26 to i16
  ret i16 %27
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i32 @safe_div_func_uint32_t_u_u(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = icmp eq i32 %5, 0
  br i1 %6, label %7, label %9

7:                                                ; preds = %2
  %8 = load i32, ptr %3, align 4
  br label %13

9:                                                ; preds = %2
  %10 = load i32, ptr %3, align 4
  %11 = load i32, ptr %4, align 4
  %12 = udiv i32 %10, %11
  br label %13

13:                                               ; preds = %9, %7
  %14 = phi i32 [ %8, %7 ], [ %12, %9 ]
  ret i32 %14
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i8 @safe_add_func_int8_t_s_s(i8 noundef signext %0, i8 noundef signext %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i8, align 1
  store i8 %0, ptr %3, align 1
  store i8 %1, ptr %4, align 1
  %5 = load i8, ptr %3, align 1
  %6 = sext i8 %5 to i32
  %7 = load i8, ptr %4, align 1
  %8 = sext i8 %7 to i32
  %9 = add nsw i32 %6, %8
  %10 = trunc i32 %9 to i8
  ret i8 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i32 @safe_div_func_int32_t_s_s(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = icmp eq i32 %5, 0
  br i1 %6, label %13, label %7

7:                                                ; preds = %2
  %8 = load i32, ptr %3, align 4
  %9 = icmp eq i32 %8, -2147483648
  br i1 %9, label %10, label %15

10:                                               ; preds = %7
  %11 = load i32, ptr %4, align 4
  %12 = icmp eq i32 %11, -1
  br i1 %12, label %13, label %15

13:                                               ; preds = %10, %2
  %14 = load i32, ptr %3, align 4
  br label %19

15:                                               ; preds = %10, %7
  %16 = load i32, ptr %3, align 4
  %17 = load i32, ptr %4, align 4
  %18 = sdiv i32 %16, %17
  br label %19

19:                                               ; preds = %15, %13
  %20 = phi i32 [ %14, %13 ], [ %18, %15 ]
  ret i32 %20
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i16 @safe_div_func_uint16_t_u_u(i16 noundef zeroext %0, i16 noundef zeroext %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i16, align 2
  store i16 %0, ptr %3, align 2
  store i16 %1, ptr %4, align 2
  %5 = load i16, ptr %4, align 2
  %6 = zext i16 %5 to i32
  %7 = icmp eq i32 %6, 0
  br i1 %7, label %8, label %11

8:                                                ; preds = %2
  %9 = load i16, ptr %3, align 2
  %10 = zext i16 %9 to i32
  br label %17

11:                                               ; preds = %2
  %12 = load i16, ptr %3, align 2
  %13 = zext i16 %12 to i32
  %14 = load i16, ptr %4, align 2
  %15 = zext i16 %14 to i32
  %16 = sdiv i32 %13, %15
  br label %17

17:                                               ; preds = %11, %8
  %18 = phi i32 [ %10, %8 ], [ %16, %11 ]
  %19 = trunc i32 %18 to i16
  ret i16 %19
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i8 @safe_div_func_int8_t_s_s(i8 noundef signext %0, i8 noundef signext %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i8, align 1
  store i8 %0, ptr %3, align 1
  store i8 %1, ptr %4, align 1
  %5 = load i8, ptr %4, align 1
  %6 = sext i8 %5 to i32
  %7 = icmp eq i32 %6, 0
  br i1 %7, label %16, label %8

8:                                                ; preds = %2
  %9 = load i8, ptr %3, align 1
  %10 = sext i8 %9 to i32
  %11 = icmp eq i32 %10, -128
  br i1 %11, label %12, label %19

12:                                               ; preds = %8
  %13 = load i8, ptr %4, align 1
  %14 = sext i8 %13 to i32
  %15 = icmp eq i32 %14, -1
  br i1 %15, label %16, label %19

16:                                               ; preds = %12, %2
  %17 = load i8, ptr %3, align 1
  %18 = sext i8 %17 to i32
  br label %25

19:                                               ; preds = %12, %8
  %20 = load i8, ptr %3, align 1
  %21 = sext i8 %20 to i32
  %22 = load i8, ptr %4, align 1
  %23 = sext i8 %22 to i32
  %24 = sdiv i32 %21, %23
  br label %25

25:                                               ; preds = %19, %16
  %26 = phi i32 [ %18, %16 ], [ %24, %19 ]
  %27 = trunc i32 %26 to i8
  ret i8 %27
}

; Function Attrs: noinline nounwind optnone uwtable
define internal ptr @func_52(i32 noundef %0, i8 noundef signext %1, i16 noundef zeroext %2) #0 {
  %4 = alloca i32, align 4
  %5 = alloca i8, align 1
  %6 = alloca i16, align 2
  %7 = alloca ptr, align 8
  %8 = alloca ptr, align 8
  %9 = alloca ptr, align 8
  %10 = alloca ptr, align 8
  %11 = alloca [9 x i32], align 16
  %12 = alloca i32, align 4
  %13 = alloca ptr, align 8
  %14 = alloca i32, align 4
  %15 = alloca ptr, align 8
  %16 = alloca [7 x [2 x ptr]], align 16
  %17 = alloca ptr, align 8
  %18 = alloca i32, align 4
  %19 = alloca ptr, align 8
  %20 = alloca i64, align 8
  %21 = alloca ptr, align 8
  %22 = alloca ptr, align 8
  %23 = alloca i16, align 2
  %24 = alloca ptr, align 8
  %25 = alloca ptr, align 8
  %26 = alloca [8 x [8 x [4 x ptr]]], align 16
  %27 = alloca i32, align 4
  %28 = alloca i8, align 1
  %29 = alloca [1 x i8], align 1
  %30 = alloca ptr, align 8
  %31 = alloca i32, align 4
  %32 = alloca ptr, align 8
  %33 = alloca i32, align 4
  %34 = alloca i32, align 4
  %35 = alloca i32, align 4
  %36 = alloca i32, align 4
  %37 = alloca [10 x i8], align 1
  %38 = alloca ptr, align 8
  %39 = alloca i64, align 8
  %40 = alloca ptr, align 8
  %41 = alloca i32, align 4
  %42 = alloca [7 x [7 x ptr]], align 16
  %43 = alloca ptr, align 8
  %44 = alloca ptr, align 8
  %45 = alloca [5 x [1 x ptr]], align 16
  %46 = alloca [2 x i32], align 4
  %47 = alloca ptr, align 8
  %48 = alloca ptr, align 8
  %49 = alloca ptr, align 8
  %50 = alloca ptr, align 8
  %51 = alloca i32, align 4
  %52 = alloca ptr, align 8
  %53 = alloca i32, align 4
  %54 = alloca i32, align 4
  %55 = alloca ptr, align 8
  %56 = alloca ptr, align 8
  %57 = alloca ptr, align 8
  %58 = alloca ptr, align 8
  %59 = alloca [7 x ptr], align 16
  %60 = alloca i32, align 4
  %61 = alloca %union.U1, align 8
  %62 = alloca ptr, align 8
  %63 = alloca [7 x [4 x ptr]], align 16
  %64 = alloca [10 x ptr], align 16
  %65 = alloca ptr, align 8
  %66 = alloca [3 x ptr], align 16
  %67 = alloca ptr, align 8
  %68 = alloca i32, align 4
  %69 = alloca i32, align 4
  %70 = alloca ptr, align 8
  %71 = alloca i32, align 4
  %72 = alloca i32, align 4
  %73 = alloca i32, align 4
  %74 = alloca ptr, align 8
  %75 = alloca ptr, align 8
  %76 = alloca [10 x [2 x ptr]], align 16
  %77 = alloca i32, align 4
  %78 = alloca i32, align 4
  %79 = alloca [5 x [10 x i16]], align 16
  %80 = alloca ptr, align 8
  %81 = alloca ptr, align 8
  %82 = alloca i32, align 4
  %83 = alloca i32, align 4
  %84 = alloca ptr, align 8
  %85 = alloca ptr, align 8
  %86 = alloca ptr, align 8
  %87 = alloca i8, align 1
  %88 = alloca ptr, align 8
  %89 = alloca ptr, align 8
  %90 = alloca i32, align 4
  %91 = alloca i32, align 4
  %92 = alloca ptr, align 8
  %93 = alloca i32, align 4
  %94 = alloca ptr, align 8
  %95 = alloca ptr, align 8
  %96 = alloca ptr, align 8
  %97 = alloca i64, align 8
  %98 = alloca i16, align 2
  %99 = alloca ptr, align 8
  %100 = alloca ptr, align 8
  %101 = alloca ptr, align 8
  store i32 %0, ptr %4, align 4
  store i8 %1, ptr %5, align 1
  store i16 %2, ptr %6, align 2
  store ptr null, ptr %7, align 8
  store ptr @g_936, ptr %8, align 8
  store ptr %8, ptr %9, align 8
  store ptr @g_81, ptr %10, align 8
  call void @llvm.memset.p0.i64(ptr align 16 %11, i8 0, i64 36, i1 false)
  %102 = getelementptr inbounds [9 x i32], ptr %11, i32 0, i32 1
  store i32 -6, ptr %102, align 4
  %103 = getelementptr inbounds [9 x i32], ptr %11, i32 0, i32 2
  store i32 -6, ptr %103, align 8
  %104 = getelementptr inbounds [9 x i32], ptr %11, i32 0, i32 4
  store i32 -6, ptr %104, align 16
  %105 = getelementptr inbounds [9 x i32], ptr %11, i32 0, i32 5
  store i32 -6, ptr %105, align 4
  %106 = getelementptr inbounds [9 x i32], ptr %11, i32 0, i32 7
  store i32 -6, ptr %106, align 4
  %107 = getelementptr inbounds [9 x i32], ptr %11, i32 0, i32 8
  store i32 -6, ptr %107, align 16
  store i32 7, ptr %12, align 4
  store ptr @g_544, ptr %13, align 8
  store i32 1897731314, ptr %14, align 4
  store ptr null, ptr %15, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %16, ptr align 16 @__const.func_52.l_987, i64 112, i1 false)
  store ptr @g_147, ptr %17, align 8
  store i32 1, ptr %18, align 4
  store ptr @g_802, ptr %19, align 8
  store i64 5, ptr %20, align 8
  store ptr getelementptr inbounds ([7 x i32], ptr @g_880, i64 0, i64 2), ptr %21, align 8
  store ptr %21, ptr %22, align 8
  store i16 0, ptr %23, align 2
  store ptr @g_1040, ptr %24, align 8
  store ptr @g_802, ptr %25, align 8
  store ptr %25, ptr %26, align 8
  %108 = getelementptr inbounds ptr, ptr %26, i64 1
  store ptr %25, ptr %108, align 8
  %109 = getelementptr inbounds ptr, ptr %26, i64 2
  store ptr %25, ptr %109, align 8
  %110 = getelementptr inbounds ptr, ptr %26, i64 3
  store ptr %25, ptr %110, align 8
  %111 = getelementptr inbounds [4 x ptr], ptr %26, i64 1
  store ptr %25, ptr %111, align 8
  %112 = getelementptr inbounds ptr, ptr %111, i64 1
  store ptr %25, ptr %112, align 8
  %113 = getelementptr inbounds ptr, ptr %111, i64 2
  store ptr %25, ptr %113, align 8
  %114 = getelementptr inbounds ptr, ptr %111, i64 3
  store ptr %25, ptr %114, align 8
  %115 = getelementptr inbounds [4 x ptr], ptr %26, i64 2
  call void @llvm.memset.p0.i64(ptr align 16 %115, i8 0, i64 32, i1 false)
  %116 = getelementptr inbounds ptr, ptr %115, i64 1
  store ptr %25, ptr %116, align 8
  %117 = getelementptr inbounds ptr, ptr %115, i64 2
  %118 = getelementptr inbounds ptr, ptr %115, i64 3
  %119 = getelementptr inbounds [4 x ptr], ptr %26, i64 3
  store ptr %25, ptr %119, align 8
  %120 = getelementptr inbounds ptr, ptr %119, i64 1
  store ptr %25, ptr %120, align 8
  %121 = getelementptr inbounds ptr, ptr %119, i64 2
  store ptr %25, ptr %121, align 8
  %122 = getelementptr inbounds ptr, ptr %119, i64 3
  store ptr %25, ptr %122, align 8
  %123 = getelementptr inbounds [4 x ptr], ptr %26, i64 4
  store ptr %25, ptr %123, align 8
  %124 = getelementptr inbounds ptr, ptr %123, i64 1
  store ptr %25, ptr %124, align 8
  %125 = getelementptr inbounds ptr, ptr %123, i64 2
  store ptr null, ptr %125, align 8
  %126 = getelementptr inbounds ptr, ptr %123, i64 3
  store ptr null, ptr %126, align 8
  %127 = getelementptr inbounds [4 x ptr], ptr %26, i64 5
  store ptr null, ptr %127, align 8
  %128 = getelementptr inbounds ptr, ptr %127, i64 1
  store ptr %25, ptr %128, align 8
  %129 = getelementptr inbounds ptr, ptr %127, i64 2
  store ptr %25, ptr %129, align 8
  %130 = getelementptr inbounds ptr, ptr %127, i64 3
  store ptr %25, ptr %130, align 8
  %131 = getelementptr inbounds [4 x ptr], ptr %26, i64 6
  store ptr %25, ptr %131, align 8
  %132 = getelementptr inbounds ptr, ptr %131, i64 1
  store ptr %25, ptr %132, align 8
  %133 = getelementptr inbounds ptr, ptr %131, i64 2
  store ptr %25, ptr %133, align 8
  %134 = getelementptr inbounds ptr, ptr %131, i64 3
  store ptr %25, ptr %134, align 8
  %135 = getelementptr inbounds [4 x ptr], ptr %26, i64 7
  store ptr %25, ptr %135, align 8
  %136 = getelementptr inbounds ptr, ptr %135, i64 1
  store ptr %25, ptr %136, align 8
  %137 = getelementptr inbounds ptr, ptr %135, i64 2
  store ptr null, ptr %137, align 8
  %138 = getelementptr inbounds ptr, ptr %135, i64 3
  store ptr null, ptr %138, align 8
  %139 = getelementptr inbounds [8 x [4 x ptr]], ptr %26, i64 1
  store ptr null, ptr %139, align 8
  %140 = getelementptr inbounds ptr, ptr %139, i64 1
  store ptr %25, ptr %140, align 8
  %141 = getelementptr inbounds ptr, ptr %139, i64 2
  store ptr %25, ptr %141, align 8
  %142 = getelementptr inbounds ptr, ptr %139, i64 3
  store ptr %25, ptr %142, align 8
  %143 = getelementptr inbounds [4 x ptr], ptr %139, i64 1
  store ptr %25, ptr %143, align 8
  %144 = getelementptr inbounds ptr, ptr %143, i64 1
  store ptr %25, ptr %144, align 8
  %145 = getelementptr inbounds ptr, ptr %143, i64 2
  store ptr %25, ptr %145, align 8
  %146 = getelementptr inbounds ptr, ptr %143, i64 3
  store ptr null, ptr %146, align 8
  %147 = getelementptr inbounds [4 x ptr], ptr %139, i64 2
  store ptr null, ptr %147, align 8
  %148 = getelementptr inbounds ptr, ptr %147, i64 1
  store ptr %25, ptr %148, align 8
  %149 = getelementptr inbounds ptr, ptr %147, i64 2
  store ptr null, ptr %149, align 8
  %150 = getelementptr inbounds ptr, ptr %147, i64 3
  store ptr %25, ptr %150, align 8
  %151 = getelementptr inbounds [4 x ptr], ptr %139, i64 3
  store ptr %25, ptr %151, align 8
  %152 = getelementptr inbounds ptr, ptr %151, i64 1
  store ptr %25, ptr %152, align 8
  %153 = getelementptr inbounds ptr, ptr %151, i64 2
  store ptr %25, ptr %153, align 8
  %154 = getelementptr inbounds ptr, ptr %151, i64 3
  store ptr %25, ptr %154, align 8
  %155 = getelementptr inbounds [4 x ptr], ptr %139, i64 4
  store ptr %25, ptr %155, align 8
  %156 = getelementptr inbounds ptr, ptr %155, i64 1
  store ptr %25, ptr %156, align 8
  %157 = getelementptr inbounds ptr, ptr %155, i64 2
  store ptr %25, ptr %157, align 8
  %158 = getelementptr inbounds ptr, ptr %155, i64 3
  store ptr %25, ptr %158, align 8
  %159 = getelementptr inbounds [4 x ptr], ptr %139, i64 5
  call void @llvm.memset.p0.i64(ptr align 16 %159, i8 0, i64 32, i1 false)
  %160 = getelementptr inbounds ptr, ptr %159, i64 1
  store ptr %25, ptr %160, align 8
  %161 = getelementptr inbounds ptr, ptr %159, i64 2
  %162 = getelementptr inbounds ptr, ptr %159, i64 3
  %163 = getelementptr inbounds [4 x ptr], ptr %139, i64 6
  store ptr %25, ptr %163, align 8
  %164 = getelementptr inbounds ptr, ptr %163, i64 1
  store ptr %25, ptr %164, align 8
  %165 = getelementptr inbounds ptr, ptr %163, i64 2
  store ptr %25, ptr %165, align 8
  %166 = getelementptr inbounds ptr, ptr %163, i64 3
  store ptr %25, ptr %166, align 8
  %167 = getelementptr inbounds [4 x ptr], ptr %139, i64 7
  store ptr %25, ptr %167, align 8
  %168 = getelementptr inbounds ptr, ptr %167, i64 1
  store ptr %25, ptr %168, align 8
  %169 = getelementptr inbounds ptr, ptr %167, i64 2
  store ptr null, ptr %169, align 8
  %170 = getelementptr inbounds ptr, ptr %167, i64 3
  store ptr null, ptr %170, align 8
  %171 = getelementptr inbounds [8 x [4 x ptr]], ptr %26, i64 2
  store ptr null, ptr %171, align 8
  %172 = getelementptr inbounds ptr, ptr %171, i64 1
  store ptr %25, ptr %172, align 8
  %173 = getelementptr inbounds ptr, ptr %171, i64 2
  store ptr %25, ptr %173, align 8
  %174 = getelementptr inbounds ptr, ptr %171, i64 3
  store ptr %25, ptr %174, align 8
  %175 = getelementptr inbounds [4 x ptr], ptr %171, i64 1
  store ptr %25, ptr %175, align 8
  %176 = getelementptr inbounds ptr, ptr %175, i64 1
  store ptr %25, ptr %176, align 8
  %177 = getelementptr inbounds ptr, ptr %175, i64 2
  store ptr %25, ptr %177, align 8
  %178 = getelementptr inbounds ptr, ptr %175, i64 3
  store ptr %25, ptr %178, align 8
  %179 = getelementptr inbounds [4 x ptr], ptr %171, i64 2
  store ptr null, ptr %179, align 8
  %180 = getelementptr inbounds ptr, ptr %179, i64 1
  store ptr %25, ptr %180, align 8
  %181 = getelementptr inbounds ptr, ptr %179, i64 2
  store ptr %25, ptr %181, align 8
  %182 = getelementptr inbounds ptr, ptr %179, i64 3
  store ptr %25, ptr %182, align 8
  %183 = getelementptr inbounds [4 x ptr], ptr %171, i64 3
  store ptr %25, ptr %183, align 8
  %184 = getelementptr inbounds ptr, ptr %183, i64 1
  store ptr %25, ptr %184, align 8
  %185 = getelementptr inbounds ptr, ptr %183, i64 2
  store ptr %25, ptr %185, align 8
  %186 = getelementptr inbounds ptr, ptr %183, i64 3
  store ptr %25, ptr %186, align 8
  %187 = getelementptr inbounds [4 x ptr], ptr %171, i64 4
  store ptr %25, ptr %187, align 8
  %188 = getelementptr inbounds ptr, ptr %187, i64 1
  store ptr %25, ptr %188, align 8
  %189 = getelementptr inbounds ptr, ptr %187, i64 2
  store ptr %25, ptr %189, align 8
  %190 = getelementptr inbounds ptr, ptr %187, i64 3
  store ptr %25, ptr %190, align 8
  %191 = getelementptr inbounds [4 x ptr], ptr %171, i64 5
  store ptr %25, ptr %191, align 8
  %192 = getelementptr inbounds ptr, ptr %191, i64 1
  store ptr null, ptr %192, align 8
  %193 = getelementptr inbounds ptr, ptr %191, i64 2
  store ptr %25, ptr %193, align 8
  %194 = getelementptr inbounds ptr, ptr %191, i64 3
  store ptr %25, ptr %194, align 8
  %195 = getelementptr inbounds [4 x ptr], ptr %171, i64 6
  store ptr null, ptr %195, align 8
  %196 = getelementptr inbounds ptr, ptr %195, i64 1
  store ptr %25, ptr %196, align 8
  %197 = getelementptr inbounds ptr, ptr %195, i64 2
  store ptr %25, ptr %197, align 8
  %198 = getelementptr inbounds ptr, ptr %195, i64 3
  store ptr %25, ptr %198, align 8
  %199 = getelementptr inbounds [4 x ptr], ptr %171, i64 7
  store ptr %25, ptr %199, align 8
  %200 = getelementptr inbounds ptr, ptr %199, i64 1
  store ptr %25, ptr %200, align 8
  %201 = getelementptr inbounds ptr, ptr %199, i64 2
  store ptr null, ptr %201, align 8
  %202 = getelementptr inbounds ptr, ptr %199, i64 3
  store ptr %25, ptr %202, align 8
  %203 = getelementptr inbounds [8 x [4 x ptr]], ptr %26, i64 3
  store ptr %25, ptr %203, align 8
  %204 = getelementptr inbounds ptr, ptr %203, i64 1
  store ptr null, ptr %204, align 8
  %205 = getelementptr inbounds ptr, ptr %203, i64 2
  store ptr %25, ptr %205, align 8
  %206 = getelementptr inbounds ptr, ptr %203, i64 3
  store ptr %25, ptr %206, align 8
  %207 = getelementptr inbounds [4 x ptr], ptr %203, i64 1
  store ptr %25, ptr %207, align 8
  %208 = getelementptr inbounds ptr, ptr %207, i64 1
  store ptr %25, ptr %208, align 8
  %209 = getelementptr inbounds ptr, ptr %207, i64 2
  store ptr %25, ptr %209, align 8
  %210 = getelementptr inbounds ptr, ptr %207, i64 3
  store ptr %25, ptr %210, align 8
  %211 = getelementptr inbounds [4 x ptr], ptr %203, i64 2
  store ptr %25, ptr %211, align 8
  %212 = getelementptr inbounds ptr, ptr %211, i64 1
  store ptr %25, ptr %212, align 8
  %213 = getelementptr inbounds ptr, ptr %211, i64 2
  store ptr %25, ptr %213, align 8
  %214 = getelementptr inbounds ptr, ptr %211, i64 3
  store ptr %25, ptr %214, align 8
  %215 = getelementptr inbounds [4 x ptr], ptr %203, i64 3
  store ptr %25, ptr %215, align 8
  %216 = getelementptr inbounds ptr, ptr %215, i64 1
  store ptr %25, ptr %216, align 8
  %217 = getelementptr inbounds ptr, ptr %215, i64 2
  store ptr null, ptr %217, align 8
  %218 = getelementptr inbounds ptr, ptr %215, i64 3
  store ptr %25, ptr %218, align 8
  %219 = getelementptr inbounds [4 x ptr], ptr %203, i64 4
  store ptr %25, ptr %219, align 8
  %220 = getelementptr inbounds ptr, ptr %219, i64 1
  store ptr null, ptr %220, align 8
  %221 = getelementptr inbounds ptr, ptr %219, i64 2
  store ptr %25, ptr %221, align 8
  %222 = getelementptr inbounds ptr, ptr %219, i64 3
  store ptr %25, ptr %222, align 8
  %223 = getelementptr inbounds [4 x ptr], ptr %203, i64 5
  store ptr null, ptr %223, align 8
  %224 = getelementptr inbounds ptr, ptr %223, i64 1
  store ptr %25, ptr %224, align 8
  %225 = getelementptr inbounds ptr, ptr %223, i64 2
  store ptr %25, ptr %225, align 8
  %226 = getelementptr inbounds ptr, ptr %223, i64 3
  store ptr %25, ptr %226, align 8
  %227 = getelementptr inbounds [4 x ptr], ptr %203, i64 6
  store ptr %25, ptr %227, align 8
  %228 = getelementptr inbounds ptr, ptr %227, i64 1
  store ptr %25, ptr %228, align 8
  %229 = getelementptr inbounds ptr, ptr %227, i64 2
  store ptr %25, ptr %229, align 8
  %230 = getelementptr inbounds ptr, ptr %227, i64 3
  store ptr %25, ptr %230, align 8
  %231 = getelementptr inbounds [4 x ptr], ptr %203, i64 7
  store ptr %25, ptr %231, align 8
  %232 = getelementptr inbounds ptr, ptr %231, i64 1
  store ptr %25, ptr %232, align 8
  %233 = getelementptr inbounds ptr, ptr %231, i64 2
  store ptr %25, ptr %233, align 8
  %234 = getelementptr inbounds ptr, ptr %231, i64 3
  store ptr %25, ptr %234, align 8
  %235 = getelementptr inbounds [8 x [4 x ptr]], ptr %26, i64 4
  store ptr %25, ptr %235, align 8
  %236 = getelementptr inbounds ptr, ptr %235, i64 1
  store ptr null, ptr %236, align 8
  %237 = getelementptr inbounds ptr, ptr %235, i64 2
  store ptr %25, ptr %237, align 8
  %238 = getelementptr inbounds ptr, ptr %235, i64 3
  store ptr %25, ptr %238, align 8
  %239 = getelementptr inbounds [4 x ptr], ptr %235, i64 1
  store ptr null, ptr %239, align 8
  %240 = getelementptr inbounds ptr, ptr %239, i64 1
  store ptr %25, ptr %240, align 8
  %241 = getelementptr inbounds ptr, ptr %239, i64 2
  store ptr %25, ptr %241, align 8
  %242 = getelementptr inbounds ptr, ptr %239, i64 3
  store ptr %25, ptr %242, align 8
  %243 = getelementptr inbounds [4 x ptr], ptr %235, i64 2
  store ptr %25, ptr %243, align 8
  %244 = getelementptr inbounds ptr, ptr %243, i64 1
  store ptr %25, ptr %244, align 8
  %245 = getelementptr inbounds ptr, ptr %243, i64 2
  store ptr null, ptr %245, align 8
  %246 = getelementptr inbounds ptr, ptr %243, i64 3
  store ptr %25, ptr %246, align 8
  %247 = getelementptr inbounds [4 x ptr], ptr %235, i64 3
  store ptr %25, ptr %247, align 8
  %248 = getelementptr inbounds ptr, ptr %247, i64 1
  store ptr null, ptr %248, align 8
  %249 = getelementptr inbounds ptr, ptr %247, i64 2
  store ptr %25, ptr %249, align 8
  %250 = getelementptr inbounds ptr, ptr %247, i64 3
  store ptr %25, ptr %250, align 8
  %251 = getelementptr inbounds [4 x ptr], ptr %235, i64 4
  store ptr %25, ptr %251, align 8
  %252 = getelementptr inbounds ptr, ptr %251, i64 1
  store ptr %25, ptr %252, align 8
  %253 = getelementptr inbounds ptr, ptr %251, i64 2
  store ptr %25, ptr %253, align 8
  %254 = getelementptr inbounds ptr, ptr %251, i64 3
  store ptr %25, ptr %254, align 8
  %255 = getelementptr inbounds [4 x ptr], ptr %235, i64 5
  store ptr %25, ptr %255, align 8
  %256 = getelementptr inbounds ptr, ptr %255, i64 1
  store ptr %25, ptr %256, align 8
  %257 = getelementptr inbounds ptr, ptr %255, i64 2
  store ptr %25, ptr %257, align 8
  %258 = getelementptr inbounds ptr, ptr %255, i64 3
  store ptr %25, ptr %258, align 8
  %259 = getelementptr inbounds [4 x ptr], ptr %235, i64 6
  store ptr %25, ptr %259, align 8
  %260 = getelementptr inbounds ptr, ptr %259, i64 1
  store ptr %25, ptr %260, align 8
  %261 = getelementptr inbounds ptr, ptr %259, i64 2
  store ptr null, ptr %261, align 8
  %262 = getelementptr inbounds ptr, ptr %259, i64 3
  store ptr %25, ptr %262, align 8
  %263 = getelementptr inbounds [4 x ptr], ptr %235, i64 7
  store ptr %25, ptr %263, align 8
  %264 = getelementptr inbounds ptr, ptr %263, i64 1
  store ptr null, ptr %264, align 8
  %265 = getelementptr inbounds ptr, ptr %263, i64 2
  store ptr %25, ptr %265, align 8
  %266 = getelementptr inbounds ptr, ptr %263, i64 3
  store ptr %25, ptr %266, align 8
  %267 = getelementptr inbounds [8 x [4 x ptr]], ptr %26, i64 5
  store ptr null, ptr %267, align 8
  %268 = getelementptr inbounds ptr, ptr %267, i64 1
  store ptr %25, ptr %268, align 8
  %269 = getelementptr inbounds ptr, ptr %267, i64 2
  store ptr %25, ptr %269, align 8
  %270 = getelementptr inbounds ptr, ptr %267, i64 3
  store ptr %25, ptr %270, align 8
  %271 = getelementptr inbounds [4 x ptr], ptr %267, i64 1
  store ptr %25, ptr %271, align 8
  %272 = getelementptr inbounds ptr, ptr %271, i64 1
  store ptr %25, ptr %272, align 8
  %273 = getelementptr inbounds ptr, ptr %271, i64 2
  store ptr %25, ptr %273, align 8
  %274 = getelementptr inbounds ptr, ptr %271, i64 3
  store ptr %25, ptr %274, align 8
  %275 = getelementptr inbounds [4 x ptr], ptr %267, i64 2
  store ptr %25, ptr %275, align 8
  %276 = getelementptr inbounds ptr, ptr %275, i64 1
  store ptr %25, ptr %276, align 8
  %277 = getelementptr inbounds ptr, ptr %275, i64 2
  store ptr %25, ptr %277, align 8
  %278 = getelementptr inbounds ptr, ptr %275, i64 3
  store ptr %25, ptr %278, align 8
  %279 = getelementptr inbounds [4 x ptr], ptr %267, i64 3
  store ptr %25, ptr %279, align 8
  %280 = getelementptr inbounds ptr, ptr %279, i64 1
  store ptr null, ptr %280, align 8
  %281 = getelementptr inbounds ptr, ptr %279, i64 2
  store ptr %25, ptr %281, align 8
  %282 = getelementptr inbounds ptr, ptr %279, i64 3
  store ptr %25, ptr %282, align 8
  %283 = getelementptr inbounds [4 x ptr], ptr %267, i64 4
  store ptr null, ptr %283, align 8
  %284 = getelementptr inbounds ptr, ptr %283, i64 1
  store ptr %25, ptr %284, align 8
  %285 = getelementptr inbounds ptr, ptr %283, i64 2
  store ptr %25, ptr %285, align 8
  %286 = getelementptr inbounds ptr, ptr %283, i64 3
  store ptr %25, ptr %286, align 8
  %287 = getelementptr inbounds [4 x ptr], ptr %267, i64 5
  store ptr %25, ptr %287, align 8
  %288 = getelementptr inbounds ptr, ptr %287, i64 1
  store ptr %25, ptr %288, align 8
  %289 = getelementptr inbounds ptr, ptr %287, i64 2
  store ptr null, ptr %289, align 8
  %290 = getelementptr inbounds ptr, ptr %287, i64 3
  store ptr %25, ptr %290, align 8
  %291 = getelementptr inbounds [4 x ptr], ptr %267, i64 6
  store ptr %25, ptr %291, align 8
  %292 = getelementptr inbounds ptr, ptr %291, i64 1
  store ptr null, ptr %292, align 8
  %293 = getelementptr inbounds ptr, ptr %291, i64 2
  store ptr %25, ptr %293, align 8
  %294 = getelementptr inbounds ptr, ptr %291, i64 3
  store ptr %25, ptr %294, align 8
  %295 = getelementptr inbounds [4 x ptr], ptr %267, i64 7
  store ptr %25, ptr %295, align 8
  %296 = getelementptr inbounds ptr, ptr %295, i64 1
  store ptr %25, ptr %296, align 8
  %297 = getelementptr inbounds ptr, ptr %295, i64 2
  store ptr %25, ptr %297, align 8
  %298 = getelementptr inbounds ptr, ptr %295, i64 3
  store ptr %25, ptr %298, align 8
  %299 = getelementptr inbounds [8 x [4 x ptr]], ptr %26, i64 6
  store ptr %25, ptr %299, align 8
  %300 = getelementptr inbounds ptr, ptr %299, i64 1
  store ptr %25, ptr %300, align 8
  %301 = getelementptr inbounds ptr, ptr %299, i64 2
  store ptr %25, ptr %301, align 8
  %302 = getelementptr inbounds ptr, ptr %299, i64 3
  store ptr %25, ptr %302, align 8
  %303 = getelementptr inbounds [4 x ptr], ptr %299, i64 1
  store ptr %25, ptr %303, align 8
  %304 = getelementptr inbounds ptr, ptr %303, i64 1
  store ptr %25, ptr %304, align 8
  %305 = getelementptr inbounds ptr, ptr %303, i64 2
  store ptr null, ptr %305, align 8
  %306 = getelementptr inbounds ptr, ptr %303, i64 3
  store ptr %25, ptr %306, align 8
  %307 = getelementptr inbounds [4 x ptr], ptr %299, i64 2
  store ptr %25, ptr %307, align 8
  %308 = getelementptr inbounds ptr, ptr %307, i64 1
  store ptr null, ptr %308, align 8
  %309 = getelementptr inbounds ptr, ptr %307, i64 2
  store ptr %25, ptr %309, align 8
  %310 = getelementptr inbounds ptr, ptr %307, i64 3
  store ptr %25, ptr %310, align 8
  %311 = getelementptr inbounds [4 x ptr], ptr %299, i64 3
  store ptr null, ptr %311, align 8
  %312 = getelementptr inbounds ptr, ptr %311, i64 1
  store ptr %25, ptr %312, align 8
  %313 = getelementptr inbounds ptr, ptr %311, i64 2
  store ptr %25, ptr %313, align 8
  %314 = getelementptr inbounds ptr, ptr %311, i64 3
  store ptr %25, ptr %314, align 8
  %315 = getelementptr inbounds [4 x ptr], ptr %299, i64 4
  store ptr %25, ptr %315, align 8
  %316 = getelementptr inbounds ptr, ptr %315, i64 1
  store ptr %25, ptr %316, align 8
  %317 = getelementptr inbounds ptr, ptr %315, i64 2
  store ptr %25, ptr %317, align 8
  %318 = getelementptr inbounds ptr, ptr %315, i64 3
  store ptr %25, ptr %318, align 8
  %319 = getelementptr inbounds [4 x ptr], ptr %299, i64 5
  store ptr %25, ptr %319, align 8
  %320 = getelementptr inbounds ptr, ptr %319, i64 1
  store ptr %25, ptr %320, align 8
  %321 = getelementptr inbounds ptr, ptr %319, i64 2
  store ptr %25, ptr %321, align 8
  %322 = getelementptr inbounds ptr, ptr %319, i64 3
  store ptr %25, ptr %322, align 8
  %323 = getelementptr inbounds [4 x ptr], ptr %299, i64 6
  store ptr %25, ptr %323, align 8
  %324 = getelementptr inbounds ptr, ptr %323, i64 1
  store ptr null, ptr %324, align 8
  %325 = getelementptr inbounds ptr, ptr %323, i64 2
  store ptr %25, ptr %325, align 8
  %326 = getelementptr inbounds ptr, ptr %323, i64 3
  store ptr %25, ptr %326, align 8
  %327 = getelementptr inbounds [4 x ptr], ptr %299, i64 7
  store ptr null, ptr %327, align 8
  %328 = getelementptr inbounds ptr, ptr %327, i64 1
  store ptr %25, ptr %328, align 8
  %329 = getelementptr inbounds ptr, ptr %327, i64 2
  store ptr %25, ptr %329, align 8
  %330 = getelementptr inbounds ptr, ptr %327, i64 3
  store ptr %25, ptr %330, align 8
  %331 = getelementptr inbounds [8 x [4 x ptr]], ptr %26, i64 7
  store ptr %25, ptr %331, align 8
  %332 = getelementptr inbounds ptr, ptr %331, i64 1
  store ptr %25, ptr %332, align 8
  %333 = getelementptr inbounds ptr, ptr %331, i64 2
  store ptr null, ptr %333, align 8
  %334 = getelementptr inbounds ptr, ptr %331, i64 3
  store ptr %25, ptr %334, align 8
  %335 = getelementptr inbounds [4 x ptr], ptr %331, i64 1
  store ptr %25, ptr %335, align 8
  %336 = getelementptr inbounds ptr, ptr %335, i64 1
  store ptr null, ptr %336, align 8
  %337 = getelementptr inbounds ptr, ptr %335, i64 2
  store ptr %25, ptr %337, align 8
  %338 = getelementptr inbounds ptr, ptr %335, i64 3
  store ptr %25, ptr %338, align 8
  %339 = getelementptr inbounds [4 x ptr], ptr %331, i64 2
  store ptr %25, ptr %339, align 8
  %340 = getelementptr inbounds ptr, ptr %339, i64 1
  store ptr %25, ptr %340, align 8
  %341 = getelementptr inbounds ptr, ptr %339, i64 2
  store ptr %25, ptr %341, align 8
  %342 = getelementptr inbounds ptr, ptr %339, i64 3
  store ptr %25, ptr %342, align 8
  %343 = getelementptr inbounds [4 x ptr], ptr %331, i64 3
  store ptr %25, ptr %343, align 8
  %344 = getelementptr inbounds ptr, ptr %343, i64 1
  store ptr %25, ptr %344, align 8
  %345 = getelementptr inbounds ptr, ptr %343, i64 2
  store ptr %25, ptr %345, align 8
  %346 = getelementptr inbounds ptr, ptr %343, i64 3
  store ptr %25, ptr %346, align 8
  %347 = getelementptr inbounds [4 x ptr], ptr %331, i64 4
  store ptr %25, ptr %347, align 8
  %348 = getelementptr inbounds ptr, ptr %347, i64 1
  store ptr %25, ptr %348, align 8
  %349 = getelementptr inbounds ptr, ptr %347, i64 2
  store ptr null, ptr %349, align 8
  %350 = getelementptr inbounds ptr, ptr %347, i64 3
  store ptr %25, ptr %350, align 8
  %351 = getelementptr inbounds [4 x ptr], ptr %331, i64 5
  store ptr %25, ptr %351, align 8
  %352 = getelementptr inbounds ptr, ptr %351, i64 1
  store ptr null, ptr %352, align 8
  %353 = getelementptr inbounds ptr, ptr %351, i64 2
  store ptr %25, ptr %353, align 8
  %354 = getelementptr inbounds ptr, ptr %351, i64 3
  store ptr %25, ptr %354, align 8
  %355 = getelementptr inbounds [4 x ptr], ptr %331, i64 6
  store ptr null, ptr %355, align 8
  %356 = getelementptr inbounds ptr, ptr %355, i64 1
  store ptr %25, ptr %356, align 8
  %357 = getelementptr inbounds ptr, ptr %355, i64 2
  store ptr %25, ptr %357, align 8
  %358 = getelementptr inbounds ptr, ptr %355, i64 3
  store ptr %25, ptr %358, align 8
  %359 = getelementptr inbounds [4 x ptr], ptr %331, i64 7
  store ptr %25, ptr %359, align 8
  %360 = getelementptr inbounds ptr, ptr %359, i64 1
  store ptr %25, ptr %360, align 8
  %361 = getelementptr inbounds ptr, ptr %359, i64 2
  store ptr %25, ptr %361, align 8
  %362 = getelementptr inbounds ptr, ptr %359, i64 3
  store ptr %25, ptr %362, align 8
  store i32 -445696314, ptr %27, align 4
  store i8 0, ptr %28, align 1
  store ptr @g_1330, ptr %30, align 8
  store i32 1105378095, ptr %31, align 4
  store ptr @g_147, ptr %32, align 8
  store i32 0, ptr %33, align 4
  br label %363

363:                                              ; preds = %370, %3
  %364 = load i32, ptr %33, align 4
  %365 = icmp slt i32 %364, 1
  br i1 %365, label %366, label %373

366:                                              ; preds = %363
  %367 = load i32, ptr %33, align 4
  %368 = sext i32 %367 to i64
  %369 = getelementptr inbounds [1 x i8], ptr %29, i64 0, i64 %368
  store i8 87, ptr %369, align 1
  br label %370

370:                                              ; preds = %366
  %371 = load i32, ptr %33, align 4
  %372 = add nsw i32 %371, 1
  store i32 %372, ptr %33, align 4
  br label %363, !llvm.loop !61

373:                                              ; preds = %363
  %374 = load volatile ptr, ptr @g_90, align 8
  %375 = load ptr, ptr %374, align 8
  %376 = load i32, ptr %375, align 4
  %377 = icmp ne i32 %376, 0
  br i1 %377, label %378, label %565

378:                                              ; preds = %373
  store i32 -2017582227, ptr %36, align 4
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %37, ptr align 1 @__const.func_52.l_959, i64 10, i1 false)
  store ptr %7, ptr %38, align 8
  store i64 -1, ptr %39, align 8
  store ptr getelementptr inbounds ([3 x i32], ptr @g_147, i64 0, i64 2), ptr %40, align 8
  store i16 0, ptr %6, align 2
  br label %379

379:                                              ; preds = %494, %378
  %380 = load i16, ptr %6, align 2
  %381 = zext i16 %380 to i32
  %382 = icmp ne i32 %381, 10
  br i1 %382, label %383, label %499

383:                                              ; preds = %379
  store i16 0, ptr @g_235, align 2
  br label %384

384:                                              ; preds = %395, %383
  %385 = load i16, ptr @g_235, align 2
  %386 = sext i16 %385 to i32
  %387 = icmp sle i32 %386, 1
  br i1 %387, label %388, label %400

388:                                              ; preds = %384
  %389 = load ptr, ptr %7, align 8
  %390 = load ptr, ptr @g_802, align 8
  store ptr %389, ptr %390, align 8
  %391 = load i16, ptr %6, align 2
  %392 = icmp ne i16 %391, 0
  br i1 %392, label %393, label %394

393:                                              ; preds = %388
  br label %583

394:                                              ; preds = %388
  br label %395

395:                                              ; preds = %394
  %396 = load i16, ptr @g_235, align 2
  %397 = sext i16 %396 to i32
  %398 = add nsw i32 %397, 1
  %399 = trunc i32 %398 to i16
  store i16 %399, ptr @g_235, align 2
  br label %384, !llvm.loop !62

400:                                              ; preds = %384
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 @g_924, ptr align 1 @g_923, i64 13, i1 true)
  store i8 0, ptr @g_237, align 1
  br label %401

401:                                              ; preds = %490, %400
  %402 = load i8, ptr @g_237, align 1
  %403 = sext i8 %402 to i32
  %404 = icmp sge i32 %403, 21
  br i1 %404, label %405, label %493

405:                                              ; preds = %401
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %42, ptr align 16 @__const.func_52.l_931, i64 392, i1 false)
  store ptr %8, ptr %43, align 8
  store ptr @g_355, ptr %44, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %45, ptr align 16 @__const.func_52.l_941, i64 40, i1 false)
  store ptr @g_235, ptr %47, align 8
  store ptr %47, ptr %48, align 8
  store ptr null, ptr %49, align 8
  store ptr %10, ptr %50, align 8
  store i32 1, ptr %51, align 4
  store ptr getelementptr inbounds ([3 x i32], ptr @g_147, i64 0, i64 2), ptr %52, align 8
  store i32 0, ptr %53, align 4
  br label %406

406:                                              ; preds = %413, %405
  %407 = load i32, ptr %53, align 4
  %408 = icmp slt i32 %407, 2
  br i1 %408, label %409, label %416

409:                                              ; preds = %406
  %410 = load i32, ptr %53, align 4
  %411 = sext i32 %410 to i64
  %412 = getelementptr inbounds [2 x i32], ptr %46, i64 0, i64 %411
  store i32 2, ptr %412, align 4
  br label %413

413:                                              ; preds = %409
  %414 = load i32, ptr %53, align 4
  %415 = add nsw i32 %414, 1
  store i32 %415, ptr %53, align 4
  br label %406, !llvm.loop !63

416:                                              ; preds = %406
  %417 = load volatile i104, ptr @g_671, align 1
  %418 = shl i104 %417, 59
  %419 = ashr i104 %418, 79
  %420 = trunc i104 %419 to i32
  %421 = trunc i32 %420 to i16
  %422 = call signext i16 @safe_add_func_int16_t_s_s(i16 noundef signext 16078, i16 noundef signext %421)
  %423 = sext i16 %422 to i32
  %424 = load i32, ptr getelementptr inbounds ([7 x i32], ptr @g_880, i64 0, i64 3), align 4
  %425 = trunc i32 %424 to i8
  store i8 %425, ptr %5, align 1
  %426 = sext i8 %425 to i32
  %427 = load ptr, ptr %9, align 8
  %428 = load ptr, ptr %43, align 8
  %429 = icmp eq ptr %427, %428
  %430 = zext i1 %429 to i32
  %431 = sext i32 %430 to i64
  %432 = load ptr, ptr %44, align 8
  store i64 %431, ptr %432, align 8
  %433 = load i16, ptr %6, align 2
  %434 = zext i16 %433 to i32
  %435 = getelementptr inbounds [2 x i32], ptr %46, i64 0, i64 0
  store i32 %434, ptr %435, align 4
  %436 = trunc i32 %434 to i16
  %437 = load ptr, ptr %48, align 8
  store ptr null, ptr %437, align 8
  %438 = load ptr, ptr %10, align 8
  %439 = load ptr, ptr %50, align 8
  store ptr %438, ptr %439, align 8
  %440 = icmp eq ptr null, %438
  %441 = zext i1 %440 to i32
  %442 = trunc i32 %441 to i16
  %443 = call signext i16 @safe_add_func_int16_t_s_s(i16 noundef signext %442, i16 noundef signext 5)
  %444 = trunc i16 %443 to i8
  %445 = call zeroext i8 @safe_lshift_func_uint8_t_u_s(i8 noundef zeroext %444, i32 noundef 0)
  %446 = zext i8 %445 to i16
  %447 = load i64, ptr getelementptr inbounds ([2 x i64], ptr getelementptr inbounds ([9 x [4 x [2 x i64]]], ptr @g_160, i64 0, i64 8), i64 0, i64 1), align 8
  %448 = trunc i64 %447 to i32
  %449 = call zeroext i16 @safe_rshift_func_uint16_t_u_s(i16 noundef zeroext %446, i32 noundef %448)
  %450 = zext i16 %449 to i64
  %451 = and i64 %450, 0
  %452 = icmp slt i64 %451, 3930356201166634023
  br i1 %452, label %453, label %458

453:                                              ; preds = %416
  %454 = load ptr, ptr @g_544, align 8
  %455 = load i8, ptr %454, align 1
  %456 = zext i8 %455 to i32
  %457 = icmp ne i32 %456, 0
  br label %458

458:                                              ; preds = %453, %416
  %459 = phi i1 [ false, %416 ], [ %457, %453 ]
  %460 = zext i1 %459 to i32
  store i32 %460, ptr %36, align 4
  %461 = call zeroext i16 @safe_rshift_func_uint16_t_u_u(i16 noundef zeroext %436, i32 noundef %460)
  %462 = zext i16 %461 to i64
  %463 = icmp uge i64 %431, %462
  %464 = zext i1 %463 to i32
  %465 = sext i32 %464 to i64
  %466 = icmp sle i64 %465, 110
  %467 = zext i1 %466 to i32
  %468 = trunc i32 %467 to i8
  %469 = load ptr, ptr @g_837, align 8
  %470 = load ptr, ptr %469, align 8
  %471 = load i8, ptr %470, align 1
  %472 = zext i8 %471 to i32
  %473 = call zeroext i8 @safe_lshift_func_uint8_t_u_u(i8 noundef zeroext %468, i32 noundef %472)
  %474 = zext i8 %473 to i32
  %475 = icmp ne i32 %426, %474
  %476 = zext i1 %475 to i32
  %477 = or i32 %423, %476
  %478 = sext i32 %477 to i64
  %479 = or i64 %478, 7610
  %480 = load i32, ptr %4, align 4
  %481 = zext i32 %480 to i64
  %482 = icmp ne i64 %479, %481
  %483 = zext i1 %482 to i32
  %484 = load i32, ptr %51, align 4
  %485 = icmp sgt i32 %483, %484
  %486 = zext i1 %485 to i32
  %487 = load ptr, ptr %52, align 8
  %488 = load i32, ptr %487, align 4
  %489 = and i32 %488, %486
  store i32 %489, ptr %487, align 4
  br label %490

490:                                              ; preds = %458
  %491 = load i8, ptr @g_237, align 1
  %492 = add i8 %491, 1
  store i8 %492, ptr @g_237, align 1
  br label %401, !llvm.loop !64

493:                                              ; preds = %401
  br label %494

494:                                              ; preds = %493
  %495 = load i16, ptr %6, align 2
  %496 = zext i16 %495 to i32
  %497 = call i32 @safe_add_func_int32_t_s_s(i32 noundef %496, i32 noundef 2)
  %498 = trunc i32 %497 to i16
  store i16 %498, ptr %6, align 2
  br label %379, !llvm.loop !65

499:                                              ; preds = %379
  %500 = getelementptr inbounds [10 x i8], ptr %37, i64 0, i64 9
  %501 = load i8, ptr %500, align 1
  %502 = sext i8 %501 to i32
  %503 = getelementptr inbounds [10 x i8], ptr %37, i64 0, i64 9
  %504 = load i8, ptr %503, align 1
  %505 = sext i8 %504 to i32
  %506 = load ptr, ptr %38, align 8
  %507 = icmp ne ptr %7, %506
  %508 = zext i1 %507 to i32
  %509 = load volatile ptr, ptr @g_82, align 8
  %510 = load i16, ptr %509, align 2
  %511 = sext i16 %510 to i64
  %512 = load ptr, ptr @g_837, align 8
  %513 = load ptr, ptr %512, align 8
  %514 = load i8, ptr %513, align 1
  %515 = call zeroext i8 @safe_mul_func_uint8_t_u_u(i8 noundef zeroext %514, i8 noundef zeroext 0)
  %516 = load i64, ptr %39, align 8
  %517 = icmp ule i64 %511, %516
  %518 = zext i1 %517 to i32
  %519 = sext i32 %518 to i64
  %520 = icmp eq i64 %519, -1405024572841970501
  %521 = zext i1 %520 to i32
  %522 = trunc i32 %521 to i16
  %523 = call zeroext i16 @safe_lshift_func_uint16_t_u_u(i16 noundef zeroext %522, i32 noundef 6)
  %524 = zext i16 %523 to i32
  %525 = load i32, ptr %4, align 4
  %526 = icmp eq i32 %524, %525
  %527 = zext i1 %526 to i32
  %528 = load i32, ptr %4, align 4
  %529 = icmp ule i32 %527, %528
  %530 = zext i1 %529 to i32
  %531 = sext i32 %530 to i64
  %532 = icmp sge i64 1, %531
  %533 = zext i1 %532 to i32
  %534 = load i8, ptr %5, align 1
  %535 = sext i8 %534 to i32
  %536 = icmp sge i32 %533, %535
  %537 = zext i1 %536 to i32
  %538 = sext i32 %537 to i64
  %539 = icmp sge i64 0, %538
  %540 = zext i1 %539 to i32
  %541 = icmp sgt i32 %508, %540
  %542 = zext i1 %541 to i32
  %543 = or i32 %505, %542
  %544 = sext i32 %543 to i64
  %545 = and i64 %544, 1
  %546 = trunc i64 %545 to i8
  %547 = load i104, ptr @g_936, align 1
  %548 = lshr i104 %547, 45
  %549 = and i104 %548, 8388607
  %550 = trunc i104 %549 to i32
  %551 = trunc i32 %550 to i8
  %552 = call zeroext i8 @safe_mul_func_uint8_t_u_u(i8 noundef zeroext %546, i8 noundef zeroext %551)
  %553 = zext i8 %552 to i32
  %554 = icmp sgt i32 %502, %553
  %555 = zext i1 %554 to i32
  %556 = load i32, ptr %4, align 4
  %557 = zext i32 %556 to i64
  %558 = call i64 @safe_add_func_int64_t_s_s(i64 noundef 0, i64 noundef %557)
  %559 = icmp slt i64 %558, 1019426258
  %560 = zext i1 %559 to i32
  %561 = sext i32 %560 to i64
  %562 = icmp ne i64 %561, -7
  %563 = zext i1 %562 to i32
  %564 = load ptr, ptr %40, align 8
  store i32 %563, ptr %564, align 4
  br label %579

565:                                              ; preds = %373
  store ptr getelementptr inbounds ([3 x i32], ptr @g_147, i64 0, i64 2), ptr %55, align 8
  store ptr null, ptr %56, align 8
  store ptr @g_147, ptr %57, align 8
  store ptr getelementptr inbounds ([3 x i32], ptr @g_147, i64 0, i64 2), ptr %58, align 8
  store i32 0, ptr %60, align 4
  br label %566

566:                                              ; preds = %573, %565
  %567 = load i32, ptr %60, align 4
  %568 = icmp slt i32 %567, 7
  br i1 %568, label %569, label %576

569:                                              ; preds = %566
  %570 = load i32, ptr %60, align 4
  %571 = sext i32 %570 to i64
  %572 = getelementptr inbounds [7 x ptr], ptr %59, i64 0, i64 %571
  store ptr getelementptr inbounds ([3 x i32], ptr @g_147, i64 0, i64 1), ptr %572, align 8
  br label %573

573:                                              ; preds = %569
  %574 = load i32, ptr %60, align 4
  %575 = add nsw i32 %574, 1
  store i32 %575, ptr %60, align 4
  br label %566, !llvm.loop !66

576:                                              ; preds = %566
  %577 = load i32, ptr %12, align 4
  %578 = add i32 %577, 1
  store i32 %578, ptr %12, align 4
  br label %579

579:                                              ; preds = %576, %499
  %580 = load i32, ptr %4, align 4
  %581 = load ptr, ptr %13, align 8
  %582 = load volatile ptr, ptr @g_980, align 8
  store ptr %581, ptr %582, align 8
  br label %583

583:                                              ; preds = %579, %393
  %584 = load i104, ptr @g_842, align 1
  %585 = shl i104 %584, 59
  %586 = ashr i104 %585, 79
  %587 = trunc i104 %586 to i32
  %588 = sext i32 %587 to i64
  %589 = icmp eq ptr %9, null
  %590 = zext i1 %589 to i32
  %591 = load i8, ptr %5, align 1
  %592 = sext i8 %591 to i32
  %593 = load ptr, ptr %8, align 8
  %594 = load i32, ptr %14, align 4
  %595 = load ptr, ptr %13, align 8
  %596 = load ptr, ptr %595, align 8
  %597 = load i8, ptr %596, align 1
  %598 = add i8 %597, -1
  store i8 %598, ptr %596, align 1
  %599 = zext i8 %597 to i32
  %600 = icmp sgt i32 %594, %599
  %601 = zext i1 %600 to i32
  %602 = trunc i32 %601 to i16
  %603 = getelementptr inbounds [9 x i32], ptr %11, i64 0, i64 5
  store i32 0, ptr %603, align 4
  %604 = load i32, ptr %4, align 4
  %605 = or i32 0, %604
  %606 = zext i32 %605 to i64
  %607 = and i64 %606, 0
  %608 = icmp ne i64 %607, 0
  br i1 %608, label %610, label %609

609:                                              ; preds = %583
  br label %610

610:                                              ; preds = %609, %583
  %611 = phi i1 [ true, %583 ], [ true, %609 ]
  %612 = zext i1 %611 to i32
  %613 = trunc i32 %612 to i16
  %614 = call zeroext i16 @safe_div_func_uint16_t_u_u(i16 noundef zeroext %602, i16 noundef zeroext %613)
  %615 = zext i16 %614 to i64
  %616 = load i64, ptr getelementptr inbounds ([2 x i64], ptr getelementptr inbounds ([9 x [4 x [2 x i64]]], ptr @g_160, i64 0, i64 8), i64 0, i64 1), align 8
  %617 = icmp sle i64 %615, %616
  %618 = zext i1 %617 to i32
  %619 = xor i32 %592, %618
  %620 = xor i32 %590, %619
  %621 = load i32, ptr %4, align 4
  %622 = icmp ne i32 %620, %621
  %623 = zext i1 %622 to i32
  %624 = load ptr, ptr %17, align 8
  %625 = load i32, ptr %624, align 4
  %626 = xor i32 %625, %623
  store i32 %626, ptr %624, align 4
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %61, ptr align 16 @g_473, i64 8, i1 true)
  %627 = icmp uge i64 %588, 4294967295
  %628 = zext i1 %627 to i32
  %629 = sext i32 %628 to i64
  %630 = icmp eq i64 254, %629
  br i1 %630, label %631, label %763

631:                                              ; preds = %610
  store ptr %8, ptr %62, align 8
  store ptr %12, ptr %63, align 8
  %632 = getelementptr inbounds ptr, ptr %63, i64 1
  store ptr @g_486, ptr %632, align 8
  %633 = getelementptr inbounds ptr, ptr %63, i64 2
  store ptr @g_486, ptr %633, align 8
  %634 = getelementptr inbounds ptr, ptr %63, i64 3
  store ptr %12, ptr %634, align 8
  %635 = getelementptr inbounds [4 x ptr], ptr %63, i64 1
  store ptr %12, ptr %635, align 8
  %636 = getelementptr inbounds ptr, ptr %635, i64 1
  store ptr @g_486, ptr %636, align 8
  %637 = getelementptr inbounds ptr, ptr %635, i64 2
  store ptr null, ptr %637, align 8
  %638 = getelementptr inbounds ptr, ptr %635, i64 3
  store ptr %12, ptr %638, align 8
  %639 = getelementptr inbounds [4 x ptr], ptr %63, i64 2
  store ptr @g_486, ptr %639, align 8
  %640 = getelementptr inbounds ptr, ptr %639, i64 1
  store ptr %12, ptr %640, align 8
  %641 = getelementptr inbounds ptr, ptr %639, i64 2
  store ptr %12, ptr %641, align 8
  %642 = getelementptr inbounds ptr, ptr %639, i64 3
  store ptr %12, ptr %642, align 8
  %643 = getelementptr inbounds [4 x ptr], ptr %63, i64 3
  store ptr %12, ptr %643, align 8
  %644 = getelementptr inbounds ptr, ptr %643, i64 1
  store ptr %12, ptr %644, align 8
  %645 = getelementptr inbounds ptr, ptr %643, i64 2
  store ptr %12, ptr %645, align 8
  %646 = getelementptr inbounds ptr, ptr %643, i64 3
  store ptr %12, ptr %646, align 8
  %647 = getelementptr inbounds [4 x ptr], ptr %63, i64 4
  store ptr %12, ptr %647, align 8
  %648 = getelementptr inbounds ptr, ptr %647, i64 1
  store ptr %12, ptr %648, align 8
  %649 = getelementptr inbounds ptr, ptr %647, i64 2
  store ptr @g_486, ptr %649, align 8
  %650 = getelementptr inbounds ptr, ptr %647, i64 3
  store ptr %12, ptr %650, align 8
  %651 = getelementptr inbounds [4 x ptr], ptr %63, i64 5
  store ptr null, ptr %651, align 8
  %652 = getelementptr inbounds ptr, ptr %651, i64 1
  store ptr @g_486, ptr %652, align 8
  %653 = getelementptr inbounds ptr, ptr %651, i64 2
  store ptr %12, ptr %653, align 8
  %654 = getelementptr inbounds ptr, ptr %651, i64 3
  store ptr %12, ptr %654, align 8
  %655 = getelementptr inbounds [4 x ptr], ptr %63, i64 6
  store ptr @g_486, ptr %655, align 8
  %656 = getelementptr inbounds ptr, ptr %655, i64 1
  store ptr @g_486, ptr %656, align 8
  %657 = getelementptr inbounds ptr, ptr %655, i64 2
  store ptr %12, ptr %657, align 8
  %658 = getelementptr inbounds ptr, ptr %655, i64 3
  store ptr %12, ptr %658, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %64, ptr align 16 @__const.func_52.l_1010, i64 80, i1 false)
  store ptr @g_490, ptr %65, align 8
  %659 = getelementptr inbounds [3 x ptr], ptr %66, i64 0, i64 0
  store ptr %659, ptr %67, align 8
  store i32 0, ptr %68, align 4
  br label %660

660:                                              ; preds = %667, %631
  %661 = load i32, ptr %68, align 4
  %662 = icmp slt i32 %661, 3
  br i1 %662, label %663, label %670

663:                                              ; preds = %660
  %664 = load i32, ptr %68, align 4
  %665 = sext i32 %664 to i64
  %666 = getelementptr inbounds [3 x ptr], ptr %66, i64 0, i64 %665
  store ptr %21, ptr %666, align 8
  br label %667

667:                                              ; preds = %663
  %668 = load i32, ptr %68, align 4
  %669 = add nsw i32 %668, 1
  store i32 %669, ptr %68, align 4
  br label %660, !llvm.loop !67

670:                                              ; preds = %660
  store i8 -21, ptr @g_88, align 1
  br label %671

671:                                              ; preds = %690, %670
  %672 = load i8, ptr @g_88, align 1
  %673 = zext i8 %672 to i32
  %674 = icmp slt i32 %673, 33
  br i1 %674, label %675, label %693

675:                                              ; preds = %671
  store ptr %8, ptr %70, align 8
  store i32 474219653, ptr %71, align 4
  store i32 -2, ptr %72, align 4
  %676 = call zeroext i16 @safe_mod_func_uint16_t_u_u(i16 noundef zeroext 0, i16 noundef zeroext 4279)
  %677 = zext i16 %676 to i32
  %678 = load ptr, ptr %62, align 8
  %679 = load ptr, ptr %70, align 8
  %680 = icmp eq ptr %678, %679
  %681 = zext i1 %680 to i32
  %682 = icmp sle i32 %677, %681
  %683 = zext i1 %682 to i32
  %684 = trunc i32 %683 to i8
  store i8 %684, ptr @g_118, align 1
  %685 = call signext i8 @safe_add_func_int8_t_s_s(i8 noundef signext %684, i8 noundef signext 1)
  %686 = sext i8 %685 to i32
  %687 = load ptr, ptr %17, align 8
  %688 = load i32, ptr %687, align 4
  %689 = and i32 %688, %686
  store i32 %689, ptr %687, align 4
  br label %690

690:                                              ; preds = %675
  %691 = load i8, ptr @g_88, align 1
  %692 = add i8 %691, 1
  store i8 %692, ptr @g_88, align 1
  br label %671, !llvm.loop !68

693:                                              ; preds = %671
  %694 = load i16, ptr %6, align 2
  %695 = zext i16 %694 to i32
  %696 = load ptr, ptr %17, align 8
  %697 = load i32, ptr %696, align 4
  store i32 %697, ptr %18, align 4
  %698 = zext i32 %697 to i64
  %699 = icmp uge i64 65531, %698
  %700 = zext i1 %699 to i32
  %701 = load ptr, ptr %19, align 8
  store ptr %701, ptr @g_1013, align 8
  %702 = load ptr, ptr %65, align 8
  store ptr @g_491, ptr %702, align 8
  %703 = icmp ne ptr %701, @g_491
  %704 = zext i1 %703 to i32
  %705 = trunc i32 %704 to i8
  store i8 %705, ptr @g_118, align 1
  %706 = sext i8 %705 to i32
  %707 = icmp sle i32 %700, %706
  %708 = zext i1 %707 to i32
  %709 = load i32, ptr %4, align 4
  %710 = icmp ne i32 %709, 0
  br i1 %710, label %742, label %711

711:                                              ; preds = %693
  %712 = icmp eq ptr %9, @g_674
  %713 = zext i1 %712 to i32
  %714 = call signext i16 @safe_lshift_func_int16_t_s_s(i16 noundef signext 19068, i32 noundef 4)
  %715 = icmp ne i16 %714, 0
  %716 = xor i1 %715, true
  %717 = zext i1 %716 to i32
  %718 = or i32 %713, %717
  %719 = sext i32 %718 to i64
  %720 = load i64, ptr %20, align 8
  %721 = icmp ule i64 %719, %720
  %722 = zext i1 %721 to i32
  %723 = trunc i32 %722 to i8
  %724 = call signext i8 @safe_mul_func_int8_t_s_s(i8 noundef signext %723, i8 noundef signext 101)
  %725 = sext i8 %724 to i16
  %726 = call signext i16 @safe_add_func_int16_t_s_s(i16 noundef signext %725, i16 noundef signext -4)
  %727 = sext i16 %726 to i32
  %728 = icmp ne i32 %727, 0
  br i1 %728, label %732, label %729

729:                                              ; preds = %711
  %730 = load i32, ptr %4, align 4
  %731 = icmp ne i32 %730, 0
  br label %732

732:                                              ; preds = %729, %711
  %733 = phi i1 [ true, %711 ], [ %731, %729 ]
  %734 = zext i1 %733 to i32
  %735 = load ptr, ptr %17, align 8
  %736 = load i32, ptr %735, align 4
  %737 = sext i32 %736 to i64
  %738 = or i64 %737, 4294967295
  %739 = trunc i64 %738 to i32
  store i32 %739, ptr %735, align 4
  %740 = load i32, ptr %4, align 4
  %741 = icmp uge i32 %739, %740
  br label %742

742:                                              ; preds = %732, %693
  %743 = phi i1 [ true, %693 ], [ %741, %732 ]
  %744 = zext i1 %743 to i32
  %745 = or i32 %695, %744
  %746 = trunc i32 %745 to i16
  %747 = load i32, ptr %4, align 4
  %748 = trunc i32 %747 to i16
  %749 = call signext i16 @safe_mod_func_int16_t_s_s(i16 noundef signext %746, i16 noundef signext %748)
  %750 = sext i16 %749 to i64
  %751 = and i64 %750, -2
  %752 = load ptr, ptr @g_544, align 8
  %753 = load i8, ptr %752, align 1
  %754 = zext i8 %753 to i32
  %755 = call zeroext i8 @safe_rshift_func_uint8_t_u_u(i8 noundef zeroext 79, i32 noundef %754)
  %756 = load i16, ptr %6, align 2
  %757 = zext i16 %756 to i32
  %758 = call signext i8 @safe_lshift_func_int8_t_s_s(i8 noundef signext %755, i32 noundef %757)
  %759 = sext i8 %758 to i32
  %760 = load volatile ptr, ptr @g_1025, align 8
  store i32 %759, ptr %760, align 4
  %761 = load ptr, ptr %22, align 8
  %762 = load ptr, ptr %67, align 8
  store ptr %761, ptr %762, align 8
  br label %798

763:                                              ; preds = %610
  store i32 -441345411, ptr %73, align 4
  %764 = getelementptr inbounds [9 x i32], ptr %11, i64 0, i64 2
  store ptr %764, ptr %74, align 8
  store ptr %14, ptr %75, align 8
  store ptr getelementptr inbounds ([3 x i32], ptr @g_147, i64 0, i64 2), ptr %76, align 8
  %765 = getelementptr inbounds ptr, ptr %76, i64 1
  store ptr @g_147, ptr %765, align 8
  %766 = getelementptr inbounds [2 x ptr], ptr %76, i64 1
  %767 = getelementptr inbounds [9 x i32], ptr %11, i64 0, i64 5
  store ptr %767, ptr %766, align 8
  %768 = getelementptr inbounds ptr, ptr %766, i64 1
  store ptr @g_147, ptr %768, align 8
  %769 = getelementptr inbounds [2 x ptr], ptr %76, i64 2
  store ptr getelementptr inbounds ([3 x i32], ptr @g_147, i64 0, i64 2), ptr %769, align 8
  %770 = getelementptr inbounds ptr, ptr %769, i64 1
  %771 = getelementptr inbounds [9 x i32], ptr %11, i64 0, i64 5
  store ptr %771, ptr %770, align 8
  %772 = getelementptr inbounds [2 x ptr], ptr %76, i64 3
  store ptr getelementptr inbounds ([8 x i32], ptr getelementptr inbounds ([7 x [1 x [8 x i32]]], ptr @g_17, i64 0, i64 3), i64 0, i64 6), ptr %772, align 8
  %773 = getelementptr inbounds ptr, ptr %772, i64 1
  store ptr getelementptr inbounds ([8 x i32], ptr getelementptr inbounds ([7 x [1 x [8 x i32]]], ptr @g_17, i64 0, i64 3), i64 0, i64 6), ptr %773, align 8
  %774 = getelementptr inbounds [2 x ptr], ptr %76, i64 4
  store ptr getelementptr inbounds ([8 x i32], ptr getelementptr inbounds ([7 x [1 x [8 x i32]]], ptr @g_17, i64 0, i64 3), i64 0, i64 6), ptr %774, align 8
  %775 = getelementptr inbounds ptr, ptr %774, i64 1
  %776 = getelementptr inbounds [9 x i32], ptr %11, i64 0, i64 5
  store ptr %776, ptr %775, align 8
  %777 = getelementptr inbounds [2 x ptr], ptr %76, i64 5
  store ptr getelementptr inbounds ([3 x i32], ptr @g_147, i64 0, i64 2), ptr %777, align 8
  %778 = getelementptr inbounds ptr, ptr %777, i64 1
  store ptr @g_147, ptr %778, align 8
  %779 = getelementptr inbounds [2 x ptr], ptr %76, i64 6
  %780 = getelementptr inbounds [9 x i32], ptr %11, i64 0, i64 5
  store ptr %780, ptr %779, align 8
  %781 = getelementptr inbounds ptr, ptr %779, i64 1
  store ptr @g_147, ptr %781, align 8
  %782 = getelementptr inbounds [2 x ptr], ptr %76, i64 7
  store ptr getelementptr inbounds ([3 x i32], ptr @g_147, i64 0, i64 2), ptr %782, align 8
  %783 = getelementptr inbounds ptr, ptr %782, i64 1
  %784 = getelementptr inbounds [9 x i32], ptr %11, i64 0, i64 5
  store ptr %784, ptr %783, align 8
  %785 = getelementptr inbounds [2 x ptr], ptr %76, i64 8
  store ptr getelementptr inbounds ([8 x i32], ptr getelementptr inbounds ([7 x [1 x [8 x i32]]], ptr @g_17, i64 0, i64 3), i64 0, i64 6), ptr %785, align 8
  %786 = getelementptr inbounds ptr, ptr %785, i64 1
  store ptr getelementptr inbounds ([8 x i32], ptr getelementptr inbounds ([7 x [1 x [8 x i32]]], ptr @g_17, i64 0, i64 3), i64 0, i64 6), ptr %786, align 8
  %787 = getelementptr inbounds [2 x ptr], ptr %76, i64 9
  store ptr getelementptr inbounds ([8 x i32], ptr getelementptr inbounds ([7 x [1 x [8 x i32]]], ptr @g_17, i64 0, i64 3), i64 0, i64 6), ptr %787, align 8
  %788 = getelementptr inbounds ptr, ptr %787, i64 1
  %789 = getelementptr inbounds [9 x i32], ptr %11, i64 0, i64 5
  store ptr %789, ptr %788, align 8
  %790 = load i16, ptr %23, align 2
  %791 = add i16 %790, 1
  store i16 %791, ptr %23, align 2
  %792 = load ptr, ptr %9, align 8
  %793 = load ptr, ptr %792, align 8
  %794 = load i104, ptr @g_842, align 1
  %795 = shl i104 %794, 59
  %796 = ashr i104 %795, 79
  %797 = trunc i104 %796 to i32
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %793, ptr align 1 @g_1038, i64 13, i1 true)
  br label %798

798:                                              ; preds = %763, %742
  %799 = load ptr, ptr @g_1040, align 8
  %800 = load ptr, ptr %24, align 8
  store ptr %799, ptr %800, align 8
  %801 = icmp eq ptr %799, @g_1041
  br i1 %801, label %802, label %813

802:                                              ; preds = %798
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %79, ptr align 16 @__const.func_52.l_1049, i64 100, i1 false)
  store ptr null, ptr %80, align 8
  store ptr @g_1026, ptr %81, align 8
  store i32 -2, ptr %82, align 4
  store i32 -1, ptr %83, align 4
  store ptr %21, ptr %84, align 8
  store ptr @g_296, ptr %85, align 8
  store ptr @g_296, ptr %86, align 8
  store i8 -5, ptr %87, align 1
  store ptr null, ptr %88, align 8
  store ptr %88, ptr %89, align 8
  store i64 0, ptr %20, align 8
  br label %803

803:                                              ; preds = %807, %802
  %804 = load i64, ptr %20, align 8
  %805 = icmp ule i64 %804, 24
  br i1 %805, label %806, label %810

806:                                              ; preds = %803
  store ptr @g_490, ptr %92, align 8
  store i32 546949936, ptr %93, align 4
  store ptr @g_365, ptr %94, align 8
  store ptr @g_544, ptr %95, align 8
  store ptr %9, ptr %96, align 8
  store i64 268832564067170597, ptr %97, align 8
  store i16 0, ptr %98, align 2
  store ptr null, ptr %99, align 8
  store ptr %21, ptr %100, align 8
  br label %807

807:                                              ; preds = %806
  %808 = load i64, ptr %20, align 8
  %809 = call i64 @safe_add_func_int64_t_s_s(i64 noundef %808, i64 noundef 2)
  store i64 %809, ptr %20, align 8
  br label %803, !llvm.loop !69

810:                                              ; preds = %803
  %811 = load ptr, ptr %88, align 8
  %812 = load ptr, ptr %89, align 8
  store ptr %811, ptr %812, align 8
  store ptr %811, ptr @g_1379, align 8
  br label %820

813:                                              ; preds = %798
  store ptr getelementptr inbounds ([9 x ptr], ptr @g_1158, i64 0, i64 1), ptr %101, align 8
  %814 = load ptr, ptr @g_1013, align 8
  %815 = load ptr, ptr %814, align 8
  %816 = load ptr, ptr %815, align 8
  %817 = load ptr, ptr %19, align 8
  %818 = load ptr, ptr %817, align 8
  store ptr %816, ptr %818, align 8
  %819 = load ptr, ptr %101, align 8
  store ptr @g_1159, ptr %819, align 8
  br label %820

820:                                              ; preds = %813, %810
  %821 = load ptr, ptr %32, align 8
  ret ptr %821
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i8 @safe_add_func_uint8_t_u_u(i8 noundef zeroext %0, i8 noundef zeroext %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i8, align 1
  store i8 %0, ptr %3, align 1
  store i8 %1, ptr %4, align 1
  %5 = load i8, ptr %3, align 1
  %6 = zext i8 %5 to i32
  %7 = load i8, ptr %4, align 1
  %8 = zext i8 %7 to i32
  %9 = add nsw i32 %6, %8
  %10 = trunc i32 %9 to i8
  ret i8 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i16 @safe_mod_func_uint16_t_u_u(i16 noundef zeroext %0, i16 noundef zeroext %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i16, align 2
  store i16 %0, ptr %3, align 2
  store i16 %1, ptr %4, align 2
  %5 = load i16, ptr %4, align 2
  %6 = zext i16 %5 to i32
  %7 = icmp eq i32 %6, 0
  br i1 %7, label %8, label %11

8:                                                ; preds = %2
  %9 = load i16, ptr %3, align 2
  %10 = zext i16 %9 to i32
  br label %17

11:                                               ; preds = %2
  %12 = load i16, ptr %3, align 2
  %13 = zext i16 %12 to i32
  %14 = load i16, ptr %4, align 2
  %15 = zext i16 %14 to i32
  %16 = srem i32 %13, %15
  br label %17

17:                                               ; preds = %11, %8
  %18 = phi i32 [ %10, %8 ], [ %16, %11 ]
  %19 = trunc i32 %18 to i16
  ret i16 %19
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i16 @safe_rshift_func_uint16_t_u_u(i16 noundef zeroext %0, i32 noundef %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i32, align 4
  store i16 %0, ptr %3, align 2
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = icmp uge i32 %5, 32
  br i1 %6, label %7, label %10

7:                                                ; preds = %2
  %8 = load i16, ptr %3, align 2
  %9 = zext i16 %8 to i32
  br label %15

10:                                               ; preds = %2
  %11 = load i16, ptr %3, align 2
  %12 = zext i16 %11 to i32
  %13 = load i32, ptr %4, align 4
  %14 = ashr i32 %12, %13
  br label %15

15:                                               ; preds = %10, %7
  %16 = phi i32 [ %9, %7 ], [ %14, %10 ]
  %17 = trunc i32 %16 to i16
  ret i16 %17
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr writeonly captures(none), i8, i64, i1 immarg) #4

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i16 @safe_add_func_int16_t_s_s(i16 noundef signext %0, i16 noundef signext %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i16, align 2
  store i16 %0, ptr %3, align 2
  store i16 %1, ptr %4, align 2
  %5 = load i16, ptr %3, align 2
  %6 = sext i16 %5 to i32
  %7 = load i16, ptr %4, align 2
  %8 = sext i16 %7 to i32
  %9 = add nsw i32 %6, %8
  %10 = trunc i32 %9 to i16
  ret i16 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i8 @safe_lshift_func_uint8_t_u_u(i8 noundef zeroext %0, i32 noundef %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i32, align 4
  store i8 %0, ptr %3, align 1
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = icmp uge i32 %5, 32
  br i1 %6, label %13, label %7

7:                                                ; preds = %2
  %8 = load i8, ptr %3, align 1
  %9 = zext i8 %8 to i32
  %10 = load i32, ptr %4, align 4
  %11 = ashr i32 255, %10
  %12 = icmp sgt i32 %9, %11
  br i1 %12, label %13, label %16

13:                                               ; preds = %7, %2
  %14 = load i8, ptr %3, align 1
  %15 = zext i8 %14 to i32
  br label %21

16:                                               ; preds = %7
  %17 = load i8, ptr %3, align 1
  %18 = zext i8 %17 to i32
  %19 = load i32, ptr %4, align 4
  %20 = shl i32 %18, %19
  br label %21

21:                                               ; preds = %16, %13
  %22 = phi i32 [ %15, %13 ], [ %20, %16 ]
  %23 = trunc i32 %22 to i8
  ret i8 %23
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i32 @safe_add_func_int32_t_s_s(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %3, align 4
  %6 = icmp sgt i32 %5, 0
  br i1 %6, label %7, label %15

7:                                                ; preds = %2
  %8 = load i32, ptr %4, align 4
  %9 = icmp sgt i32 %8, 0
  br i1 %9, label %10, label %15

10:                                               ; preds = %7
  %11 = load i32, ptr %3, align 4
  %12 = load i32, ptr %4, align 4
  %13 = sub nsw i32 2147483647, %12
  %14 = icmp sgt i32 %11, %13
  br i1 %14, label %26, label %15

15:                                               ; preds = %10, %7, %2
  %16 = load i32, ptr %3, align 4
  %17 = icmp slt i32 %16, 0
  br i1 %17, label %18, label %28

18:                                               ; preds = %15
  %19 = load i32, ptr %4, align 4
  %20 = icmp slt i32 %19, 0
  br i1 %20, label %21, label %28

21:                                               ; preds = %18
  %22 = load i32, ptr %3, align 4
  %23 = load i32, ptr %4, align 4
  %24 = sub nsw i32 -2147483648, %23
  %25 = icmp slt i32 %22, %24
  br i1 %25, label %26, label %28

26:                                               ; preds = %21, %10
  %27 = load i32, ptr %3, align 4
  br label %32

28:                                               ; preds = %21, %18, %15
  %29 = load i32, ptr %3, align 4
  %30 = load i32, ptr %4, align 4
  %31 = add nsw i32 %29, %30
  br label %32

32:                                               ; preds = %28, %26
  %33 = phi i32 [ %27, %26 ], [ %31, %28 ]
  ret i32 %33
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i64 @safe_add_func_int64_t_s_s(i64 noundef %0, i64 noundef %1) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, ptr %3, align 8
  store i64 %1, ptr %4, align 8
  %5 = load i64, ptr %3, align 8
  %6 = icmp sgt i64 %5, 0
  br i1 %6, label %7, label %15

7:                                                ; preds = %2
  %8 = load i64, ptr %4, align 8
  %9 = icmp sgt i64 %8, 0
  br i1 %9, label %10, label %15

10:                                               ; preds = %7
  %11 = load i64, ptr %3, align 8
  %12 = load i64, ptr %4, align 8
  %13 = sub nsw i64 9223372036854775807, %12
  %14 = icmp sgt i64 %11, %13
  br i1 %14, label %26, label %15

15:                                               ; preds = %10, %7, %2
  %16 = load i64, ptr %3, align 8
  %17 = icmp slt i64 %16, 0
  br i1 %17, label %18, label %28

18:                                               ; preds = %15
  %19 = load i64, ptr %4, align 8
  %20 = icmp slt i64 %19, 0
  br i1 %20, label %21, label %28

21:                                               ; preds = %18
  %22 = load i64, ptr %3, align 8
  %23 = load i64, ptr %4, align 8
  %24 = sub nsw i64 -9223372036854775808, %23
  %25 = icmp slt i64 %22, %24
  br i1 %25, label %26, label %28

26:                                               ; preds = %21, %10
  %27 = load i64, ptr %3, align 8
  br label %32

28:                                               ; preds = %21, %18, %15
  %29 = load i64, ptr %3, align 8
  %30 = load i64, ptr %4, align 8
  %31 = add nsw i64 %29, %30
  br label %32

32:                                               ; preds = %28, %26
  %33 = phi i64 [ %27, %26 ], [ %31, %28 ]
  ret i64 %33
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i8 @safe_mul_func_uint8_t_u_u(i8 noundef zeroext %0, i8 noundef zeroext %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i8, align 1
  store i8 %0, ptr %3, align 1
  store i8 %1, ptr %4, align 1
  %5 = load i8, ptr %3, align 1
  %6 = zext i8 %5 to i32
  %7 = load i8, ptr %4, align 1
  %8 = zext i8 %7 to i32
  %9 = mul i32 %6, %8
  %10 = trunc i32 %9 to i8
  ret i8 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i16 @safe_lshift_func_uint16_t_u_u(i16 noundef zeroext %0, i32 noundef %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i32, align 4
  store i16 %0, ptr %3, align 2
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = icmp uge i32 %5, 32
  br i1 %6, label %13, label %7

7:                                                ; preds = %2
  %8 = load i16, ptr %3, align 2
  %9 = zext i16 %8 to i32
  %10 = load i32, ptr %4, align 4
  %11 = ashr i32 65535, %10
  %12 = icmp sgt i32 %9, %11
  br i1 %12, label %13, label %16

13:                                               ; preds = %7, %2
  %14 = load i16, ptr %3, align 2
  %15 = zext i16 %14 to i32
  br label %21

16:                                               ; preds = %7
  %17 = load i16, ptr %3, align 2
  %18 = zext i16 %17 to i32
  %19 = load i32, ptr %4, align 4
  %20 = shl i32 %18, %19
  br label %21

21:                                               ; preds = %16, %13
  %22 = phi i32 [ %15, %13 ], [ %20, %16 ]
  %23 = trunc i32 %22 to i16
  ret i16 %23
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i8 @safe_lshift_func_int8_t_s_s(i8 noundef signext %0, i32 noundef %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i32, align 4
  store i8 %0, ptr %3, align 1
  store i32 %1, ptr %4, align 4
  %5 = load i8, ptr %3, align 1
  %6 = sext i8 %5 to i32
  %7 = icmp slt i32 %6, 0
  br i1 %7, label %20, label %8

8:                                                ; preds = %2
  %9 = load i32, ptr %4, align 4
  %10 = icmp slt i32 %9, 0
  br i1 %10, label %20, label %11

11:                                               ; preds = %8
  %12 = load i32, ptr %4, align 4
  %13 = icmp sge i32 %12, 32
  br i1 %13, label %20, label %14

14:                                               ; preds = %11
  %15 = load i8, ptr %3, align 1
  %16 = sext i8 %15 to i32
  %17 = load i32, ptr %4, align 4
  %18 = ashr i32 127, %17
  %19 = icmp sgt i32 %16, %18
  br i1 %19, label %20, label %23

20:                                               ; preds = %14, %11, %8, %2
  %21 = load i8, ptr %3, align 1
  %22 = sext i8 %21 to i32
  br label %28

23:                                               ; preds = %14
  %24 = load i8, ptr %3, align 1
  %25 = sext i8 %24 to i32
  %26 = load i32, ptr %4, align 4
  %27 = shl i32 %25, %26
  br label %28

28:                                               ; preds = %23, %20
  %29 = phi i32 [ %22, %20 ], [ %27, %23 ]
  %30 = trunc i32 %29 to i8
  ret i8 %30
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i8 @safe_rshift_func_uint8_t_u_u(i8 noundef zeroext %0, i32 noundef %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i32, align 4
  store i8 %0, ptr %3, align 1
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = icmp uge i32 %5, 32
  br i1 %6, label %7, label %10

7:                                                ; preds = %2
  %8 = load i8, ptr %3, align 1
  %9 = zext i8 %8 to i32
  br label %15

10:                                               ; preds = %2
  %11 = load i8, ptr %3, align 1
  %12 = zext i8 %11 to i32
  %13 = load i32, ptr %4, align 4
  %14 = ashr i32 %12, %13
  br label %15

15:                                               ; preds = %10, %7
  %16 = phi i32 [ %9, %7 ], [ %14, %10 ]
  %17 = trunc i32 %16 to i8
  ret i8 %17
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i16 @safe_mod_func_int16_t_s_s(i16 noundef signext %0, i16 noundef signext %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i16, align 2
  store i16 %0, ptr %3, align 2
  store i16 %1, ptr %4, align 2
  %5 = load i16, ptr %4, align 2
  %6 = sext i16 %5 to i32
  %7 = icmp eq i32 %6, 0
  br i1 %7, label %16, label %8

8:                                                ; preds = %2
  %9 = load i16, ptr %3, align 2
  %10 = sext i16 %9 to i32
  %11 = icmp eq i32 %10, -32768
  br i1 %11, label %12, label %19

12:                                               ; preds = %8
  %13 = load i16, ptr %4, align 2
  %14 = sext i16 %13 to i32
  %15 = icmp eq i32 %14, -1
  br i1 %15, label %16, label %19

16:                                               ; preds = %12, %2
  %17 = load i16, ptr %3, align 2
  %18 = sext i16 %17 to i32
  br label %25

19:                                               ; preds = %12, %8
  %20 = load i16, ptr %3, align 2
  %21 = sext i16 %20 to i32
  %22 = load i16, ptr %4, align 2
  %23 = sext i16 %22 to i32
  %24 = srem i32 %21, %23
  br label %25

25:                                               ; preds = %19, %16
  %26 = phi i32 [ %18, %16 ], [ %24, %19 ]
  %27 = trunc i32 %26 to i16
  ret i16 %27
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i8 @safe_mul_func_int8_t_s_s(i8 noundef signext %0, i8 noundef signext %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i8, align 1
  store i8 %0, ptr %3, align 1
  store i8 %1, ptr %4, align 1
  %5 = load i8, ptr %3, align 1
  %6 = sext i8 %5 to i32
  %7 = load i8, ptr %4, align 1
  %8 = sext i8 %7 to i32
  %9 = mul nsw i32 %6, %8
  %10 = trunc i32 %9 to i8
  ret i8 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i16 @safe_lshift_func_int16_t_s_s(i16 noundef signext %0, i32 noundef %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i32, align 4
  store i16 %0, ptr %3, align 2
  store i32 %1, ptr %4, align 4
  %5 = load i16, ptr %3, align 2
  %6 = sext i16 %5 to i32
  %7 = icmp slt i32 %6, 0
  br i1 %7, label %20, label %8

8:                                                ; preds = %2
  %9 = load i32, ptr %4, align 4
  %10 = icmp slt i32 %9, 0
  br i1 %10, label %20, label %11

11:                                               ; preds = %8
  %12 = load i32, ptr %4, align 4
  %13 = icmp sge i32 %12, 32
  br i1 %13, label %20, label %14

14:                                               ; preds = %11
  %15 = load i16, ptr %3, align 2
  %16 = sext i16 %15 to i32
  %17 = load i32, ptr %4, align 4
  %18 = ashr i32 32767, %17
  %19 = icmp sgt i32 %16, %18
  br i1 %19, label %20, label %23

20:                                               ; preds = %14, %11, %8, %2
  %21 = load i16, ptr %3, align 2
  %22 = sext i16 %21 to i32
  br label %28

23:                                               ; preds = %14
  %24 = load i16, ptr %3, align 2
  %25 = sext i16 %24 to i32
  %26 = load i32, ptr %4, align 4
  %27 = shl i32 %25, %26
  br label %28

28:                                               ; preds = %23, %20
  %29 = phi i32 [ %22, %20 ], [ %27, %23 ]
  %30 = trunc i32 %29 to i16
  ret i16 %30
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i64 @safe_div_func_uint64_t_u_u(i64 noundef %0, i64 noundef %1) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, ptr %3, align 8
  store i64 %1, ptr %4, align 8
  %5 = load i64, ptr %4, align 8
  %6 = icmp eq i64 %5, 0
  br i1 %6, label %7, label %9

7:                                                ; preds = %2
  %8 = load i64, ptr %3, align 8
  br label %13

9:                                                ; preds = %2
  %10 = load i64, ptr %3, align 8
  %11 = load i64, ptr %4, align 8
  %12 = udiv i64 %10, %11
  br label %13

13:                                               ; preds = %9, %7
  %14 = phi i64 [ %8, %7 ], [ %12, %9 ]
  ret i64 %14
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i16 @safe_unary_minus_func_int16_t_s(i16 noundef signext %0) #0 {
  %2 = alloca i16, align 2
  store i16 %0, ptr %2, align 2
  %3 = load i16, ptr %2, align 2
  %4 = sext i16 %3 to i32
  %5 = sub nsw i32 0, %4
  %6 = trunc i32 %5 to i16
  ret i16 %6
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i16 @safe_mul_func_uint16_t_u_u(i16 noundef zeroext %0, i16 noundef zeroext %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i16, align 2
  store i16 %0, ptr %3, align 2
  store i16 %1, ptr %4, align 2
  %5 = load i16, ptr %3, align 2
  %6 = zext i16 %5 to i32
  %7 = load i16, ptr %4, align 2
  %8 = zext i16 %7 to i32
  %9 = mul i32 %6, %8
  %10 = trunc i32 %9 to i16
  ret i16 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i8 @safe_sub_func_uint8_t_u_u(i8 noundef zeroext %0, i8 noundef zeroext %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i8, align 1
  store i8 %0, ptr %3, align 1
  store i8 %1, ptr %4, align 1
  %5 = load i8, ptr %3, align 1
  %6 = zext i8 %5 to i32
  %7 = load i8, ptr %4, align 1
  %8 = zext i8 %7 to i32
  %9 = sub nsw i32 %6, %8
  %10 = trunc i32 %9 to i8
  ret i8 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i32 @safe_mod_func_int32_t_s_s(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = icmp eq i32 %5, 0
  br i1 %6, label %13, label %7

7:                                                ; preds = %2
  %8 = load i32, ptr %3, align 4
  %9 = icmp eq i32 %8, -2147483648
  br i1 %9, label %10, label %15

10:                                               ; preds = %7
  %11 = load i32, ptr %4, align 4
  %12 = icmp eq i32 %11, -1
  br i1 %12, label %13, label %15

13:                                               ; preds = %10, %2
  %14 = load i32, ptr %3, align 4
  br label %19

15:                                               ; preds = %10, %7
  %16 = load i32, ptr %3, align 4
  %17 = load i32, ptr %4, align 4
  %18 = srem i32 %16, %17
  br label %19

19:                                               ; preds = %15, %13
  %20 = phi i32 [ %14, %13 ], [ %18, %15 ]
  ret i32 %20
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i16 @safe_rshift_func_int16_t_s_s(i16 noundef signext %0, i32 noundef %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i32, align 4
  store i16 %0, ptr %3, align 2
  store i32 %1, ptr %4, align 4
  %5 = load i16, ptr %3, align 2
  %6 = sext i16 %5 to i32
  %7 = icmp slt i32 %6, 0
  br i1 %7, label %14, label %8

8:                                                ; preds = %2
  %9 = load i32, ptr %4, align 4
  %10 = icmp slt i32 %9, 0
  br i1 %10, label %14, label %11

11:                                               ; preds = %8
  %12 = load i32, ptr %4, align 4
  %13 = icmp sge i32 %12, 32
  br i1 %13, label %14, label %17

14:                                               ; preds = %11, %8, %2
  %15 = load i16, ptr %3, align 2
  %16 = sext i16 %15 to i32
  br label %22

17:                                               ; preds = %11
  %18 = load i16, ptr %3, align 2
  %19 = sext i16 %18 to i32
  %20 = load i32, ptr %4, align 4
  %21 = ashr i32 %19, %20
  br label %22

22:                                               ; preds = %17, %14
  %23 = phi i32 [ %16, %14 ], [ %21, %17 ]
  %24 = trunc i32 %23 to i16
  ret i16 %24
}

; Function Attrs: noinline nounwind optnone uwtable
define internal signext i8 @safe_sub_func_int8_t_s_s(i8 noundef signext %0, i8 noundef signext %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i8, align 1
  store i8 %0, ptr %3, align 1
  store i8 %1, ptr %4, align 1
  %5 = load i8, ptr %3, align 1
  %6 = sext i8 %5 to i32
  %7 = load i8, ptr %4, align 1
  %8 = sext i8 %7 to i32
  %9 = sub nsw i32 %6, %8
  %10 = trunc i32 %9 to i8
  ret i8 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i64 @safe_mod_func_uint64_t_u_u(i64 noundef %0, i64 noundef %1) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, ptr %3, align 8
  store i64 %1, ptr %4, align 8
  %5 = load i64, ptr %4, align 8
  %6 = icmp eq i64 %5, 0
  br i1 %6, label %7, label %9

7:                                                ; preds = %2
  %8 = load i64, ptr %3, align 8
  br label %13

9:                                                ; preds = %2
  %10 = load i64, ptr %3, align 8
  %11 = load i64, ptr %4, align 8
  %12 = urem i64 %10, %11
  br label %13

13:                                               ; preds = %9, %7
  %14 = phi i64 [ %8, %7 ], [ %12, %9 ]
  ret i64 %14
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i64 @safe_sub_func_uint64_t_u_u(i64 noundef %0, i64 noundef %1) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, ptr %3, align 8
  store i64 %1, ptr %4, align 8
  %5 = load i64, ptr %3, align 8
  %6 = load i64, ptr %4, align 8
  %7 = sub i64 %5, %6
  ret i64 %7
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i8 @safe_rshift_func_uint8_t_u_s(i8 noundef zeroext %0, i32 noundef %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i32, align 4
  store i8 %0, ptr %3, align 1
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = icmp slt i32 %5, 0
  br i1 %6, label %10, label %7

7:                                                ; preds = %2
  %8 = load i32, ptr %4, align 4
  %9 = icmp sge i32 %8, 32
  br i1 %9, label %10, label %13

10:                                               ; preds = %7, %2
  %11 = load i8, ptr %3, align 1
  %12 = zext i8 %11 to i32
  br label %18

13:                                               ; preds = %7
  %14 = load i8, ptr %3, align 1
  %15 = zext i8 %14 to i32
  %16 = load i32, ptr %4, align 4
  %17 = ashr i32 %15, %16
  br label %18

18:                                               ; preds = %13, %10
  %19 = phi i32 [ %12, %10 ], [ %17, %13 ]
  %20 = trunc i32 %19 to i8
  ret i8 %20
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i64 @safe_sub_func_int64_t_s_s(i64 noundef %0, i64 noundef %1) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, ptr %3, align 8
  store i64 %1, ptr %4, align 8
  %5 = load i64, ptr %3, align 8
  %6 = load i64, ptr %4, align 8
  %7 = xor i64 %5, %6
  %8 = load i64, ptr %3, align 8
  %9 = load i64, ptr %3, align 8
  %10 = load i64, ptr %4, align 8
  %11 = xor i64 %9, %10
  %12 = and i64 %11, -9223372036854775808
  %13 = xor i64 %8, %12
  %14 = load i64, ptr %4, align 8
  %15 = sub nsw i64 %13, %14
  %16 = load i64, ptr %4, align 8
  %17 = xor i64 %15, %16
  %18 = and i64 %7, %17
  %19 = icmp slt i64 %18, 0
  br i1 %19, label %20, label %22

20:                                               ; preds = %2
  %21 = load i64, ptr %3, align 8
  br label %26

22:                                               ; preds = %2
  %23 = load i64, ptr %3, align 8
  %24 = load i64, ptr %4, align 8
  %25 = sub nsw i64 %23, %24
  br label %26

26:                                               ; preds = %22, %20
  %27 = phi i64 [ %21, %20 ], [ %25, %22 ]
  ret i64 %27
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i8 @safe_mod_func_uint8_t_u_u(i8 noundef zeroext %0, i8 noundef zeroext %1) #0 {
  %3 = alloca i8, align 1
  %4 = alloca i8, align 1
  store i8 %0, ptr %3, align 1
  store i8 %1, ptr %4, align 1
  %5 = load i8, ptr %4, align 1
  %6 = zext i8 %5 to i32
  %7 = icmp eq i32 %6, 0
  br i1 %7, label %8, label %11

8:                                                ; preds = %2
  %9 = load i8, ptr %3, align 1
  %10 = zext i8 %9 to i32
  br label %17

11:                                               ; preds = %2
  %12 = load i8, ptr %3, align 1
  %13 = zext i8 %12 to i32
  %14 = load i8, ptr %4, align 1
  %15 = zext i8 %14 to i32
  %16 = srem i32 %13, %15
  br label %17

17:                                               ; preds = %11, %8
  %18 = phi i32 [ %10, %8 ], [ %16, %11 ]
  %19 = trunc i32 %18 to i8
  ret i8 %19
}

; Function Attrs: noinline nounwind optnone uwtable
define internal i32 @safe_sub_func_uint32_t_u_u(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %3, align 4
  %6 = load i32, ptr %4, align 4
  %7 = sub i32 %5, %6
  ret i32 %7
}

; Function Attrs: noinline nounwind optnone uwtable
define internal zeroext i16 @safe_add_func_uint16_t_u_u(i16 noundef zeroext %0, i16 noundef zeroext %1) #0 {
  %3 = alloca i16, align 2
  %4 = alloca i16, align 2
  store i16 %0, ptr %3, align 2
  store i16 %1, ptr %4, align 2
  %5 = load i16, ptr %3, align 2
  %6 = zext i16 %5 to i32
  %7 = load i16, ptr %4, align 2
  %8 = zext i16 %7 to i32
  %9 = add nsw i32 %6, %8
  %10 = trunc i32 %9 to i16
  ret i16 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define internal void @crc32_8bytes(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, ptr %2, align 8
  %3 = load i64, ptr %2, align 8
  %4 = lshr i64 %3, 0
  %5 = and i64 %4, 255
  %6 = trunc i64 %5 to i8
  call void @crc32_byte(i8 noundef zeroext %6)
  %7 = load i64, ptr %2, align 8
  %8 = lshr i64 %7, 8
  %9 = and i64 %8, 255
  %10 = trunc i64 %9 to i8
  call void @crc32_byte(i8 noundef zeroext %10)
  %11 = load i64, ptr %2, align 8
  %12 = lshr i64 %11, 16
  %13 = and i64 %12, 255
  %14 = trunc i64 %13 to i8
  call void @crc32_byte(i8 noundef zeroext %14)
  %15 = load i64, ptr %2, align 8
  %16 = lshr i64 %15, 24
  %17 = and i64 %16, 255
  %18 = trunc i64 %17 to i8
  call void @crc32_byte(i8 noundef zeroext %18)
  %19 = load i64, ptr %2, align 8
  %20 = lshr i64 %19, 32
  %21 = and i64 %20, 255
  %22 = trunc i64 %21 to i8
  call void @crc32_byte(i8 noundef zeroext %22)
  %23 = load i64, ptr %2, align 8
  %24 = lshr i64 %23, 40
  %25 = and i64 %24, 255
  %26 = trunc i64 %25 to i8
  call void @crc32_byte(i8 noundef zeroext %26)
  %27 = load i64, ptr %2, align 8
  %28 = lshr i64 %27, 48
  %29 = and i64 %28, 255
  %30 = trunc i64 %29 to i8
  call void @crc32_byte(i8 noundef zeroext %30)
  %31 = load i64, ptr %2, align 8
  %32 = lshr i64 %31, 56
  %33 = and i64 %32, 255
  %34 = trunc i64 %33 to i8
  call void @crc32_byte(i8 noundef zeroext %34)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define internal void @crc32_byte(i8 noundef zeroext %0) #0 {
  %2 = alloca i8, align 1
  store i8 %0, ptr %2, align 1
  %3 = load i32, ptr @crc32_context, align 4
  %4 = lshr i32 %3, 8
  %5 = and i32 %4, 16777215
  %6 = load i32, ptr @crc32_context, align 4
  %7 = load i8, ptr %2, align 1
  %8 = zext i8 %7 to i32
  %9 = xor i32 %6, %8
  %10 = and i32 %9, 255
  %11 = zext i32 %10 to i64
  %12 = getelementptr inbounds nuw [256 x i32], ptr @crc32_tab, i64 0, i64 %11
  %13 = load i32, ptr %12, align 4
  %14 = xor i32 %5, %13
  store i32 %14, ptr @crc32_context, align 4
  ret void
}

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nounwind willreturn memory(read) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #3 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #5 = { nounwind willreturn memory(read) }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"Homebrew clang version 21.1.8"}
!6 = distinct !{!6, !7}
!7 = !{!"llvm.loop.mustprogress"}
!8 = distinct !{!8, !7}
!9 = distinct !{!9, !7}
!10 = distinct !{!10, !7}
!11 = distinct !{!11, !7}
!12 = distinct !{!12, !7}
!13 = distinct !{!13, !7}
!14 = distinct !{!14, !7}
!15 = distinct !{!15, !7}
!16 = distinct !{!16, !7}
!17 = distinct !{!17, !7}
!18 = distinct !{!18, !7}
!19 = distinct !{!19, !7}
!20 = distinct !{!20, !7}
!21 = distinct !{!21, !7}
!22 = distinct !{!22, !7}
!23 = distinct !{!23, !7}
!24 = distinct !{!24, !7}
!25 = distinct !{!25, !7}
!26 = distinct !{!26, !7}
!27 = distinct !{!27, !7}
!28 = distinct !{!28, !7}
!29 = distinct !{!29, !7}
!30 = distinct !{!30, !7}
!31 = distinct !{!31, !7}
!32 = distinct !{!32, !7}
!33 = distinct !{!33, !7}
!34 = distinct !{!34, !7}
!35 = distinct !{!35, !7}
!36 = distinct !{!36, !7}
!37 = distinct !{!37, !7}
!38 = distinct !{!38, !7}
!39 = distinct !{!39, !7}
!40 = distinct !{!40, !7}
!41 = distinct !{!41, !7}
!42 = distinct !{!42, !7}
!43 = distinct !{!43, !7}
!44 = distinct !{!44, !7}
!45 = distinct !{!45, !7}
!46 = distinct !{!46, !7}
!47 = distinct !{!47, !7}
!48 = distinct !{!48, !7}
!49 = distinct !{!49, !7}
!50 = distinct !{!50, !7}
!51 = distinct !{!51, !7}
!52 = distinct !{!52, !7}
!53 = distinct !{!53, !7}
!54 = distinct !{!54, !7}
!55 = distinct !{!55, !7}
!56 = distinct !{!56, !7}
!57 = distinct !{!57, !7}
!58 = distinct !{!58, !7}
!59 = distinct !{!59, !7}
!60 = distinct !{!60, !7}
!61 = distinct !{!61, !7}
!62 = distinct !{!62, !7}
!63 = distinct !{!63, !7}
!64 = distinct !{!64, !7}
!65 = distinct !{!65, !7}
!66 = distinct !{!66, !7}
!67 = distinct !{!67, !7}
!68 = distinct !{!68, !7}
!69 = distinct !{!69, !7}
