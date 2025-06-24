* generated 2017-10-27 04:52:43.870966 (EDT)
RegInstr "vl      " [19,2,15] [288] (Just 0,"")
RegInstr "vl      " [15,2,15] [272] (Just 0,"")
RegInstr "vl      " [24,2,15] [256] (Just 0,"")
RegInstr "vl      " [16,2,15] [240] (Just 0,"")
RegInstr "vl      " [8,2,15] [224] (Just 0,"")
RegInstr "vl      " [7,2,15] [208] (Just 0,"")
RegInstr "vl      " [6,2,15] [192] (Just 0,"")
RegInstr "vl      " [5,2,15] [176] (Just 0,"")
RegInstr "vl      " [4,2,15] [160] (Just 0,"")
RegInstr "vl      " [17,2,15] [144] (Just 0,"")
RegInstr "vl      " [14,2,15] [128] (Just 0,"")
RegInstr "vl      " [13,2,15] [112] (Just 0,"")
RegInstr "vl      " [11,2,15] [96] (Just 0,"")
RegInstr "vl      " [21,2,15] [80] (Just 0,"")
RegInstr "vl      " [12,2,15] [64] (Just 0,"")
RegInstr "vl      " [3,2,15] [48] (Just 0,"")
RegInstr "vl      " [2,2,15] [32] (Just 0,"")
RegInstr "vl      " [1,2,15] [16] (Just 0,"")
RegInstr "vl      " [0,2,15] [0] (Just 0,"")
    VREPIB  $0x4,M0*i28 <  > i50,i64,i67,i88,i116,i128,i130,i150
    VGBM    $0xff1f,T_3*i29 <  > i31,i69,i111,i132
    VPERM   m3,h2_1,EX2,T_1*i30 < i1,i14,i17 > i31
    VPERM   m3,h0_1,EX0,h2_1*i37 < i1,i12,i15 > i38
    VESLG   $0x4,h1_1,T_5*i42 < i13 > i43
    VGBM    $0x3f,M1*i51 <  > i52
    VREPIB  $0x2,h0_1*i80 <  > i82,i93,i144,i155
    VPERM   m2,h0_0,EX0,T_0*i99 < i0,i9,i15 > i100
    VESLG   $0x4,h1_0,h1_1*i104 < i10 > i105
    VPERM   m2,h2_0,EX2,T_4*i110 < i0,i11,i17 > i111
    VGBM    $0xff3f,h0_0*i35 <  > i36
    VPERM   m5,V0,EX0,h2_0*i39 < i3,i18,i15 > i40
    VESRLG  $0x4,M1,M1*i52 < i51 > i61,i65,i85,i89,i96,i125,i147,i151,i
    VPERM   m5,V0,EX1,h1_0*i53 < i3,i18,i16 > i54
    VPERM   m5,V0,EX2,m5*i68 < i3,i18,i17 > i69
    VPERM   m4,V0,EX0,T_2*i101 < i2,i18,i15 > i102
    VPERM   m2,h1_1,EX1,h1_1*i105 < i0,i104,i16 > i106
    VPERM   m4,V0,EX1,m2*i117 < i2,i18,i16 > i118
    VPERM   m4,V0,EX2,m4*i131 < i2,i18,i17 > i132
    VESRLG  $0x4,h0_0,h0_0*i36 < i35 > i38,i40,i45,i55,i100,i102,i107,i
    VESRLG  $0x4,h1_0,h1_0*i54 < i53 > i55
    VN      T_3,m5,m5*i69 < i29,i68 > i70
    VESRLG  $0x4,h1_1,h1_1*i106 < i105 > i107
    VN      T_3,m4,m4*i132 < i29,i131 > i133
    VN      h0_0,h2_1,h2_1*i38 < i36,i37 > i41,i56,i71
    VN      h0_0,h1_0,h1_0*i55 < i36,i54 > i56
    VLEIB   $0xa,$0x1,m5// ?= m5*i70 < i69 > i71
    VN      h0_0,T_0,T_0*i100 < i36,i99 > i103,i120,i134
    VLEIB   $0xa,$0x1,m4// ?= m4*i133 < i132 > i134
    VMSLG   h2_1,r_2,m5,m5*i71 < i38,i6,i70 > i72
    VMSLG   T_0,r_2,m4,m4*i134 < i100,i6,i133 > i135
    VPERM   m3,T_5,EX1,m3*i43 < i1,i42,i16 > i44
    VESRLG  $0x4,m3,m3*i44 < i43 > i45
    VESRLG  $0x4,m2,m2*i118 < i117 > i119
    VN      T_3,T_1,T_1*i31 < i29,i30 > i32
    VN      h0_0,h2_0,h2_0*i40 < i36,i39 > i41
    VN      h0_0,m3,m3*i45 < i36,i44 > i46,i57,i73
    VN      T_3,T_4,notRewritten29*i111 < i29,i110 > i112
    VLEIB   $0xa,$0x1,T_1// ?= T_1*i32 < i31 > i33,i59,i72
    VMSLG   h2_1,r_0,h2_0,T_6*i41 < i38,i4,i40 > i47
    VMSLG   m3,r5_2,V0,h2_0*i46 < i45,i8,i18 > i47
    VMSLG   h2_1,r_1,h1_0,T_4*i56 < i38,i5,i55 > i58
    VLEIB   $0xa,$0x1,notRewritten29// ?= notRewritten29*i112 < i111 > 
    VMSLG   T_1,r5_1,V0,h2_1*i33 < i32,i7,i18 > i34,i48
    VMSLG   m3,r_0,V0,h1_0*i57 < i45,i4,i18 > i58
    VN      h0_0,T_2,T_7*i102 < i36,i101 > i103
    VN      h0_0,h1_1,T_2*i107 < i36,i106 > i108,i121,i136
    VN      h0_0,m2,h0_0*i119 < i36,i118 > i120
    VMSLG   T_1,r5_2,V0,h1_1*i59 < i32,i8,i18 > i60
    VMSLG   T_0,r_1,h0_0,T_3*i120 < i100,i5,i119 > i122
    VMSLG   T_2,r_0,V0,h0_0*i121 < i107,i4,i18 > i122
    VMSLG   T_2,r_1,V0,m2*i136 < i107,i5,i18 > i137
    VMSLG   T_1,r_0,m5,T_1*i72 < i32,i4,i71 > i74
    VMSLG   m3,r_1,V0,T_5*i73 < i45,i5,i18 > i74
    VMSLG   notRewritten29,r5_1,V0,m3*i113 < i112,i7,i18 > i114
    VMSLG   notRewritten29,r5_2,V0,m5*i123 < i112,i8,i18 > i124
    VMSLG   notRewritten29,r_0,m4,m4*i135 < i112,i4,i134 > i137
    VMSLG   T_0,r_0,T_7,T_0*i103 < i100,i4,i102 > i109
    VMSLG   T_2,r5_2,V0,T_2*i108 < i107,i8,i18 > i109
    VAQ     T_6,h2_0,h2_0*i47 < i41,i46 > i48
    VAQ     h2_0,h2_1,h2_0*i48 < i47,i33 > i49,i85
    VLEIB   $0x7,$0x28,h2_1// ?= h2_1*i34 < i33 > i49,i63,i66,i81,i87,i
    VAQ     T_4,h1_0,T_4*i58 < i56,i57 > i60
    VSRLB   h2_1,h2_0,h1_0*i49 < i34,i48 > i50
    VAQ     T_4,h1_1,h1_1*i60 < i58,i59 > i61,i66
    VN      M1,h2_0,h2_0*i85 < i52,i48 > i86
    VAQ     T_3,h0_0,T_4*i122 < i120,i121 > i124
    VSRL    M0,h1_0,T_3*i50 < i28,i49 > i62
    VN      M1,h1_1,h1_0*i61 < i52,i60 > i62
    VESRLG  $0x2,M1,h0_0*i65 < i52 > i75,i77,i138,i140
    VSRLB   h2_1,h1_1,h1_1*i66 < i34,i60 > i67
    VAQ     T_1,T_5,T_1*i74 < i72,i73 > i75,i81
    VAQ     T_4,m5,m5*i124 < i122,i123 > i125,i129
    VAQ     m4,m2,m2*i137 < i135,i136 > i138,i143
    VAQ     T_3,h1_0,h1_0*i62 < i50,i61 > i63,i89
    VSRL    M0,h1_1,T_4*i67 < i28,i66 > i76
    VN      h0_0,T_1,h1_1*i75 < i65,i74 > i76
    VSRLB   h2_1,T_1,T_1*i81 < i34,i74 > i82
    VAQ     T_0,T_2,m4*i109 < i103,i108 > i114
    VN      M1,m5,T_0*i125 < i52,i124 > i126
    VSRLB   h2_1,m5,T_3*i129 < i34,i124 > i130
    VSRLB   h2_1,m2,T_2*i143 < i34,i137 > i144
    VSRLB   h2_1,h1_0,m5*i63 < i34,i62 > i64
    VAQ     T_4,h1_1,h1_1*i76 < i67,i75 > i77,i92
    VSRL    h0_1,T_1,T_1*i82 < i80,i81 > i83,i84
    VN      M1,h1_0,h1_0*i89 < i52,i62 > i90
    VAQ     m4,m3,m3*i114 < i109,i113 > i115,i147
    VSRL    M0,T_3,T_3*i130 < i28,i129 > i139
    VN      h0_0,m2,m2*i138 < i65,i137 > i139
    VSRL    h0_1,T_2,T_2*i144 < i80,i143 > i145,i146
    VESLG   $0x2,T_1,T_5*i83 < i82 > i84
    VSRLB   h2_1,h1_1,T_4*i92 < i34,i76 > i93
    VSRLB   h2_1,m3,m4*i115 < i34,i114 > i116
    VAQ     T_3,m2,m2*i139 < i130,i138 > i140,i154
    VESLG   $0x2,T_2,T_3*i145 < i144 > i146
    VN      M1,m3,m3*i147 < i52,i114 > i148
    VN      h0_0,h1_1,h1_1*i77 < i65,i76 > i78
    VAQ     T_1,T_5,T_1*i84 < i82,i83 > i86
    VSRL    h0_1,T_4,T_4*i93 < i80,i92 > i94,i95
    VSRL    M0,m4,m4*i116 < i28,i115 > i126
    VN      h0_0,m2,h0_0*i140 < i65,i139 > i141
    VAQ     T_2,T_3,T_2*i146 < i144,i145 > i148
    VSRLB   h2_1,m2,m2*i154 < i34,i139 > i155
    VAQ     T_1,h2_0,h2_0*i86 < i84,i85 > i87,i96
    VESLG   $0x2,T_4,T_1*i94 < i93 > i95
    VAQ     m4,T_0,T_0*i126 < i116,i125 > i127,i151
    VAQ     T_2,m3,m3*i148 < i146,i147 > i149,i158
    VSRL    h0_1,m2,h0_1*i155 < i80,i154 > i156,i157
    VSRLB   h2_1,h2_0,m2*i87 < i34,i86 > i88
    VAQ     T_4,T_1,T_1*i95 < i93,i94 > i97
    VN      M1,h2_0,h2_0*i96 < i52,i86 > i97
    VSRLB   h2_1,T_0,T_2*i127 < i34,i126 > i128
    VSRLB   h2_1,m3,h2_1*i149 < i34,i148 > i150
    VN      M1,T_0,T_0*i151 < i52,i126 > i152
    VESLG   $0x2,h0_1,T_3*i156 < i155 > i157
    VN      M1,m3,M1*i158 < i52,i148 > i159
    VSRL    M0,m5,m3*i64 < i28,i63 > i78
    VSRL    M0,m2,m5*i88 < i28,i87 > i90
    VSRL    M0,T_2,m2*i128 < i28,i127 > i141
    VSRL    M0,h2_1,M0*i150 < i28,i149 > i152
    VAQ     h0_1,T_3,T_2*i157 < i155,i156 > i159
    VAQ     T_1,h2_0,h0_1*i97 < i95,i96 > i98
    VAQ     m3,h1_1,h2_1*i78 < i64,i77 > i79
    VAQ     m5,h1_0,h1_1*i90 < i88,i89 > i91
    VAQ     m2,h0_0,h2_0*i141 < i128,i140 > i142
    VAQ     M0,T_0,h1_0*i152 < i150,i151 > i153
    VAQ     T_2,M1,h0_0*i159 < i157,i158 > i160
("h0_0_2",RegInstr "vst     " [17,5,15] [128] (Just 0,""))
("h1_0_2",RegInstr "vst     " [14,5,15] [144] (Just 0,""))
("h2_0_3",RegInstr "vst     " [13,5,15] [160] (Just 0,""))
("h0_1_2",RegInstr "vst     " [11,5,15] [176] (Just 0,""))
("h1_1_2",RegInstr "vst     " [21,5,15] [192] (Just 0,""))
("h2_1_3",RegInstr "vst     " [12,5,15] [208] (Just 0,""))
("r_0",RegInstr "vst     " [8,5,15] [224] (Just 0,""))
("r_1",RegInstr "vst     " [7,5,15] [240] (Just 0,""))
("r_2",RegInstr "vst     " [6,5,15] [256] (Just 0,""))
("r5_1",RegInstr "vst     " [5,5,15] [272] (Just 0,""))
("r5_2",RegInstr "vst     " [4,5,15] [288] (Just 0,""))
("EX0",RegInstr "vst     " [3,5,15] [304] (Just 0,""))
("EX1",RegInstr "vst     " [2,5,15] [320] (Just 0,""))
("EX2",RegInstr "vst     " [1,5,15] [336] (Just 0,""))
("V0",RegInstr "vst     " [0,5,15] [352] (Just 0,""))
("h0_0_2",159)
("h1_0_2",152)
("h2_0_3",141)
("h0_1_2",97)
("h1_1_2",90)
("h2_1_3",78)
("r_0",4)
("r_1",5)
("r_2",6)
("r5_1",7)
("r5_2",8)
("EX0",15)
("EX1",16)
("EX2",17)
("V0",18)
(("h0_0","h0_0_2"),(9,159))
(("h1_0","h1_0_2"),(10,152))
(("h2_0","h2_0_3"),(11,141))
(("h0_1","h0_1_2"),(12,97))
(("h1_1","h1_1_2"),(13,90))
(("h2_1","h2_1_3"),(14,78))
(19,"m2")
(15,"m3")
(24,"m4")
(16,"m5")
(8,"r_0")
(7,"r_1")
(6,"r_2")
(5,"r5_1")
(4,"r5_2")
(17,"h0_0")
(14,"h1_0")
(13,"h2_0")
(11,"h0_1")
(21,"h1_1")
(12,"h2_1")
(3,"EX0")
(2,"EX1")
(1,"EX2")
(0,"V0")
[(159,9),(152,10),(141,11),(97,12),(90,13),(78,14)]
[("h0_0","h0_0_2"),("h1_0","h1_0_2"),("h2_0","h2_0_3"),("h0_1","h0_1_2"
(12,"m2")
(9,"m3")
(13,"m4")
(19,"m5")
(8,"r_0")
(7,"r_1")
(6,"r_2")
(5,"r5_1")
(4,"r5_2")
(11,"h0_0")
(17,"h1_0")
(14,"h2_0")
(16,"h0_1")
(15,"h1_1")
(10,"h2_1")
(3,"EX0")
(2,"EX1")
(1,"EX2")
(0,"V0")
0
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
28
29
30
37
42
51
80
99
104
110
35
39
52
53
68
101
105
117
131
36
54
69
106
132
38
55
70
100
133
71
134
43
44
118
31
40
45
111
32
41
46
56
112
33
57
102
107
119
59
120
121
136
72
73
113
123
135
103
108
47
48
34
58
49
60
85
122
50
61
65
66
74
124
137
62
67
75
81
109
125
129
143
63
76
82
89
114
130
138
144
83
92
115
139
145
147
77
84
93
116
140
146
154
86
94
126
148
155
87
95
96
127
149
151
156
158
64
88
128
150
157
97
78
90
141
152
159
19
20
21
22
23
24
25
26
27
79
91
98
142
153
160
