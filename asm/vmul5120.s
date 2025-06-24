// generated 2019-08-28 19:38:18.914829917 (EDT)
PIPELINEDMULP   DS    0L
    VL      0x0(R6,R0),v24  //ns  // i3007 ffffffffffffffffffffffffffff
    VL      0x10(R6,R0),v18  //ns  // i3006 12131415161718191a1b1c1d1e1f
    VL      0x0(R2,R15),v12  //s2 imms[0]
    VGBM    0x0,v0  //s2 i8 <  > i10,i12,i13,i14,i15,i16,i19,i20,i21,i25,i29,i30,i32,i38,i40,i41,i44,i48,i49,i52,i53,i55,i58,i64,i67,i68,i71,i75,i76,i80,i81,i83,i86,i92,i96,i97,i98,i100,i102,i104,i105,i110,i111,i115,i120,i123,i126,i134,i135,i140,i141,i142,i146,i147,i149,i152,i156,i157,i161,i162,i163,i166,i167,i169,i172,i176,i177,i180,i181,i182,i184,i185,i187,i190,i194,i195,i197,i198,i199,i200,i201,i203,i204,i205,i208
    VL      0x10(R2,R15),v6  //s2 imms[16]
    VMSLG   v10,v26,v38,v38  //s0 i79 < i17,i72,i78 > i80,i126
    VSLDB   0x5,v13,v0,v36  //s0 i40 < i39,i8 > i41,i53
    VSLDB   0xa,v38,v0,v26  //s0 i80 < i79,i8 > i82,i84
    VSLDB   0x5,v0,v13,v13  //s0 i83 < i8,i39 > i85,i87
    VMSLG   v10,v22,v19,v19  //s0 i95 < i17,i90,i94 > i96
    VMSLG   v10,v21,v25,v21  //s0 i109 < i17,i101,i108 > i110,i149
    VMSLG   v10,v20,v31,v22  //s0 i119 < i17,i113,i118 > i120,i152
    VSLDB   0xa,v0,v38,v10  //s0 i126 < i8,i79 > i128
    VMSLG   v17,v20,v37,v17  //s0 i139 < i130,i113,i138 > i140,i169
    VMSLG   v1,v20,v30,v20  //s0 i145 < i37,i113,i144 > i146,i172
    VAC     0x4,v0,v36,v0,v30  //s0 i41 < i8,i40,i8 > i54,i56
    VSLDB   0xc,v33,v0,v25  //s0 i52 < i51,i8 > i54,i56
    VACCC   0x4,v0,v36,v0,v36  //s0 i53 < i8,i40,i8 > i54,i56
    VAC     0x4,v34,v26,v35,v31  //s0 i82 < i68,i80,i81 > i85,i87
    VACCC   0x4,v34,v26,v35,v26  //s0 i84 < i68,i80,i81 > i85,i87
    VSLDB   0x1,v19,v0,v34  //s0 i96 < i95,i8 > i97,i111
    VAC     0x4,v30,v25,v36,v19  //s0 i54 < i41,i52,i53 > i57,i59
    VSLDB   0x7,v0,v15,v15  //s0 i55 < i8,i19 > i57,i59
    VACCC   0x4,v30,v25,v36,v35  //s0 i56 < i41,i52,i53 > i57,i59
    VAC     0x4,v0,v34,v0,v30  //s0 i97 < i8,i96,i8 > i112,i121
    VSLDB   0x8,v21,v0,v25  //s0 i110 < i109,i8 > i112,i121
    VACCC   0x4,v0,v34,v0,v37  //s0 i111 < i8,i96,i8 > i112,i121
    VSLDB   0x6,v17,v0,v36  //s0 i140 < i139,i8 > i141,i147
    VSLDB   0xd,v20,v0,v34  //s0 i146 < i145,i8 > i148,i150
    VSLDB   0x8,v0,v21,v21  //s0 i149 < i8,i109 > i151,i153
    VSLDB   0x4,v39,v0,v38  //s0 i161 < i160,i8 > i162,i167
    VSLDB   0x6,v0,v17,v17  //s0 i169 < i8,i139 > i171,i173
    VSLDB   0x4,v0,v39,v39  //s0 i187 < i8,i160 > i189,i191
    VSLDB   0x6,v11,v7,v11  //s1 i73 < i3,i2 > i74
    VAC     0x4,v19,v15,v35,v7  //s0 i57 < i54,i55,i56 > i60
    VACCC   0x4,v19,v15,v35,v19  //s0 i59 < i54,i55,i56 > i60
    VAC     0x4,v31,v13,v26,v15  //s0 i85 < i82,i83,i84 > i88
    VSLDB   0xc,v0,v33,v33  //s0 i86 < i8,i51 > i88
    VACCC   0x4,v31,v13,v26,v31  //s0 i87 < i82,i83,i84 > i88
    VAC     0x4,v30,v25,v37,v26  //s0 i112 < i97,i110,i111 > i122,i124
    VSLDB   0xf,v22,v0,v13  //s0 i120 < i119,i8 > i122,i124
    VACCC   0x4,v30,v25,v37,v30  //s0 i121 < i97,i110,i111 > i122,i124
    VAC     0x4,v0,v36,v0,v25  //s0 i141 < i8,i140,i8 > i148,i150
    VACCC   0x4,v0,v36,v0,v35  //s0 i147 < i8,i140,i8 > i148,i150
    VSLDB   0xf,v0,v22,v22  //s0 i152 < i8,i119 > i154
    VAC     0x4,v0,v38,v0,v37  //s0 i162 < i8,i161,i8 > i168,i170
    VSLDB   0xb,v29,v0,v36  //s0 i166 < i165,i8 > i168,i170
    VACCC   0x4,v0,v38,v0,v38  //s0 i167 < i8,i161,i8 > i168,i170
    VSLDB   0xd,v0,v20,v20  //s0 i172 < i8,i145 > i174
    VAC     0x4,v23,v39,v28,v40  //s0 i189 < i186,i187,i188 > i192
    VSLDB   0xb,v0,v29,v29  //s0 i190 < i8,i165 > i192
    VACCC   0x4,v23,v39,v28,v28  //s0 i191 < i186,i187,i188 > i192
    VN      v32,v24,v23  //s1 i11 < i4,i213 > i12
    VN      v11,v24,v11  //s1 i74 < i73,i213 > i75
    VAC     0x4,v7,v8,v19,v8  //s0 i60 < i57,i58,i59 > i61
    VAC     0x4,v15,v33,v31,v7  //s0 i88 < i85,i86,i87 > i89
    VAC     0x4,v26,v13,v30,v15  //s0 i122 < i112,i120,i121 > i125,i127
    VACCC   0x4,v26,v13,v30,v19  //s0 i124 < i112,i120,i121 > i125,i127
    VAC     0x4,v25,v34,v35,v13  //s0 i148 < i141,i146,i147 > i151,i153
    VACCC   0x4,v25,v34,v35,v30  //s0 i150 < i141,i146,i147 > i151,i153
    VAC     0x4,v37,v36,v38,v24  //s0 i168 < i162,i166,i167 > i171,i173
    VACCC   0x4,v37,v36,v38,v31  //s0 i170 < i162,i166,i167 > i171,i173
    VAC     0x4,v40,v29,v28,v28  //s0 i192 < i189,i190,i191 > i193
    VPERM   v0,v23,v18,v25  //s1 i12 < i8,i11,i214 > i13,i18,i26,i134
    VSLDB   0x2,v9,v32,v23  //s1 i23 < i5,i4 > i24
    VL      0x0(R6,R0),v36  //ns  // i3004 ffffffffffffffffffffffffffff
    VL      0x10(R6,R0),v29  //ns  // i3005 12131415161718191a1b1c1d1e1f
    VPERM   v0,v11,v29,v26  //s1 i75 < i8,i74,i214 > i76,i91,i132,i133
    VPERM   v0,v27,v29,v18  //s1 i104 < i8,i103,i214 > i105,i114,i133,i135,i157,i177,i195,i204
    VST     0x90(R5,R15),v8  //s0 imms[144]
    VAC     0x4,v15,v16,v19,v8  //s0 i125 < i122,i123,i124 > i128
    VACCC   0x4,v15,v16,v19,v11  //s0 i127 < i122,i123,i124 > i128
    VAC     0x4,v13,v21,v30,v33  //s0 i151 < i148,i149,i150 > i154
    VACCC   0x4,v13,v21,v30,v35  //s0 i153 < i148,i149,i150 > i154
    VAC     0x4,v24,v17,v31,v13  //s0 i171 < i168,i169,i170 > i174
    VACCC   0x4,v24,v17,v31,v34  //s0 i173 < i168,i169,i170 > i174
    VPDI    $0x1,v0,v25,v31  //s1 i13 < i8,i12 > i14,i30,i49,i76,i105
    VPDI    $0x1,v25,v25,v15  //s1 i18 < i12,i12 > i19,i38,i64,i92,i115
    VN      v23,v36,v17  //s1 i24 < i23,i213 > i25
    VPDI    $0x4,v26,v26,v23  //s1 i91 < i75,i75 > i92,i106,i116,i143,i164,i183
    VPDI    $0x4,v18,v18,v24  //s1 i114 < i104,i104 > i115,i142,i163,i182,i199
    VPDI    $0x1,v3,v26,v16  //s1 i132 < i48,i75 > i137,i159,i179
    VPDI    $0x0,v25,v0,v27  //s1 i134 < i12,i8 > i135
    VST     0xa0(R5,R15),v7  //s0 imms[160]
    VAC     0x4,v8,v10,v11,v7  //s0 i128 < i125,i126,i127 > i129
    VL      0x70(R2,R15),v21  //s1 imms[112]
    VPERM   v0,v17,v29,v11  //s1 i25 < i8,i24,i214 > i26,i36,i45,i156
    VMSLG   v2,v15,v0,v32  //s1 i64 < i63,i18,i8 > i65
    VMSLG   v26,v31,v0,v8  //s1 i76 < i75,i13,i8 > i77
    VMSLG   v23,v15,v0,v19  //s1 i92 < i91,i18,i8 > i93
    VMSLG   v18,v31,v0,v30  //s1 i105 < i104,i13,i8 > i106
    VMSLG   v24,v15,v0,v17  //s1 i115 < i114,i18,i8 > i116
    VMSLG   v18,v27,v0,v27  //s1 i135 < i104,i134,i8 > i136
    VST     0xb0(R5,R15),v7  //s0 imms[176]
    VAC     0x4,v33,v22,v35,v22  //s0 i154 < i151,i152,i153 > i155
    VPDI    $0x4,v4,v4,v10  //s1 i17 < i10,i10 > i19,i31,i39,i51,i66,i79,i95,i109,i119
    VPDI    $0x4,v11,v25,v25  //s1 i26 < i25,i12 > i31,i50,i77,i106
    VPDI    $0x1,v11,v11,v4  //s1 i36 < i25,i25 > i39,i65,i93,i116,i136,i142
    VMSLG   v1,v15,v0,v33  //s1 i38 < i37,i18,i8 > i39
    VPDI    $0x1,v26,v18,v26  //s1 i133 < i75,i104 > i136,i158,i178,i196
    VPDI    $0x0,v11,v0,v7  //s1 i156 < i25,i8 > i157
    VST     0xc0(R5,R15),v22  //s0 imms[192]
    VAC     0x4,v13,v20,v34,v20  //s0 i174 < i171,i172,i173 > i175
    VMSLG   v10,v4,v33,v13  //s1 i39 < i17,i36,i38 > i40,i83
    VMSLG   v1,v4,v32,v32  //s1 i65 < i37,i36,i64 > i66
    VMSLG   v2,v4,v19,v19  //s1 i93 < i63,i36,i92 > i94
    VMSLG   v23,v25,v30,v35  //s1 i106 < i91,i26,i105 > i107
    VMSLG   v26,v4,v27,v33  //s1 i136 < i133,i36,i135 > i137
    VMSLG   v24,v4,v0,v30  //s1 i142 < i114,i36,i8 > i143
    VMSLG   v18,v7,v0,v1  //s1 i157 < i104,i156,i8 > i158
    VL      0x20(R2,R15),v7  //s2 imms[32]
    VST     0xd0(R5,R15),v20  //s0 imms[208]
    VMSLG   v23,v4,v17,v34  //s1 i116 < i91,i36,i115 > i117
    VST     0xe0(R5,R15),v28  //s0 imms[224]
    VST     0xf0(R5,R15),v14  //s0 imms[240]
    VSLDB   0x4,v5,v9,v4  //s1 i42 < i6,i5 > i43
    VN      v4,v36,v4  //s1 i43 < i42,i213 > i44
    VPERM   v0,v4,v29,v14  //s1 i44 < i8,i43,i214 > i45,i62,i72,i176
    VPDI    $0x4,v14,v11,v9  //s1 i45 < i44,i25 > i51,i78,i107
    VPDI    $0x1,v14,v14,v37  //s1 i62 < i44,i44 > i66,i94,i117,i137,i143,i158,i163
    VPDI    $0x0,v14,v0,v4  //s1 i176 < i44,i8 > i177
    VMSLG   v26,v37,v1,v27  //s1 i158 < i133,i62,i157 > i159
    VMSLG   v24,v37,v0,v29  //s1 i163 < i114,i62,i8 > i164
    VMSLG   v18,v4,v0,v28  //s1 i177 < i104,i176,i8 > i178
    VN      v12,v36,v1  //s2 i9 < i0,i213 > i10
    VSLDB   0x2,v6,v12,v4  //s2 i27 < i1,i0 > i28
    VN      v4,v36,v12  //s2 i28 < i27,i213 > i29
    VL      0x30(R2,R15),v11  //s2 imms[48]
    VL      0x0(R6,R0),v22  //ns  // i3002 ffffffffffffffffffffffffffff
    VL      0x10(R6,R0),v36  //ns  // i3003 12131415161718191a1b1c1d1e1f
    VPERM   v0,v1,v36,v4  //s2 i10 < i8,i9,i214 > i14,i17,i130
    VPERM   v0,v12,v36,v20  //s2 i29 < i8,i28,i214 > i30,i37,i130,i131
    VPDI    $0x4,v20,v20,v1  //s2 i37 < i29,i29 > i38,i50,i65,i78,i94,i108,i118,i145
    VPDI    $0x1,v4,v20,v17  //s2 i130 < i10,i29 > i139
    VMSLG   v4,v31,v0,v12  //s1 i14 < i10,i13,i8 > i15
    VSLDB   0x0,v12,v0,v38  //s1 i15 < i14,i8 > i16,i21
    VMSLG   v10,v15,v0,v15  //s1 i19 < i17,i18,i8 > i20,i55
    VMSLG   v20,v31,v0,v12  //s1 i30 < i29,i13,i8 > i31
    VMSLG   v3,v31,v0,v31  //s1 i49 < i48,i13,i8 > i50
    VAC     0x4,v0,v38,v0,v41  //s1 i16 < i8,i15,i8 > i22,i33
    VSLDB   0x7,v15,v0,v39  //s1 i20 < i19,i8 > i22,i33
    VACCC   0x4,v0,v38,v0,v42  //s1 i21 < i8,i15,i8 > i22,i33
    VMSLG   v10,v25,v12,v40  //s1 i31 < i17,i26,i30 > i32,i58
    VMSLG   v2,v25,v8,v12  //s1 i77 < i63,i26,i76 > i78
    VAC     0x4,v41,v39,v42,v38  //s1 i22 < i16,i20,i21 > i34
    VSLDB   0xe,v40,v0,v3  //s1 i32 < i31,i8 > i34
    VACCC   0x4,v41,v39,v42,v39  //s1 i33 < i16,i20,i21 > i34
    VSLDB   0xe,v0,v40,v8  //s1 i58 < i8,i31 > i60
    VMSLG   v10,v37,v32,v32  //s1 i66 < i17,i62,i65 > i67,i123
    VMSLG   v1,v37,v19,v19  //s1 i94 < i37,i62,i93 > i95
    VMSLG   v2,v9,v35,v35  //s1 i107 < i63,i45,i106 > i108
    VMSLG   v2,v37,v34,v34  //s1 i117 < i63,i62,i116 > i118
    VMSLG   v16,v37,v33,v33  //s1 i137 < i132,i62,i136 > i138
    VMSLG   v23,v37,v30,v30  //s1 i143 < i91,i62,i142 > i144
    VAC     0x4,v38,v3,v39,v2  //s1 i34 < i22,i32,i33 > i35
    VST     0xc0(R5,R15),v2  //s1 imms[128]
    VSLDB   0x4,v7,v6,v2  //s2 i46 < i2,i1 > i47
    VN      v2,v22,v3  //s2 i47 < i46,i213 > i48
    VSLDB   0x6,v21,v5,v2  //s1 i69 < i7,i6 > i70
    VSLDB   0x8,v0,v21,v5  //s1 i98 < i8,i7 > i99
    VPERM   v0,v3,v36,v3  //s2 i48 < i8,i47,i214 > i49,i63,i131,i132
    VN      v2,v22,v2  //s1 i70 < i69,i213 > i71
    VN      v5,v22,v21  //s1 i99 < i98,i213 > i100
    VL      0x10(R6,R0),v5  //ns  // i3001 12131415161718191a1b1c1d1e1f
    VPERM   v0,v2,v5,v6  //s1 i71 < i8,i70,i214 > i72,i90,i101,i194
    VPERM   v0,v21,v5,v36  //s1 i100 < i8,i99,i214 > i101,i113,i203
    VPDI    $0x4,v3,v3,v2  //s2 i63 < i48,i48 > i64,i77,i93,i107,i117,i144,i165
    VPDI    $0x1,v20,v3,v5  //s2 i131 < i29,i48 > i138,i160
    VPDI    $0x1,v6,v6,v22  //s1 i90 < i71,i71 > i95,i118,i138,i144,i159,i164,i178,i182
    VPDI    $0x4,v36,v6,v21  //s1 i101 < i100,i71 > i109
    VPDI    $0x1,v36,v36,v20  //s1 i113 < i100,i100 > i119,i139,i145,i160,i165,i179,i183,i196,i199
    VPDI    $0x0,v6,v0,v37  //s1 i194 < i71,i8 > i195
    VPDI    $0x0,v36,v0,v36  //s1 i203 < i100,i8 > i204
    VMSLG   v23,v22,v29,v29  //s1 i164 < i91,i90,i163 > i165
    VMSLG   v24,v22,v0,v38  //s1 i182 < i114,i90,i8 > i183
    VMSLG   v18,v37,v0,v37  //s1 i195 < i104,i194,i8 > i196
    VMSLG   v24,v20,v0,v24  //s1 i199 < i114,i113,i8 > i200
    VMSLG   v18,v36,v0,v18  //s1 i204 < i104,i203,i8 > i205
    VMSLG   v23,v20,v38,v23  //s1 i183 < i91,i113,i182 > i184,i208
    VMSLG   v26,v20,v37,v36  //s1 i196 < i133,i113,i195 > i197
    VSLDB   0xe,v18,v0,v18  //s1 i205 < i204,i8 > i207,i209
    VMSLG   v26,v22,v28,v26  //s1 i178 < i133,i90,i177 > i179
    VMSLG   v16,v22,v27,v27  //s1 i159 < i132,i90,i158 > i160
    VMSLG   v16,v20,v26,v28  //s1 i179 < i132,i113,i178 > i180
    VPDI    $0x4,v6,v14,v26  //s1 i72 < i71,i44 > i79,i108
    VMSLG   v1,v25,v31,v14  //s1 i50 < i37,i26,i49 > i51
    VSLDB   0x3,v32,v0,v6  //s1 i67 < i66,i8 > i68,i81
    VMSLG   v1,v26,v35,v25  //s1 i108 < i37,i72,i107 > i109
    VMSLG   v1,v22,v34,v31  //s1 i118 < i37,i90,i117 > i119
    VSLDB   0x3,v0,v32,v16  //s1 i123 < i8,i66 > i125,i127
    VMSLG   v5,v22,v33,v37  //s1 i138 < i131,i90,i137 > i139
    VMSLG   v10,v9,v14,v33  //s1 i51 < i17,i45,i50 > i52,i86
    VMSLG   v1,v9,v12,v38  //s1 i78 < i37,i45,i77 > i79
    VAC     0x4,v0,v6,v0,v34  //s1 i68 < i8,i67,i8 > i82,i84
    VACCC   0x4,v0,v6,v0,v35  //s1 i81 < i8,i67,i8 > i82,i84
    VL      0x40(R2,R15),v32  //s2 imms[64]
    VSLDB   0x8,v0,v11,v6  //s2 i102 < i8,i3 > i103
    VMSLG   v5,v20,v27,v39  //s1 i160 < i131,i113,i159 > i161,i187
    VL      0x50(R2,R15),v9  //s2 imms[80]
    VL      0x0(R6,R0),v5  //ns  // i3000 ffffffffffffffffffffffffffff
    VN      v6,v5,v27  //s2 i103 < i102,i213 > i104
    VMSLG   v2,v22,v30,v30  //s1 i144 < i63,i90,i143 > i145
    VMSLG   v2,v20,v29,v29  //s1 i165 < i63,i113,i164 > i166,i190
    VSLDB   0x9,v23,v0,v6  //s1 i184 < i183,i8 > i186,i188
    VSLDB   0x0,v36,v0,v5  //s1 i197 < i196,i8 > i198,i201
    VSLDB   0x9,v0,v23,v12  //s1 i208 < i8,i183 > i210
    VAC     0x4,v0,v5,v0,v14  //s1 i198 < i8,i197,i8 > i202,i206
    VSLDB   0x7,v24,v0,v23  //s1 i200 < i199,i8 > i202,i206
    VACCC   0x4,v0,v5,v0,v24  //s1 i201 < i8,i197,i8 > i202,i206
    VAC     0x4,v14,v23,v24,v5  //s1 i202 < i198,i200,i201 > i207,i209
    VACCC   0x4,v14,v23,v24,v23  //s1 i206 < i198,i200,i201 > i207,i209
    VAC     0x4,v5,v18,v23,v14  //s1 i207 < i202,i205,i206 > i210
    VACCC   0x4,v5,v18,v23,v18  //s1 i209 < i202,i205,i206 > i210
    VSLDB   0x2,v28,v0,v23  //s1 i180 < i179,i8 > i181,i185
    VL      0x60(R2,R15),v5  //s2 imms[96]
    VAC     0x4,v14,v12,v18,v14  //s1 i210 < i207,i208,i209 > i211
    VAC     0x4,v0,v23,v0,v12  //s1 i181 < i8,i180,i8 > i186,i188
    VACCC   0x4,v0,v23,v0,v0  //s1 i185 < i8,i180,i8 > i186,i188
    VAC     0x4,v12,v6,v0,v23  //s1 i186 < i181,i184,i185 > i189,i191
    VACCC   0x4,v12,v6,v0,v28  //s1 i188 < i181,i184,i185 > i189,i191
    BCR condition, LINKREGISTER // return, either set condition which is always true, or find out from Bill a condition code which is always true
CONSTANTS   DS    0L
    DC    XL8'ffffffffffff'
    DC    XL8'ffffffffffffffff'
    DC    XL8'12131415161718'
    DC    XL8'191a1b1c1d1e1f'
