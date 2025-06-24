// generated 2018-10-27 04:45:05.973638 (EDT)
DATA p256vmsl<>+0x0(SB)/8, $0x0000000000000000
DATA p256vmsl<>+0x8(SB)/8, $0x0000000000000000
DATA p256vmsl<>+0x10(SB)/8, $0x0000000000000000
DATA p256vmsl<>+0x18(SB)/8, $0x000000000000001b
DATA p256vmsl<>+0x20(SB)/8, $0x0000000000000000
DATA p256vmsl<>+0x28(SB)/8, $0x0000000000111213
DATA p256vmsl<>+0x30(SB)/8, $0x0000000000000011
DATA p256vmsl<>+0x38(SB)/8, $0x1213141516171819
DATA p256vmsl<>+0x40(SB)/8, $0x0000000000001011
DATA p256vmsl<>+0x48(SB)/8, $0x0000121314151617
DATA p256vmsl<>+0x50(SB)/8, $0x0000000000001819
DATA p256vmsl<>+0x58(SB)/8, $0x00001a1b1c1d1e1f
DATA p256vmsl<>+0x60(SB)/8, $0x0000000008090a0b
DATA p256vmsl<>+0x68(SB)/8, $0x0c0d0e0f10111213
DATA p256vmsl<>+0x70(SB)/8, $0x0000000014151617
DATA p256vmsl<>+0x78(SB)/8, $0x18191a1b1c1d1e1f
DATA p256vmsl<>+0x80(SB)/8, $0x000000001a1b1c1d
DATA p256vmsl<>+0x88(SB)/8, $0x1e1f000000000000
DATA p256vmsl<>+0x90(SB)/8, $0x00000000ffffffff
DATA p256vmsl<>+0x98(SB)/8, $0xffffffffffffffff
DATA p256vmsl<>+0xa0(SB)/8, $0x0000141516171819
DATA p256vmsl<>+0xa8(SB)/8, $0x00001a1b1c1d1e1f
DATA p256vmsl<>+0xb0(SB)/8, $0x0405060708090a0b
DATA p256vmsl<>+0xb8(SB)/8, $0x0c0d0e0f14151617
DATA p256vmsl<>+0xc0(SB)/8, $0x0c0d0e0f14151617
DATA p256vmsl<>+0xc8(SB)/8, $0x18191a1b1c1d1e1f
DATA p256vmsl<>+0xd0(SB)/8, $0x1213141516171819
DATA p256vmsl<>+0xd8(SB)/8, $0x00001a1b1c1d1e1f
DATA p256vmsl<>+0xe0(SB)/8, $0xffffffff00000001
DATA p256vmsl<>+0xe8(SB)/8, $0x0000000000000000

GLOBL p256vmsl<>(SB), 8, $240

TEXT p256MulInternal<>(SB), NOSPLIT, $272-0
    VSTM    V16, V31, x-256(SP)
    VST     V15, x-272(SP)
    MOVD    $p256vmsl<>+0x00(SB), CPOOL
    VL      0x0(CPOOL),V31  //resident load //  00
    VL      0x80(CPOOL),V30  //resident load //  1a1b1c1d1e1f000000000000
    VL      0x70(CPOOL),V29  //resident load //  1415161718191a1b1c1d1e1f
    VL      0x30(CPOOL),V28  //resident load //  111213141516171819
    VL      0x20(CPOOL),V27  //resident load //  0111213
    VL      0xa0(CPOOL),V26  //resident load //  1415161718191a1b1c1d1e1f
    VL      0xe0(CPOOL),V25  //resident load //  ffffffff000000010
    VL      0x40(CPOOL),V24  //resident load //  1011121314151617
    VL      0xd0(CPOOL),V23  //resident load //  12131415161718191a1b1c1d1e1f
    VPERM   V31,V25,V24,V4  //i16 < i149,i154,i155 > i17,i73,i85
    VPERM   V31,V1,V24,V6  //i23 < i149,i0,i155 > i24,i75,i87
    VL      0x60(CPOOL),V5  // // i3015 8090a0bc0d0e0f10111213
    VPERM   V31,V0,V26,V7  //i6 < i149,i1,i150 > i7,i9,i22,i52
    VPERM   V1,V0,V5,V1  //i20 < i0,i1,i156 > i21
    VPDI    $0x4,V6,V6,V18  //i75 < i23,i23 > i76,i90,i100
    VPDI    $0x0,V31,V6,V0  //i87 < i149,i23 > i88,i96,i108
    VPERM   V31,V1,V26,V17  //i21 < i149,i20,i150 > i22,i24,i51,i74
    VPDI    $0x1,V17,V6,V14  //i24 < i21,i23 > i28,i97,i109
    VPERM   V31,V3,V24,V6  //i25 < i149,i2,i155 > i26,i78,i86,i90,i97,i101,i110
    VPDI    $0x1,V31,V7,V13  //i7 < i149,i6 > i8,i27,i32
    VPDI    $0x1,V31,V6,V1  //i26 < i149,i25 > i27
    VMSLG   V1,V13,V31,V10  //i27 < i26,i7,i149 > i28
    VPERM   V31,V2,V26,V8  //i4 < i149,i3,i150 > i5,i10,i28,i33,i50,i76,i107
    VPDI    $0x4,V7,V7,V1  //i9 < i6,i6 > i10,i78
    VPERM   V3,V2,V5,V5  //i18 < i2,i3,i156 > i19
    VPDI    $0x1,V31,V8,V3  //i5 < i149,i4 > i8
    VMSLG   V8,V1,V31,V9  //i10 < i4,i9,i149 > i11,i41
    VPERM   V31,V5,V26,V15  //i19 < i149,i18,i150 > i29,i31,i49,i77,i95,i100,i109
    VPDI    $0x4,V8,V8,V19  //i50 < i4,i4 > i51
    VPDI    $0x4,V17,V17,V11  //i74 < i21,i21 > i77,i101
    VMSLG   V8,V18,V31,V5  //i76 < i4,i75,i149 > i77
    VPDI    $0x0,V31,V8,V16  //i107 < i149,i4 > i108
    VMSLG   V3,V13,V31,V3  //i8 < i5,i7,i149 > i12,i44
    VPERM   V9,V9,V30,V2  //i11 < i10,i10,i151 > i12
    VPDI    $0x1,V7,V17,V12  //i22 < i6,i21 > i29,i33,i110
    VMSLG   V8,V14,V10,V10  //i28 < i4,i24,i27 > i29
    VPDI    $0x1,V31,V15,V22  //i31 < i149,i19 > i32
    VPDI    $0x4,V15,V15,V20  //i49 < i19,i19 > i52
    VMSLG   V19,V17,V31,V21  //i51 < i50,i21,i149 > i52
    VMSLG   V15,V11,V5,V19  //i77 < i19,i74,i76 > i78
    VMSLG   V6,V18,V31,V5  //i90 < i25,i75,i149 > i91
    VPDI    $0x0,V31,V15,V17  //i95 < i149,i19 > i96
    VMSLG   V15,V18,V31,V18  //i100 < i19,i75,i149 > i101
    VMSLG   V16,V0,V31,V16  //i108 < i107,i87,i149 > i109
    VAQ     V3,V2,V2  //i12 < i8,i11 > i13
    VMSLG   V15,V12,V10,V10  //i29 < i19,i22,i28 > i30
    VMSLG   V22,V13,V31,V13  //i32 < i31,i7,i149 > i33
    VMSLG   V20,V7,V21,V7  //i52 < i49,i6,i51 > i53,i65
    VMSLG   V6,V1,V19,V1  //i78 < i25,i9,i77 > i79
    VMSLG   V6,V11,V18,V11  //i101 < i25,i74,i100 > i102
    VMSLG   V15,V14,V16,V19  //i109 < i19,i24,i108 > i110
    VPERM   V31,V2,V29,V2  //i13 < i149,i12,i152 > i14
    VMSLG   V8,V12,V13,V16  //i33 < i4,i22,i32 > i37
    VPERM   V31,V2,V23,V2  //i14 < i149,i13,i153 > i15,i34,i79
    VPDI    $0x1,V31,V2,V13  //i15 < i149,i14 > i30,i38,i39
    VPDI    $0x0,V31,V2,V8  //i34 < i149,i14 > i35,i36,i111
    VPERM   V8,V8,V30,V18  //i35 < i34,i34,i151 > i36
    VPERM   V13,V13,V30,V15  //i38 < i15,i15,i151 > i39
    VMSLG   V17,V0,V31,V17  //i96 < i95,i87,i149 > i97
    VSQ     V8,V18,V18  //i36 < i35,i34 > i37,i40
    VSQ     V13,V15,V20  //i39 < i38,i15 > i40,i44
    VAQ     V18,V20,V15  //i40 < i36,i39 > i41
    VAQ     V3,V20,V3  //i44 < i8,i39 > i46
    VAQ     V9,V15,V15  //i41 < i10,i40 > i42,i45
    VPERM   V15,V15,V28,V9  //i42 < i41,i41,i157 > i43
    VPERM   V15,V15,V30,V15  //i45 < i41,i41,i151 > i46
    VAQ     V3,V15,V3  //i46 < i44,i45 > i47
    VPERM   V7,V7,V30,V15  //i53 < i52,i52,i151 > i54
    VMSLG   V6,V12,V19,V12  //i110 < i25,i22,i109 > i111
    VAQ     V16,V18,V16  //i37 < i33,i36 > i43
    VMSLG   V6,V14,V17,V14  //i97 < i25,i24,i96 > i98
    VAQ     V16,V9,V16  //i43 < i37,i42 > i48
    VPERM   V3,V3,V27,V17  //i47 < i46,i46,i158 > i48
    VPDI    $0x1,V31,V4,V3  //i17 < i149,i16 > i30,i99,i112
    VPDI    $0x4,V4,V4,V9  //i73 < i16,i16 > i79,i91,i102
    VPDI    $0x0,V31,V4,V4  //i85 < i149,i16 > i89,i98,i111
    VAQ     V16,V17,V16  //i48 < i43,i47 > i54,i68
    VMSLG   V13,V3,V10,V10  //i30 < i15,i17,i29 > i60
    VAQ     V16,V15,V13  //i54 < i48,i53 > i55
    VPERM   V31,V13,V29,V13  //i55 < i149,i54,i152 > i56
    VPERM   V31,V13,V23,V17  //i56 < i149,i55,i153 > i57,i61,i102
    VPDI    $0x0,V31,V17,V15  //i57 < i149,i56 > i58,i59,i98
    VPDI    $0x1,V31,V17,V13  //i61 < i149,i56 > i62,i63,i112
    VMSLG   V17,V9,V11,V11  //i102 < i56,i73,i101 > i103,i122
    VPDI    $0x0,V31,V6,V18  //i86 < i149,i25 > i88
    VPERM   V15,V15,V30,V17  //i58 < i57,i57,i151 > i59
    VPERM   V13,V13,V30,V6  //i62 < i61,i61,i151 > i63
    VMSLG   V18,V0,V31,V0  //i88 < i86,i87,i149 > i89
    VSQ     V15,V17,V17  //i59 < i58,i57 > i60,i64
    VSQ     V13,V6,V6  //i63 < i62,i61 > i64,i68
    VMSLG   V15,V4,V14,V14  //i98 < i57,i85,i97 > i99
    VAQ     V17,V6,V15  //i64 < i59,i63 > i65
    VAQ     V7,V15,V15  //i65 < i52,i64 > i66,i69
    VPERM   V15,V15,V28,V7  //i66 < i65,i65,i157 > i67
    VPERM   V15,V15,V30,V15  //i69 < i65,i65,i151 > i70
    VAQ     V16,V6,V6  //i68 < i48,i63 > i70
    VAQ     V6,V15,V6  //i70 < i68,i69 > i71
    VMSLG   V8,V4,V12,V12  //i111 < i34,i85,i110 > i112
    VPERM   V11,V11,V30,V8  //i122 < i102,i102,i151 > i123
    VPERM   V6,V6,V27,V6  //i71 < i70,i70,i158 > i72
    VAQ     V10,V17,V10  //i60 < i30,i59 > i67
    VAQ     V10,V7,V7  //i67 < i60,i66 > i72
    VAQ     V7,V6,V6  //i72 < i67,i71 > i81,i124
    VMSLG   V2,V9,V1,V1  //i79 < i14,i73,i78 > i80,i119
    VPERM   V1,V1,V30,V2  //i80 < i79,i79,i151 > i81
    VAQ     V6,V2,V2  //i81 < i72,i80 > i82
    VMSLG   V13,V3,V12,V7  //i112 < i61,i17,i111 > i115
    VPDI    $0x1,V31,V2,V10  //i82 < i149,i81 > i83
    VL      0x50(CPOOL),V2  // // i3014 18191a1b1c1d1e1f
    VPERM   V31,V10,V2,V10  //i83 < i149,i82,i159 > i84,i91,i94
    VPDI    $0x0,V31,V10,V2  //i84 < i149,i83 > i89,i113,i114
    VMSLG   V10,V9,V5,V5  //i91 < i83,i73,i90 > i92,i105
    VPDI    $0x1,V31,V10,V9  //i94 < i149,i83 > i99,i116,i117
    VPERM   V11,V11,V28,V10  //i103 < i102,i102,i157 > i104
    VMSLG   V2,V4,V0,V12  //i89 < i84,i85,i88 > i93
    VPERM   V5,V5,V28,V0  //i92 < i91,i91,i157 > i93
    VPERM   V2,V2,V30,V11  //i113 < i84,i84,i151 > i114
    VPERM   V9,V9,V30,V4  //i116 < i94,i94,i151 > i117
    VAQ     V12,V0,V0  //i93 < i89,i92 > i132
    VSQ     V2,V11,V2  //i114 < i113,i84 > i115,i118
    VSQ     V9,V4,V4  //i117 < i116,i94 > i118,i124
    VAQ     V7,V2,V7  //i115 < i112,i114 > i121
    VAQ     V2,V4,V2  //i118 < i114,i117 > i119
    VAQ     V6,V4,V6  //i124 < i72,i117 > i126
    VAQ     V1,V2,V1  //i119 < i79,i118 > i120,i125
    VPERM   V1,V1,V28,V4  //i120 < i119,i119,i157 > i121
    VPERM   V1,V1,V30,V1  //i125 < i119,i119,i151 > i126
    VAQ     V7,V4,V4  //i121 < i115,i120 > i123
    VAQ     V6,V1,V6  //i126 < i124,i125 > i127,i136
    VMSLG   V9,V3,V14,V1  //i99 < i94,i17,i98 > i104
    VAQ     V4,V8,V4  //i123 < i121,i122 > i128
    VPERM   V6,V6,V27,V3  //i127 < i126,i126,i158 > i128
    VAQ     V1,V10,V1  //i104 < i99,i103 > i106
    VPERM   V5,V5,V30,V5  //i105 < i91,i91,i151 > i106
    VAQ     V4,V3,V3  //i128 < i123,i127 > i129,i135
    VAQ     V1,V5,V1  //i106 < i104,i105 > i130
    VPERM   V3,V3,V27,V5  //i129 < i128,i128,i158 > i130
    VPERM   V31,V6,V29,V6  //i136 < i149,i126,i152 > i137
    VAQ     V1,V5,V5  //i130 < i106,i129 > i131,i133
    VPERM   V31,V3,V29,V1  //i135 < i149,i128,i152 > i137
    VPERM   V5,V5,V27,V3  //i131 < i130,i130,i158 > i132
    VPERM   V31,V5,V29,V5  //i133 < i149,i130,i152 > i134
    VAQ     V0,V3,V0  //i132 < i93,i131 > i134,i140
    VL      0xb0(CPOOL),V2  // // i3013 405060708090a0bc0d0e0f14151617
    VL      0xc0(CPOOL),V4  // // i3012 c0d0e0f1415161718191a1b1c1d1e1f
    VL      0x90(CPOOL),V3  // // i3011 ffffffffffffffffffffffff
    VPERM   V1,V6,V2,V6  //i137 < i135,i136,i161 > i138,i145,i146
    VPERM   V0,V5,V4,V5  //i134 < i132,i133,i160 > i139,i141,i143
    VSCBIQ  V3,V6,V1  //i138 < i137,i162 > i139,i141
    VL      0x10(CPOOL),V3  // // i3010 01b
    VPERM   V31,V0,V3,V3  //i140 < i149,i132,i163 > i142
    VSBIQ   V5,V25,V1,V0  //i139 < i134,i154,i138 > i143
    VSBCBIQ V5,V25,V1,V1  //i141 < i134,i154,i138 > i142
    VSBIQ   V3,V31,V1,V1  //i142 < i140,i149,i141 > i143,i146
    VL      0x90(CPOOL),V3  // // i3009 ffffffffffffffffffffffff
    VSQ     V3,V6,V3  //i145 < i137,i162 > i146
    VSEL    V5,V0,V1,V5  //i143 < i139,i134,i142 > i144
    VSEL    V6,V3,V1,V0  //i146 < i145,i137,i142 > i147
    VLM     x-256(SP), V16, V31
    VL      x-272(SP), V15
    MOVD $p256mul<>+0x00(SB), CPOOL
    RET
