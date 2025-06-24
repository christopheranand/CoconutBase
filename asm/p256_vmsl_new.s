DATA p256vmsl<>+0x0(SB)/8, $0x0000000000000000
DATA p256vmsl<>+0x8(SB)/8, $0x0000000000000000
DATA p256vmsl<>+0x10(SB)/8, $0x0000000000000000
DATA p256vmsl<>+0x18(SB)/8, $0x000000000000001b
DATA p256vmsl<>+0x20(SB)/8, $0x0000000000000000
DATA p256vmsl<>+0x28(SB)/8, $0x0000000010111213
DATA p256vmsl<>+0x30(SB)/8, $0x0000000000001011
DATA p256vmsl<>+0x38(SB)/8, $0x0000121314151617
DATA p256vmsl<>+0x40(SB)/8, $0x0000000000001011
DATA p256vmsl<>+0x48(SB)/8, $0x1213141516171819
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

TEXT p256MulInternal<>(SB), NOSPLIT, $224-0
    VSTM V16, V29, x-224(SP)

    MOVD    $p256vmsl<>+0x00(SB), R7
    VLR     X1, V26
    VLR     X0, V27 
    VLR     Y1, V28 
    VLR     Y0, V29
    
    VLR     V26, V2  //
    VL      0x0(R7),V0  // // i3065 00
    VL      0xe0(R7),V1  // // i3064 ffffffff000000010
    VL      0x30(R7),V3  // // i3063 1011121314151617
    VPERM   V0,V1,V3,V4  //i16 < i149,i154,i155 > i17,i73,i85
    VLR     V27, V1  //
    VLR     V28, V5  //
    VL      0x0(R7),V3  // // i3062 00
    VL      0xa0(R7),V6  // // i3061 1415161718191a1b1c1d1e1f
    VL      0x60(R7),V0  // // i3060 8090a0bc0d0e0f10111213
    VPERM   V3,V1,V6,V6  //i6 < i149,i1,i150 > i7,i9,i22,i52
    VPERM   V2,V1,V0,V3  //i20 < i0,i1,i156 > i21
    VL      0x30(R7),V7  // // i3059 1011121314151617
    VL      0x0(R7),V0  // // i3058 00
    VL      0xa0(R7),V1  // // i3057 1415161718191a1b1c1d1e1f
    VPERM   V0,V2,V7,V2  //i23 < i149,i0,i155 > i24,i75,i87
    VPERM   V0,V3,V1,V1  //i21 < i149,i20,i150 > i22,i24,i51,i74
    VLR     V29, V3  //
    VL      0x80(R7),V9  // // i3047 1a1b1c1d1e1f000000000000
    VL      0x0(R7),V7  // // i3056 00
    VL      0xa0(R7),V8  // // i3055 1415161718191a1b1c1d1e1f
    VL      0x60(R7),V0  // // i3054 8090a0bc0d0e0f10111213
    VPERM   V7,V3,V8,V13  //i4 < i149,i3,i150 > i5,i10,i28,i33,i50,i76,i107
    VPERM   V5,V3,V0,V7  //i18 < i2,i3,i156 > i19
    VL      0x30(R7),V8  // // i3053 1011121314151617
    VL      0xa0(R7),V0  // // i3052 1415161718191a1b1c1d1e1f
    VL      0x0(R7),V3  // // i3051 00
    VPERM   V3,V5,V8,V5  //i25 < i149,i2,i155 > i26,i78,i86,i90,i97,i101,i110
    VPERM   V3,V7,V0,V12  //i19 < i149,i18,i150 > i29,i31,i49,i77,i95,i100,i109
    VPDI    $0x1,V3,V13,V10  //i5 < i149,i4 > i8
    VPDI    $0x1,V3,V6,V7  //i7 < i149,i6 > i8,i27,i32
    VPDI    $0x1,V3,V5,V8  //i26 < i149,i25 > i27
    VPDI    $0x1,V3,V12,V0  //i31 < i149,i19 > i32
    VMSLG   V10,V7,V3,V10  //i8 < i5,i7,i149 > i12,i44
    VMSLG   V8,V7,V3,V11  //i27 < i26,i7,i149 > i28
    VMSLG   V0,V7,V3,V17  //i32 < i31,i7,i149 > i33
    VPDI    $0x1,V1,V2,V15  //i24 < i21,i23 > i28,i97,i109
    VPDI    $0x4,V13,V13,V8  //i50 < i4,i4 > i51
    VPDI    $0x4,V2,V2,V19  //i75 < i23,i23 > i76,i90,i100
    VPDI    $0x0,V3,V5,V0  //i86 < i149,i25 > i88
    VPDI    $0x0,V3,V2,V18  //i87 < i149,i23 > i88,i96,i108
    VPDI    $0x0,V3,V12,V7  //i95 < i149,i19 > i96
    VPDI    $0x0,V3,V13,V2  //i107 < i149,i4 > i108
    VPDI    $0x1,V6,V1,V14  //i22 < i6,i21 > i29,i33,i110
    VMSLG   V13,V15,V11,V11  //i28 < i4,i24,i27 > i29
    VPDI    $0x4,V12,V12,V20  //i49 < i19,i19 > i52
    VMSLG   V8,V1,V3,V21  //i51 < i50,i21,i149 > i52
    VPDI    $0x4,V1,V1,V8  //i74 < i21,i21 > i77,i101
    VMSLG   V13,V19,V3,V16  //i76 < i4,i75,i149 > i77
    VMSLG   V0,V18,V3,V0  //i88 < i86,i87,i149 > i89
    VMSLG   V5,V19,V3,V1  //i90 < i25,i75,i149 > i91
    VMSLG   V7,V18,V3,V7  //i96 < i95,i87,i149 > i97
    VMSLG   V12,V19,V3,V19  //i100 < i19,i75,i149 > i101
    VMSLG   V2,V18,V3,V18  //i108 < i107,i87,i149 > i109
    VPDI    $0x4,V6,V6,V2  //i9 < i6,i6 > i10,i78
    VMSLG   V12,V14,V11,V11  //i29 < i19,i22,i28 > i30
    VMSLG   V20,V6,V21,V6  //i52 < i49,i6,i51 > i53,i65
    VMSLG   V12,V8,V16,V16  //i77 < i19,i74,i76 > i78
    VMSLG   V5,V15,V7,V7  //i97 < i25,i24,i96 > i98
    VMSLG   V5,V8,V19,V8  //i101 < i25,i74,i100 > i102
    VMSLG   V12,V15,V18,V15  //i109 < i19,i24,i108 > i110
    VMSLG   V13,V2,V3,V12  //i10 < i4,i9,i149 > i11,i41
    VMSLG   V13,V14,V17,V13  //i33 < i4,i22,i32 > i37
    VMSLG   V5,V2,V16,V2  //i78 < i25,i9,i77 > i79
    VMSLG   V5,V14,V15,V14  //i110 < i25,i22,i109 > i111
    VPERM   V3,V12,V9,V3  //i11 < i149,i10,i151 > i12
    VAQ     V10,V3,V16  //i12 < i8,i11 > i13
    VL      0x70(R7),V5  // // i3050 1415161718191a1b1c1d1e1f
    VL      0xd0(R7),V3  // // i3049 12131415161718191a1b1c1d1e1f
    VL      0x0(R7),V15  // // i3048 00
    VPERM   V15,V16,V5,V5  //i13 < i149,i12,i152 > i14
    VPERM   V15,V5,V3,V17  //i14 < i149,i13,i153 > i15,i34,i79
    VPDI    $0x1,V15,V4,V3  //i17 < i149,i16 > i30,i99,i112
    VPDI    $0x4,V4,V4,V5  //i73 < i16,i16 > i79,i91,i102
    VPDI    $0x0,V15,V4,V4  //i85 < i149,i16 > i89,i98,i111
    VPDI    $0x1,V15,V17,V16  //i15 < i149,i14 > i30,i38,i39
    VPDI    $0x0,V15,V17,V15  //i34 < i149,i14 > i35,i36,i111
    VMSLG   V17,V5,V2,V2  //i79 < i14,i73,i78 > i80,i119
    VPERM   V15,V15,V9,V17  //i35 < i34,i34,i151 > i36
    VPERM   V16,V16,V9,V9  //i38 < i15,i15,i151 > i39
    VMSLG   V16,V3,V11,V11  //i30 < i15,i17,i29 > i60
    VSQ     V15,V17,V17  //i36 < i35,i34 > i37,i40
    VSQ     V16,V9,V9  //i39 < i38,i15 > i40,i44
    VMSLG   V15,V4,V14,V14  //i111 < i34,i85,i110 > i112
    VAQ     V13,V17,V13  //i37 < i33,i36 > i43
    VAQ     V17,V9,V15  //i40 < i36,i39 > i41
    VAQ     V10,V9,V9  //i44 < i8,i39 > i46
    VAQ     V12,V15,V16  //i41 < i10,i40 > i42,i45
    VL      0x40(R7),V10  // // i3046 10111213141516171819
    VL      0x0(R7),V12  // // i3045 00
    VL      0x80(R7),V15  // // i3044 1a1b1c1d1e1f000000000000
    VPERM   V12,V16,V10,V10  //i42 < i149,i41,i157 > i43
    VPERM   V12,V16,V15,V12  //i45 < i149,i41,i151 > i46
    VAQ     V9,V12,V9  //i46 < i44,i45 > i47
    VAQ     V13,V10,V13  //i43 < i37,i42 > i48
    VL      0x20(R7),V15  // // i3043 010111213
    VL      0x0(R7),V10  // // i3042 00
    VL      0x80(R7),V12  // // i3041 1a1b1c1d1e1f000000000000
    VPERM   V10,V9,V15,V9  //i47 < i149,i46,i158 > i48
    VAQ     V13,V9,V9  //i48 < i43,i47 > i54,i68
    VPERM   V10,V6,V12,V10  //i53 < i149,i52,i151 > i54
    VAQ     V9,V10,V16  //i54 < i48,i53 > i55
    VL      0x80(R7),V12  // // i3037 1a1b1c1d1e1f000000000000
    VL      0x70(R7),V15  // // i3040 1415161718191a1b1c1d1e1f
    VL      0xd0(R7),V13  // // i3039 12131415161718191a1b1c1d1e1f
    VL      0x0(R7),V10  // // i3038 00
    VPERM   V10,V16,V15,V15  //i55 < i149,i54,i152 > i56
    VPERM   V10,V15,V13,V15  //i56 < i149,i55,i153 > i57,i61,i102
    VPDI    $0x0,V10,V15,V13  //i57 < i149,i56 > i58,i59,i98
    VPDI    $0x1,V10,V15,V10  //i61 < i149,i56 > i62,i63,i112
    VMSLG   V15,V5,V8,V8  //i102 < i56,i73,i101 > i103,i122
    VPERM   V13,V13,V12,V15  //i58 < i57,i57,i151 > i59
    VPERM   V10,V10,V12,V12  //i62 < i61,i61,i151 > i63
    VMSLG   V13,V4,V7,V7  //i98 < i57,i85,i97 > i99
    VSQ     V13,V15,V13  //i59 < i58,i57 > i60,i64
    VSQ     V10,V12,V15  //i63 < i62,i61 > i64,i68
    VAQ     V13,V15,V12  //i64 < i59,i63 > i65
    VAQ     V9,V15,V9  //i68 < i48,i63 > i70
    VAQ     V6,V12,V16  //i65 < i52,i64 > i66,i69
    VL      0x40(R7),V6  // // i3036 10111213141516171819
    VL      0x0(R7),V12  // // i3035 00
    VL      0x80(R7),V15  // // i3034 1a1b1c1d1e1f000000000000
    VPERM   V12,V16,V6,V6  //i66 < i149,i65,i157 > i67
    VPERM   V12,V16,V15,V12  //i69 < i149,i65,i151 > i70
    VAQ     V11,V13,V11  //i60 < i30,i59 > i67
    VAQ     V9,V12,V9  //i70 < i68,i69 > i71
    VAQ     V11,V6,V12  //i67 < i60,i66 > i72
    VL      0x20(R7),V13  // // i3033 010111213
    VL      0x80(R7),V11  // // i3032 1a1b1c1d1e1f000000000000
    VL      0x0(R7),V6  // // i3031 00
    VPERM   V6,V9,V13,V9  //i71 < i149,i70,i158 > i72
    VAQ     V12,V9,V9  //i72 < i67,i71 > i81,i124
    VPERM   V6,V2,V11,V11  //i80 < i149,i79,i151 > i81
    VMSLG   V10,V3,V14,V10  //i112 < i61,i17,i111 > i115
    VAQ     V9,V11,V11  //i81 < i72,i80 > i82
    VPDI    $0x1,V6,V11,V13  //i82 < i149,i81 > i83
    VL      0x50(R7),V6  // // i3030 18191a1b1c1d1e1f
    VL      0x0(R7),V12  // // i3029 00
    VL      0x80(R7),V11  // // i3028 1a1b1c1d1e1f000000000000
    VPERM   V12,V13,V6,V13  //i83 < i149,i82,i159 > i84,i91,i94
    VPDI    $0x0,V12,V13,V6  //i84 < i149,i83 > i89,i113,i114
    VMSLG   V13,V5,V1,V1  //i91 < i83,i73,i90 > i92,i105
    VPDI    $0x1,V12,V13,V5  //i94 < i149,i83 > i99,i116,i117
    VMSLG   V6,V4,V0,V0  //i89 < i84,i85,i88 > i93
    VMSLG   V5,V3,V7,V3  //i99 < i94,i17,i98 > i104
    VPERM   V6,V6,V11,V4  //i113 < i84,i84,i151 > i114
    VPERM   V5,V5,V11,V11  //i116 < i94,i94,i151 > i117
    VL      0x40(R7),V7  // // i3027 10111213141516171819
    VL      0x0(R7),V12  // // i3026 00
    VL      0x80(R7),V13  // // i3025 1a1b1c1d1e1f000000000000
    VPERM   V12,V8,V7,V7  //i103 < i149,i102,i157 > i104
    VPERM   V12,V8,V13,V8  //i122 < i149,i102,i151 > i123
    VSQ     V6,V4,V4  //i114 < i113,i84 > i115,i118
    VSQ     V5,V11,V5  //i117 < i116,i94 > i118,i124
    VAQ     V10,V4,V6  //i115 < i112,i114 > i121
    VAQ     V4,V5,V4  //i118 < i114,i117 > i119
    VAQ     V9,V5,V5  //i124 < i72,i117 > i126
    VAQ     V2,V4,V10  //i119 < i79,i118 > i120,i125
    VL      0x40(R7),V4  // // i3024 10111213141516171819
    VL      0x0(R7),V2  // // i3023 00
    VL      0x80(R7),V9  // // i3022 1a1b1c1d1e1f000000000000
    VPERM   V2,V10,V4,V4  //i120 < i149,i119,i157 > i121
    VPERM   V2,V10,V9,V9  //i125 < i149,i119,i151 > i126
    VL      0x40(R7),V2  // // i3021 10111213141516171819
    VL      0x0(R7),V10  // // i3020 00
    VL      0x80(R7),V11  // // i3019 1a1b1c1d1e1f000000000000
    VPERM   V10,V1,V2,V2  //i92 < i149,i91,i157 > i93
    VPERM   V10,V1,V11,V1  //i105 < i149,i91,i151 > i106
    VAQ     V5,V9,V5  //i126 < i124,i125 > i127,i136
    VAQ     V6,V4,V6  //i121 < i115,i120 > i123
    VL      0x20(R7),V4  // // i3018 010111213
    VL      0x0(R7),V9  // // i3017 00
    VL      0x70(R7),V10  // // i3016 1415161718191a1b1c1d1e1f
    VPERM   V9,V5,V4,V4  //i127 < i149,i126,i158 > i128
    VPERM   V9,V5,V10,V5  //i136 < i149,i126,i152 > i137
    VAQ     V6,V8,V6  //i123 < i121,i122 > i128
    VAQ     V6,V4,V4  //i128 < i123,i127 > i129,i135
    VAQ     V0,V2,V0  //i93 < i89,i92 > i132
    VAQ     V3,V7,V3  //i104 < i99,i103 > i106
    VL      0x20(R7),V2  // // i3015 010111213
    VL      0x0(R7),V6  // // i3014 00
    VL      0x70(R7),V7  // // i3013 1415161718191a1b1c1d1e1f
    VPERM   V6,V4,V2,V2  //i129 < i149,i128,i158 > i130
    VPERM   V6,V4,V7,V4  //i135 < i149,i128,i152 > i137
    VAQ     V3,V1,V1  //i106 < i104,i105 > i130
    VAQ     V1,V2,V2  //i130 < i106,i129 > i131,i133
    VL      0x90(R7),V1  // // i3006 ffffffffffffffffffffffff
    VL      0xb0(R7),V7  // // i3012 405060708090a0bc0d0e0f14151617
    VL      0x0(R7),V3  // // i3011 00
    VL      0x20(R7),V6  // // i3010 010111213
    VPERM   V4,V5,V7,V4  //i137 < i135,i136,i161 > i138,i145,i146
    VPERM   V3,V2,V6,V5  //i131 < i149,i130,i158 > i132
    VL      0x0(R7),V6  // // i3009 00
    VL      0x70(R7),V7  // // i3008 1415161718191a1b1c1d1e1f
    VL      0xc0(R7),V3  // // i3007 c0d0e0f1415161718191a1b1c1d1e1f
    VPERM   V6,V2,V7,V2  //i133 < i149,i130,i152 > i134
    VAQ     V0,V5,V0  //i132 < i93,i131 > i134,i140
    VPERM   V0,V2,V3,V2  //i134 < i132,i133,i160 > i139,i141,i143
    VSCBIQ  V1,V4,V1  //i138 < i137,i162 > i139,i141
    VL      0x0(R7),V5  // // i3005 00
    VL      0x10(R7),V6  // // i3004 01b
    VL      0xe0(R7),V3  // // i3003 ffffffff000000010
    VPERM   V5,V0,V6,V0  //i140 < i149,i132,i163 > i142
    VSBIQ   V2,V3,V1,V3  //i139 < i134,i154,i138 > i143
    VL      0xe0(R7),V7  // // i3002 ffffffff000000010
    VL      0x0(R7),V6  // // i3001 00
    VL      0x90(R7),V5  // // i3000 ffffffffffffffffffffffff
    VSBCBIQ V2,V7,V1,V1  //i141 < i134,i154,i138 > i142
    VSBIQ   V0,V6,V1,V0  //i142 < i140,i149,i141 > i143,i146
    VSQ     V5,V4,V1  //i145 < i137,i162 > i146
    VSEL    V2,V3,V0,V2  //i143 < i139,i134,i142 > i144
    VSEL    V4,V1,V0,V0  //i146 < i145,i137,i142 > i147

    VLR     V2, T1
    VLR     V0, T0

    VLR     V26, X1
    VLR     V27, X0  
    VLR     V28, Y1  
    VLR     V29, Y0 

    VLM     x-224(SP), V16, V29
    RET
