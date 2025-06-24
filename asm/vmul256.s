// generated 2019-08-28 00:40:12.270769059 (EDT)
PIPELINEDMULP   DS    0L
    VL      0x10(R6,R0),v5  //ns  // i3002 12131415161718191a1b1c1d1e1f
    VL      0x0(R6,R0),v1  //ns  // i3001 ffffffffffffffffffffffffffff
    VL      0x0(R2,R15),v16  //s2 imms[0]
    VGBM    0x0,v0  //s2 i4 <  > i6,i8,i9,i10,i11,i12,i15,i16,i17,i21,i25,i26,i28,i34,i36,i37,i38,i40,i42,i44,i45,i48,i49,i51,i54,i60,i63,i64,i67,i68,i71,i72,i74,i77,i81,i83,i84,i85,i86,i88,i89,i91,i92,i95,i98
    VL      0x10(R2,R15),v6  //s2 imms[16]
    VL      0x20(R2,R15),v3  //s2 imms[32]
    VSLDB   0x1,v14,v0,v14  //s1 i83 < i82,i4 > i84,i89
    VSLDB   0x8,v15,v0,v15  //s1 i88 < i87,i4 > i90,i93
    VAC     0x4,v0,v14,v0,v19  //s1 i84 < i4,i83,i4 > i90,i93
    VACCC   0x4,v0,v14,v0,v14  //s1 i89 < i4,i83,i4 > i90,i93
    VMSLG   v4,v9,v10,v4  //s1 i47 < i13,i41,i46 > i48,i77
    VAC     0x4,v19,v15,v14,v9  //s1 i90 < i84,i88,i89 > i94,i96
    VSLDB   0xf,v12,v0,v10  //s1 i92 < i91,i4 > i94,i96
    VACCC   0x4,v19,v15,v14,v19  //s1 i93 < i84,i88,i89 > i94,i96
    VSLDB   0xc,v4,v0,v15  //s1 i48 < i47,i4 > i50,i52
    VSLDB   0x3,v11,v0,v14  //s1 i63 < i62,i4 > i64,i72
    VSLDB   0xa,v13,v0,v12  //s1 i71 < i70,i4 > i73,i75
    VSLDB   0xa,v0,v13,v13  //s1 i98 < i4,i70 > i100
    VAC     0x4,v9,v10,v19,v20  //s1 i94 < i90,i92,i93 > i97,i99
    VSLDB   0x3,v0,v11,v11  //s1 i95 < i4,i62 > i97,i99
    VACCC   0x4,v9,v10,v19,v10  //s1 i96 < i90,i92,i93 > i97,i99
    VAC     0x4,v17,v15,v18,v9  //s1 i50 < i37,i48,i49 > i53,i55
    VACCC   0x4,v17,v15,v18,v17  //s1 i52 < i37,i48,i49 > i53,i55
    VAC     0x4,v0,v14,v0,v15  //s1 i64 < i4,i63,i4 > i73,i75
    VACCC   0x4,v0,v14,v0,v14  //s1 i72 < i4,i63,i4 > i73,i75
    VSLDB   0xc,v0,v4,v4  //s1 i77 < i4,i47 > i79
    VAC     0x4,v20,v11,v10,v18  //s1 i97 < i94,i95,i96 > i100
    VACCC   0x4,v20,v11,v10,v11  //s1 i99 < i94,i95,i96 > i100
    VAC     0x4,v9,v2,v17,v10  //s1 i53 < i50,i51,i52 > i56
    VACCC   0x4,v9,v2,v17,v9  //s1 i55 < i50,i51,i52 > i56
    VAC     0x4,v15,v12,v14,v2  //s1 i73 < i64,i71,i72 > i76,i78
    VACCC   0x4,v15,v12,v14,v12  //s1 i75 < i64,i71,i72 > i76,i78
    VAC     0x4,v18,v13,v11,v11  //s1 i100 < i97,i98,i99 > i101
    VAC     0x4,v10,v7,v9,v9  //s1 i56 < i53,i54,i55 > i57
    VAC     0x4,v2,v8,v12,v7  //s1 i76 < i73,i74,i75 > i79
    VACCC   0x4,v2,v8,v12,v2  //s1 i78 < i73,i74,i75 > i79
    VST     0x90(R5,R15),v9  //s1 imms[80]
    VAC     0x4,v7,v4,v2,v2  //s1 i79 < i76,i77,i78 > i80
    VST     0xa0(R5,R15),v2  //s1 imms[96]
    VST     0xb0(R5,R15),v11  //s1 imms[112]
    VN      v16,v1,v4  //s2 i5 < i0,i103 > i6
    VSLDB   0x2,v6,v16,v2  //s2 i23 < i1,i0 > i24
    VSLDB   0x4,v0,v6,v6  //s2 i42 < i4,i1 > i43
    VPERM   v0,v4,v5,v13  //s2 i6 < i4,i5,i104 > i10,i13,i65
    VN      v2,v1,v4  //s2 i24 < i23,i103 > i25
    VN      v6,v1,v2  //s2 i43 < i42,i103 > i44
    VPERM   v0,v4,v5,v17  //s2 i25 < i4,i24,i104 > i26,i33,i65,i66
    VPERM   v0,v2,v5,v12  //s2 i44 < i4,i43,i104 > i45,i59,i66,i68,i86
    VPDI    $0x1,v17,v12,v15  //s2 i66 < i25,i44 > i69,i87
    VL      0x30(R2,R15),v6  //s2 imms[48]
    VN      v3,v1,v2  //s2 i7 < i2,i103 > i8
    VPERM   v0,v2,v5,v9  //s2 i8 < i4,i7,i104 > i9,i14,i22,i67
    VPDI    $0x1,v0,v9,v10  //s2 i9 < i4,i8 > i10,i26,i45
    VPDI    $0x4,v13,v13,v4  //s2 i13 < i6,i6 > i15,i27,i35,i47,i62
    VPDI    $0x1,v9,v9,v8  //s2 i14 < i8,i8 > i15,i34,i60
    VPDI    $0x4,v12,v12,v19  //s2 i59 < i44,i44 > i60,i81,i91
    VPDI    $0x0,v9,v0,v16  //s2 i67 < i8,i4 > i68
    VMSLG   v13,v10,v0,v5  //s2 i10 < i6,i9,i4 > i11
    VMSLG   v4,v8,v0,v2  //s2 i15 < i13,i14,i4 > i16,i51
    VMSLG   v17,v10,v0,v7  //s2 i26 < i25,i9,i4 > i27
    VPDI    $0x4,v17,v17,v14  //s2 i33 < i25,i25 > i34,i46,i61,i82
    VMSLG   v12,v10,v0,v10  //s2 i45 < i44,i9,i4 > i46
    VMSLG   v19,v8,v0,v11  //s2 i60 < i59,i14,i4 > i61
    VPDI    $0x1,v13,v17,v13  //s2 i65 < i6,i25 > i70
    VMSLG   v12,v16,v0,v18  //s2 i68 < i44,i67,i4 > i69
    VSLDB   0x0,v5,v0,v5  //s2 i11 < i10,i4 > i12,i17
    VSLDB   0x7,v2,v0,v16  //s2 i16 < i15,i4 > i18,i29
    VSLDB   0x7,v0,v2,v2  //s2 i51 < i4,i15 > i53,i55
    VAC     0x4,v0,v5,v0,v17  //s2 i12 < i4,i11,i4 > i18,i29
    VACCC   0x4,v0,v5,v0,v20  //s2 i17 < i4,i11,i4 > i18,i29
    VAC     0x4,v17,v16,v20,v5  //s2 i18 < i12,i16,i17 > i30
    VACCC   0x4,v17,v16,v20,v16  //s2 i29 < i12,i16,i17 > i30
    VSLDB   0x2,v6,v3,v3  //s2 i19 < i3,i2 > i20
    VSLDB   0x4,v0,v6,v17  //s2 i38 < i4,i3 > i39
    VN      v3,v1,v3  //s2 i20 < i19,i103 > i21
    VN      v17,v1,v17  //s2 i39 < i38,i103 > i40
    VL      0x10(R6,R0),v1  //ns  // i3000 12131415161718191a1b1c1d1e1f
    VPERM   v0,v3,v1,v3  //s2 i21 < i4,i20,i104 > i22,i32,i41,i85
    VPERM   v0,v17,v1,v17  //s2 i40 < i4,i39,i104 > i41,i58
    VPDI    $0x4,v3,v9,v20  //s2 i22 < i21,i8 > i27,i46
    VPDI    $0x1,v3,v3,v1  //s2 i32 < i21,i21 > i35,i61,i69,i81
    VMSLG   v14,v8,v0,v8  //s2 i34 < i33,i14,i4 > i35
    VPDI    $0x4,v17,v3,v9  //s2 i41 < i40,i21 > i47
    VPDI    $0x1,v17,v17,v17  //s2 i58 < i40,i40 > i62,i70,i82,i87,i91
    VPDI    $0x0,v3,v0,v3  //s2 i85 < i21,i4 > i86
    VMSLG   v4,v20,v7,v7  //s2 i27 < i13,i22,i26 > i28,i54
    VMSLG   v4,v1,v8,v8  //s2 i35 < i13,i32,i34 > i36,i74
    VMSLG   v14,v20,v10,v10  //s2 i46 < i33,i22,i45 > i47
    VMSLG   v14,v1,v11,v11  //s2 i61 < i33,i32,i60 > i62
    VMSLG   v15,v1,v18,v18  //s2 i69 < i66,i32,i68 > i70
    VMSLG   v19,v1,v0,v1  //s2 i81 < i59,i32,i4 > i82
    VMSLG   v12,v3,v0,v3  //s2 i86 < i44,i85,i4 > i87
    VMSLG   v19,v17,v0,v12  //s2 i91 < i59,i58,i4 > i92
    VMSLG   v14,v17,v1,v14  //s2 i82 < i33,i58,i81 > i83
    VMSLG   v15,v17,v3,v15  //s2 i87 < i66,i58,i86 > i88
    VSLDB   0xe,v7,v0,v3  //s2 i28 < i27,i4 > i30
    VSLDB   0x5,v8,v0,v1  //s2 i36 < i35,i4 > i37,i49
    VSLDB   0xe,v0,v7,v7  //s2 i54 < i4,i27 > i56
    VMSLG   v4,v17,v11,v11  //s2 i62 < i13,i58,i61 > i63,i95
    VMSLG   v13,v17,v18,v13  //s2 i70 < i65,i58,i69 > i71,i98
    VSLDB   0x5,v0,v8,v8  //s2 i74 < i4,i35 > i76,i78
    VAC     0x4,v5,v3,v16,v3  //s2 i30 < i18,i28,i29 > i31
    VST     0xc0(R5,R15),v3  //s2 imms[64]
    VAC     0x4,v0,v1,v0,v17  //s2 i37 < i4,i36,i4 > i50,i52
    VACCC   0x4,v0,v1,v0,v18  //s2 i49 < i4,i36,i4 > i50,i52
    BCR condition, LINKREGISTER // return, either set condition which is always true, or find out from Bill a condition code which is always true
CONSTANTS   DS    0L
    DC    XL8'ffffffffffff'
    DC    XL8'ffffffffffffffff'
    DC    XL8'12131415161718'
    DC    XL8'191a1b1c1d1e1f'
