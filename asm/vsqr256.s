// generated 2019-08-28 00:41:51.834818222 (EDT)
PIPELINEDMULP   DS    0L
    VL      0x10(R6,R0),v13  //ns  // i3003 12131415161718191a1b1c1d1e1f
    VL      0x0(R6,R0),v12  //ns  // i3002 ffffffffffffffffffffffffffff
    VL      0x0(R2,R15),v10  //s2 imms[0]
    VGBM    0x0,v0  //s2 i2 <  > i4,i5,i6,i7,i8,i11,i12,i13,i17,i19,i20,i25,i26,i27,i28,i30,i32,i33,i35,i36,i38,i41,i47,i49,i50,i52,i53,i55,i56,i58,i61,i65,i66,i67,i69,i70,i71,i74,i75,i78,i81
    VAC     0x4,v9,v7,v14,v8  //s1 i57 < i50,i55,i56 > i60,i62
    VACCC   0x4,v9,v7,v14,v9  //s1 i59 < i50,i55,i56 > i60,i62
    VAC     0x4,v8,v11,v9,v7  //s1 i60 < i57,i58,i59 > i63
    VACCC   0x4,v8,v11,v9,v8  //s1 i62 < i57,i58,i59 > i63
    VL      0x10(R2,R15),v9  //s2 imms[16]
    VN      v10,v12,v11  //s2 i3 < i0,i86 > i4
    VPERM   v0,v11,v13,v11  //s2 i4 < i2,i3,i87 > i5,i6,i9,i10,i18,i51
    VSLDB   0x2,v9,v10,v16  //s2 i15 < i1,i0 > i16
    VSLDB   0x4,v0,v9,v13  //s2 i28 < i2,i1 > i29
    VPDI    $0x1,v0,v11,v14  //s2 i5 < i2,i4 > i6
    VPDI    $0x4,v11,v11,v15  //s2 i9 < i4,i4 > i11,i19,i25,i34,i48
    VPDI    $0x1,v11,v11,v10  //s2 i10 < i4,i4 > i11
    VN      v16,v12,v12  //s2 i16 < i15,i86 > i17
    VL      0x0(R6,R0),v16  //ns  // i3001 ffffffffffffffffffffffffffff
    VN      v13,v16,v13  //s2 i29 < i28,i86 > i30
    VMSLG   v11,v14,v0,v21  //s2 i6 < i4,i5,i2 > i7
    VMSLG   v15,v10,v0,v10  //s2 i11 < i9,i10,i2 > i12,i38
    VL      0x10(R6,R0),v14  //ns  // i3000 12131415161718191a1b1c1d1e1f
    VPERM   v0,v12,v14,v12  //s2 i17 < i2,i16,i87 > i18,i24,i31,i32,i33,i46,i51,i52,i53,i68
    VPERM   v0,v13,v14,v23  //s2 i30 < i2,i29,i87 > i31,i45,i68,i73
    VSLDB   0x7,v10,v0,v19  //s2 i12 < i11,i2 > i14,i21
    VPDI    $0x4,v12,v11,v18  //s2 i18 < i17,i4 > i19
    VPDI    $0x1,v12,v12,v22  //s2 i24 < i17,i17 > i25,i47
    VPDI    $0x1,v0,v12,v24  //s2 i32 < i2,i17 > i33
    VSLDB   0x7,v0,v10,v10  //s2 i38 < i2,i11 > i40,i42
    VPDI    $0x1,v23,v23,v13  //s2 i45 < i30,i30 > i48,i54,i65,i69,i74
    VPDI    $0x4,v12,v12,v20  //s2 i46 < i17,i17 > i47,i65
    VPDI    $0x1,v11,v12,v14  //s2 i51 < i4,i17 > i54
    VPDI    $0x1,v12,v23,v17  //s2 i68 < i17,i30 > i69
    VMSLG   v15,v22,v0,v11  //s2 i25 < i9,i24,i2 > i26,i58
    VPDI    $0x4,v23,v12,v16  //s2 i31 < i30,i17 > i34
    VMSLG   v12,v24,v0,v24  //s2 i33 < i17,i32,i2 > i34
    VMSLG   v20,v22,v0,v22  //s2 i47 < i46,i24,i2 > i48
    VMSLG   v20,v13,v0,v20  //s2 i65 < i46,i45,i2 > i66
    VMSLG   v17,v13,v0,v17  //s2 i69 < i68,i45,i2 > i70
    VMSLG   v15,v18,v0,v25  //s2 i19 < i9,i18,i2 > i20,i41
    VSLDB   0x5,v11,v0,v18  //s2 i26 < i25,i2 > i27,i36
    VMSLG   v15,v16,v24,v16  //s2 i34 < i9,i31,i33 > i35,i61
    VMSLG   v15,v13,v22,v15  //s2 i48 < i9,i45,i47 > i49,i78
    VSLDB   0x5,v0,v11,v11  //s2 i58 < i2,i25 > i60,i62
    VSLDB   0x1,v20,v0,v24  //s2 i66 < i65,i2 > i67,i71
    VSLDB   0x8,v17,v0,v20  //s2 i70 < i69,i2 > i72,i76
    VAC     0x4,v0,v18,v0,v17  //s2 i27 < i2,i26,i2 > i37,i39
    VACCC   0x4,v0,v18,v0,v18  //s2 i36 < i2,i26,i2 > i37,i39
    VAC     0x4,v0,v24,v0,v22  //s2 i67 < i2,i66,i2 > i72,i76
    VACCC   0x4,v0,v24,v0,v24  //s2 i71 < i2,i66,i2 > i72,i76
    VSLDB   0x0,v21,v0,v26  //s-1 i7 < i6,i2 > i8,i13
    VAC     0x4,v0,v26,v0,v21  //s-1 i8 < i2,i7,i2 > i14,i21
    VACCC   0x4,v0,v26,v0,v26  //s-1 i13 < i2,i7,i2 > i14,i21
    VPDI    $0x4,v23,v23,v28  //s-1 i73 < i30,i30 > i74
    VAC     0x4,v21,v19,v26,v27  //s-1 i14 < i8,i12,i13 > i22
    VSLDB   0xe,v25,v0,v23  //s-1 i20 < i19,i2 > i22
    VACCC   0x4,v21,v19,v26,v19  //s-1 i21 < i8,i12,i13 > i22
    VSLDB   0xe,v0,v25,v21  //s-1 i41 < i2,i19 > i43
    VMSLG   v28,v13,v0,v25  //s-1 i74 < i73,i45,i2 > i75
    VAC     0x4,v27,v23,v19,v19  //s-1 i22 < i14,i20,i21 > i23
    VSLDB   0xf,v25,v0,v25  //s-1 i75 < i74,i2 > i77,i79
    VSLDB   0x3,v0,v15,v23  //s-1 i78 < i2,i48 > i80,i82
    VAC     0x4,v5,v21,v6,v5  //s-1 i43 < i40,i41,i42 > i44
    VST     0xffffffffffffffe0(R5,R15),v19  //s-1 imms[32]
    VAC     0x4,v2,v25,v1,v6  //s-1 i77 < i72,i75,i76 > i80,i82
    VACCC   0x4,v2,v25,v1,v1  //s-1 i79 < i72,i75,i76 > i80,i82
    VST     0xfffffffffffffff0(R5,R15),v5  //s-1 imms[48]
    VAC     0x4,v7,v4,v8,v5  //s-1 i63 < i60,i61,i62 > i64
    VAC     0x4,v6,v23,v1,v4  //s-1 i80 < i77,i78,i79 > i83
    VACCC   0x4,v6,v23,v1,v6  //s-1 i82 < i77,i78,i79 > i83
    VAC     0x4,v22,v20,v24,v2  //s0 i72 < i67,i70,i71 > i77,i79
    VACCC   0x4,v22,v20,v24,v1  //s0 i76 < i67,i70,i71 > i77,i79
    VST     0x0(R5,R15),v5  //s-1 imms[64]
    VAC     0x4,v4,v3,v6,v3  //s-1 i83 < i80,i81,i82 > i84
    VST     0x10(R5,R15),v3  //s-1 imms[80]
    VPDI    $0x0,v12,v0,v3  //s2 i52 < i17,i2 > i53
    VMSLG   v12,v3,v0,v3  //s2 i53 < i17,i52,i2 > i54
    VMSLG   v14,v13,v3,v7  //s2 i54 < i51,i45,i53 > i55,i81
    VSLDB   0xc,v16,v0,v5  //s2 i35 < i34,i2 > i37,i39
    VSLDB   0xc,v0,v16,v4  //s2 i61 < i2,i34 > i63
    VSLDB   0xa,v0,v7,v3  //s2 i81 < i2,i54 > i83
    VAC     0x4,v17,v5,v18,v6  //s2 i37 < i27,i35,i36 > i40,i42
    VACCC   0x4,v17,v5,v18,v8  //s2 i39 < i27,i35,i36 > i40,i42
    VAC     0x4,v6,v10,v8,v5  //s2 i40 < i37,i38,i39 > i43
    VACCC   0x4,v6,v10,v8,v6  //s2 i42 < i37,i38,i39 > i43
    VSLDB   0x3,v15,v0,v8  //s2 i49 < i48,i2 > i50,i56
    VAC     0x4,v0,v8,v0,v9  //s2 i50 < i2,i49,i2 > i57,i59
    VSLDB   0xa,v7,v0,v7  //s2 i55 < i54,i2 > i57,i59
    VACCC   0x4,v0,v8,v0,v14  //s2 i56 < i2,i49,i2 > i57,i59
    BCR condition, LINKREGISTER // return, either set condition which is always true, or find out from Bill a condition code which is always true
CONSTANTS   DS    0L
    DC    XL8'ffffffffffff'
    DC    XL8'ffffffffffffffff'
    DC    XL8'12131415161718'
    DC    XL8'191a1b1c1d1e1f'
