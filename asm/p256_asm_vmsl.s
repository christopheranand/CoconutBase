DATA p256vmsl<>+0x00(SB)/8, $0xffffffff00000001 // 0xffffffff00000001
DATA p256vmsl<>+0x08(SB)/8, $0x0000000000000000 // 0x0000000000000000
DATA p256vmsl<>+0x10(SB)/8, $0x0000000000000000
DATA p256vmsl<>+0x18(SB)/8, $0x0000000000000000 // 0x0
DATA p256vmsl<>+0x20(SB)/8, $0x0000000000000000
DATA p256vmsl<>+0x28(SB)/8, $0x000000000000001b // 0x01b
DATA p256vmsl<>+0x30(SB)/8, $0x0000000000000000 // 0x0000000000000000
DATA p256vmsl<>+0x38(SB)/8, $0x0000000010111213 // 0x0000000010111213
DATA p256vmsl<>+0x40(SB)/8, $0x0000000000001011 // 0x0000000000001011
DATA p256vmsl<>+0x48(SB)/8, $0x0000121314151617 // 0x0000121314151617 
DATA p256vmsl<>+0x50(SB)/8, $0x0000000000001011 // 0x0000000000001011
DATA p256vmsl<>+0x58(SB)/8, $0x1213141516171819 // 0x1213141516171819
DATA p256vmsl<>+0x60(SB)/8, $0x0000000000001819 // 0x0000000000001819
DATA p256vmsl<>+0x68(SB)/8, $0x00001a1b1c1d1e1f // 0x00001a1b1c1d1e1f
DATA p256vmsl<>+0x70(SB)/8, $0x0000000008090a0b // 0x0000000008090a0b
DATA p256vmsl<>+0x78(SB)/8, $0x0c0d0e0f10111213 // 0x0c0d0e0f10111213
DATA p256vmsl<>+0x80(SB)/8, $0x0000000014151617 // 0x0000000014151617  
DATA p256vmsl<>+0x88(SB)/8, $0x18191a1b1c1d1e1f // 0x18191a1b1c1d1e1f 
DATA p256vmsl<>+0x90(SB)/8, $0x000000001a1b1c1d // 0x000000001a1b1c1d
DATA p256vmsl<>+0x98(SB)/8, $0x1e1f000000000000 // 0x1e1f000000000000
DATA p256vmsl<>+0xa0(SB)/8, $0x0000141516171819 // 0x0000141516171819
DATA p256vmsl<>+0xa8(SB)/8, $0x00001a1b1c1d1e1f // 0x00001a1b1c1d1e1f
DATA p256vmsl<>+0xb0(SB)/8, $0x0405060708090a0b // 0x0405060708090a0b
DATA p256vmsl<>+0xb8(SB)/8, $0x0c0d0e0f14151617 // 0x0c0d0e0f14151617  
DATA p256vmsl<>+0xc0(SB)/8, $0x0c0d0e0f14151617 // 0x0c0d0e0f14151617
DATA p256vmsl<>+0xc8(SB)/8, $0x18191a1b1c1d1e1f // 0x18191a1b1c1d1e1f
DATA p256vmsl<>+0xd0(SB)/8, $0x1213141516171819 // 0x1213141516171819
DATA p256vmsl<>+0xd8(SB)/8, $0x00001a1b1c1d1e1f // 0x00001a1b1c1d1e1f
DATA p256vmsl<>+0xe0(SB)/8, $0x00000000ffffffff // 0x00000000ffffffff
DATA p256vmsl<>+0xe8(SB)/8, $0xffffffffffffffff // 0xffffffffffffffff 

GLOBL p256vmsl<>(SB), 8, $240

TEXT p256MulInternal<>(SB), NOSPLIT, $0-0
    MOVD    $p256vmsl<>+0x00(SB), CPOOL
    VLR     X1, V26
    VLR     X0, V27 
    VLR     Y1, V28 
    VLR     Y0, V29
    
    VLR     V26, V2
    VLR     V27, V3 
    VLR     V28, V9

    VL      0x10(CPOOL), V4
    VL      0x70(CPOOL), V1 
    VL      0xa0(CPOOL), V0 
    VPERM   V4, V3, V0, V11  
    VPERM   V2, V3, V1, V1  
    VL      0x0(CPOOL), V4 
    VL      0x40(CPOOL), V5
    VL      0x10(CPOOL), V3
    VPERM   V3, V2, V5, V2  
    VPERM   V3, V4, V5, V4   
    VL      0x0(CPOOL), V6  
    VL      0xe0(CPOOL), V7 
    VL      0x70(CPOOL), V8 
    VPERM   V3, V7, V0, V5   
    VPERM   V6, V7, V8, V6   
    VPERM   V3, V1, V0, V1   
    VPERM   V3, V6, V0, V7  
    VLR     V29, V3
    VL      0x90(CPOOL), V6 
    VL      0x10(CPOOL), V8 
    VL      0xa0(CPOOL), V10 
    VL      0x70(CPOOL), V0
    VPERM   V8, V3, V10, V15
    VPERM   V9, V3, V0, V3
    VL      0x40(CPOOL), V10
    VL      0xa0(CPOOL), V0
    VL      0x10(CPOOL), V8
    VPERM   V8, V9, V10, V9
    VPERM   V8, V3, V0, V17
    VPDI    $0x1, V8, V11, V19
    VPDI    $0x1, V8, V9, V0
    VPDI    $0x1, V8, V15, V12
    VPDI    $0x1, V8, V17, V14
    VPDI    $0x1, V1, V2, V18
    VPDI    $0x4, V2, V2, V16
    VPDI    $0x0, V8, V2, V10
    VPDI    $0x4, V15, V15, V13
    VMSLG   V0, V19, V8, V3
    VPDI    $0x0, V8, V9, V0 
    VPDI    $0x0, V8, V17, V2 
    VPDI    $0x0, V8, V15, V23
    VMSLG   V12, V19, V8, V12 
    VMSLG   V14, V19, V8, V19 
    VPDI    $0x1, V11, V1, V14 
    VPDI    $0x4, V17, V17, V21 
    VMSLG   V13, V1, V8, V22 
    VMSLG   V15, V18, V3, V13 
    VPDI    $0x4, V1, V1, V3 
    VMSLG   V15, V16, V8, V20 
    VMSLG   V0, V10, V8, V0 
    VMSLG   V9, V16, V8, V1 
    VMSLG   V2, V10, V8, V2 
    VMSLG   V17, V16, V8, V16 
    VMSLG   V23, V10, V8, V23 
    VPDI    $0x4, V11, V11, V10 
    VMSLG   V9, V18, V2, V2 
    VMSLG   V17, V18, V23, V18 
    VMSLG   V21, V11, V22, V11
    VMSLG   V17, V14, V13, V13 
    VMSLG   V17, V3, V20, V17 
    VMSLG   V9, V3, V16, V3 
    VMSLG   V15, V10, V8, V16 
    VMSLG   V15, V14, V19, V15 
    VMSLG   V9, V14, V18, V14 
    VMSLG   V9, V10, V17, V10 
    VPERM   V8, V16, V6, V6 
    VAQ     V12, V6, V9 
    VL      0x80(CPOOL), V8 // i3048 1415161718191a1b1c1d1e1f
    VL      0xd0(CPOOL), V6 // i3047 12131415161718191a1b1c1d1e1f
    VL      0x10(CPOOL), V18 // i3046 00
    VPERM   V18, V9, V8, V8 
    VPERM   V18, V8, V6, V17  
    VPDI    $0x1, V5, V7, V6  
    VPDI    $0x1, V7, V4, V8  
    VPDI    $0x4, V4, V4, V9  
    VPDI    $0x0, V18, V4, V4 
    VPDI    $0x1, V5, V5, V7 
    VPDI    $0x1, V18, V5, V5 
    VMSLG   V17, V6, V15, V15 
    VPDI    $0x1, V18, V17, V18 
    VMSLG   V17, V7, V16, V16 
    VMSLG   V17, V8, V13, V13 
    VMSLG   V17, V9, V10, V10 
    VL      0x10(CPOOL), V19 // i3045 00
    VPDI    $0x0, V19, V17, V17 
    VMSLG   V18, V5, V12, V12 
    VMSLG   V17, V4, V14, V14 
    VL      0x50(CPOOL), V17 // i3044 10111213141516171819
    VL      0x10(CPOOL), V18 // i3043 00
    VL      0x90(CPOOL), V19 // i3042 1a1b1c1d1e1f000000000000
    VPERM   V18, V16, V17, V17 
    VPERM   V18, V16, V19, V16 
    VAQ     V12, V16, V12 
    VAQ     V15, V17, V17 
    VL      0x30(CPOOL), V18 // i3041 010111213
    VL      0x10(CPOOL), V15 // i3040 00
    VL      0x90(CPOOL), V16 // i3039 1a1b1c1d1e1f000000000000
    VPERM   V15, V12, V18, V12 
    VAQ     V17, V12, V12 
    VPERM   V15, V11, V16, V15 
    VAQ     V12, V15, V19 
    VL      0x50(CPOOL), V17 // i3035 10111213141516171819
    VL      0x80(CPOOL), V18 // i3038 1415161718191a1b1c1d1e1f
    VL      0xd0(CPOOL), V15 // i3037 12131415161718191a1b1c1d1e1f
    VL      0x10(CPOOL), V16 // i3036 00
    VPERM   V16, V19, V18, V18 
    VPERM   V16, V18, V15, V15 
    VMSLG   V15, V7, V11, V11 
    VPDI    $0x1, V16, V15, V18 
    VPERM   V16, V11, V17, V16 
    VMSLG   V18, V5, V12, V18 
    VL      0x90(CPOOL), V19 // i3034 1a1b1c1d1e1f000000000000
    VL      0x10(CPOOL), V12 // i3033 00
    VL      0x30(CPOOL), V17 // i3032 010111213
    VPERM   V12, V11, V19, V11 
    VMSLG   V15, V6, V13, V13 
    VAQ     V18, V11, V11 
    VAQ     V13, V16, V13 
    VPERM   V12, V11, V17, V11 
    VAQ     V13, V11, V11 
    VL      0x90(CPOOL), V16 // i3031 1a1b1c1d1e1f000000000000
    VL      0x10(CPOOL), V12 // i3030 00
    VL      0x60(CPOOL), V13 // i3029 18191a1b1c1d1e1f
    VPERM   V12, V10, V16, V17 
    VPDI    $0x0, V12, V15, V16 
    VMSLG   V15, V9, V3, V3 
    VMSLG   V15, V8, V14, V14 
    VAQ     V11, V17, V15 
    VPDI    $0x1, V12, V15, V15 
    VMSLG   V16, V4, V2, V2 
    VPERM   V12, V15, V13, V13 
    VL      0x10(CPOOL), V12 // i3028 00
    VPDI    $0x0, V12, V13, V12 
    VMSLG   V13, V9, V1, V1 
    VMSLG   V13, V8, V2, V2 
    VMSLG   V13, V6, V14, V6 
    VMSLG   V13, V7, V10, V7 
    VL      0x10(CPOOL), V8 // i3027 00
    VPDI    $0x1, V8, V13, V8 
    VMSLG   V12, V4, V0, V0 
    VMSLG   V8, V5, V11, V4 
    VL      0x50(CPOOL), V5 // i3026 10111213141516171819
    VL      0x10(CPOOL), V8 // i3025 00
    VL      0x90(CPOOL), V9 // i3024 1a1b1c1d1e1f000000000000
    VPERM   V8, V7, V5, V5 
    VPERM   V8, V7, V9, V8 
    VL      0x50(CPOOL), V7 // i3023 10111213141516171819
    VL      0x10(CPOOL), V9 // i3022 00
    VL      0x90(CPOOL), V10 // i3021 1a1b1c1d1e1f000000000000
    VPERM   V9, V3, V7, V7 
    VPERM   V9, V3, V10, V9 
    VL      0x50(CPOOL), V3 // i3020 10111213141516171819
    VL      0x10(CPOOL), V10 // i3019 00
    VL      0x90(CPOOL), V11 // i3018 1a1b1c1d1e1f000000000000
    VPERM   V10, V1, V3, V3 
    VPERM   V10, V1, V11, V1 
    VAQ     V6, V5, V5 
    VAQ     V4, V8, V4 
    VAQ     V5, V9, V5 
    VL      0x30(CPOOL), V6 // i3017 010111213
    VL      0x10(CPOOL), V8 // i3016 00
    VL      0x80(CPOOL), V9 // i3015 1415161718191a1b1c1d1e1f
    VPERM   V8, V4, V6, V6 
    VPERM   V8, V4, V9, V4 
    VAQ     V2, V7, V2 
    VAQ     V5, V6, V5 
    VAQ     V2, V1, V1 
    VL      0x30(CPOOL), V2 // i3014 010111213
    VL      0x10(CPOOL), V6 // i3013 00
    VL      0x80(CPOOL), V7 // i3012 1415161718191a1b1c1d1e1f
    VPERM   V6, V5, V2, V2 
    VPERM   V6, V5, V7, V5 
    VAQ     V0, V3, V0 
    VAQ     V1, V2, V6 
    VL      0x30(CPOOL), V1 // i3011 010111213
    VL      0x10(CPOOL), V2 // i3010 00
    VL      0x80(CPOOL), V3 // i3009 1415161718191a1b1c1d1e1f
    VPERM   V2, V6, V1, V1 
    VPERM   V2, V6, V3, V6 
    VL      0xb0(CPOOL), V7 // i3008 405060708090a0bc0d0e0f14151617
    VL      0xc0(CPOOL), V3 // i3007 c0d0e0f1415161718191a1b1c1d1e1f
    VL      0xe0(CPOOL), V2 // i3006 ffffffffffffffff
    VPERM   V5, V4, V7, V4 
    VAQ     V0, V1, V0 
    VPERM   V0, V6, V3, V1 
    VSQ     V2, V4, V9  // c0 = vsq a0 b0 
    VSCBIQ  V4, V2, V2  // brw0 = vscbiq a0 b0
    VL      0x10(CPOOL), V5 // i3005 00
    VL      0x20(CPOOL), V6 // i3004 01b
    VL      (CPOOL), V3 // i3003 0
    VPERM   V5, V0, V6, V0 
    VSBCBIQ V1, V3, V2, V6  // brw1 = vsbcbiq a1 b1 brw0
    VL      0xe0(CPOOL), V8 // i3002 ffffffffffffffff
    VL      0x0(CPOOL), V7  // i3001 0
    VL      0x10(CPOOL), V3 // i3000 00
    VSBIQ   V1, V7, V2, V5  // c1 = vsbiq a1 b1 brw0
    VNC     V0, V6, V0 
    VSQ     V0, V3, V0 
    VSEL    V1, V5, V0, T1
    VSEL    V4, V9, V0, T0 
    RET 
// a1 -> V1
// a0 -> V4