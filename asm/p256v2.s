#define CPOOL   R4

// Parameters
#define X0    V0 // Not modified
#define X1    V1 // Not modified
#define Y0    V2 // Not modified
#define Y1    V3 // Not modified
#define T0    V4
#define T1    V5
#define P0    V30 // Not modified
#define P1    V31 // Not modified

// input: d0
// output: h0, h1
// temp: TEMP, ZERO, BORROW
#define OBSERVATION3(d0, h0, h1, TEMP, ZERO, BORROW) \
	VZERO ZERO                   \
	VSLDB $4, d0, ZERO, h0       \
	VLR   h0, BORROW             \
	VSLDB $12, ZERO, h0, TEMP    \
	VSQ   TEMP, h0, h0           \
	VSLDB $12, d0, BORROW, h1    \
	VSLDB $8, ZERO, BORROW, TEMP \
	VAQ   TEMP, h0, h0           \

#define OBSERVATION3A(d2, h0, h1, TEMP, ZERO) \
	VZERO ZERO                \
	VSLDB $8, d2, ZERO, TEMP  \
	VSLDB $8, d2, TEMP, h0    \
	VSLDB $12, ZERO, TEMP, h1 \
	VSQ   h1, h0, h0          \

TEXT p256MulInternal<>(SB), NOSPLIT, $336-0
	VSTM V16, V31, x-256(SP)
	VST  V15, x-272(SP)
	VSTM V0, V3, x-336(SP)
	VLR  V1, V28
	VLR  V0, V29
	VLR  V3, V30
	VLR  V2, V31

	MOVD $p256mul<>+0x00(SB), CPOOL
	// Divide input1 into 5 limbs
	VGBM  $0x007f, V27
	VN    V27, V29, V5      // V5: first 7 bytes limb
	VSLDB $9, V28, V29, V4
	VN    V27, V4, V4       // V4: second 7 bytes limb
	VSLDB $2, V28, V29, V3
	VN    V27, V3, V3       // V3: third 7 bytes limb
	VSLDB $11, V29, V28, V2
	VN    V27, V2, V2       // V2: fourth 7 bytes limb
	VSLDB $4, V29, V28, V1
	VGBM  $0x000f, V27
	VN    V27, V1, V1       // V1: 4 bytes limb

	// Divide input2 into 5 limbs
	VGBM  $0x007f, V27
	VN    V27, V31, V10     // V10: first 7 bytes limb
	VSLDB $9, V30, V31, V9
	VN    V27, V9, V9       // V9: second 7 bytes limb
	VSLDB $2, V30, V31, V8
	VN    V27, V8, V8       // V8: third 7 bytes limb
	VSLDB $11, V31, V30, V7
	VN    V27, V7, V7       // V7: fourth 7 bytes limb
	VSLDB $4, V31, V30, V6
	VGBM  $0x000f, V27
	VN    V27, V6, V6       // V6: 4 bytes limb

	VZERO V31

	// start 2 prepare
	VSLDB $8, V10, V10, V12
	VO    V9, V12, V12      // v12: v10 | v9 -- b1 | b0

	VSLDB $8, V4, V4, V13
	VO    V5, V13, V13    // v13: v4 | v5 -- a1 | a0

	// start 3 prepare
	VSLDB $8, V3, V3, V15
	VO    V4, V15, V15    // v15: v3 | v4 -- a2 | a1 

	// start 4 prepare
	VSLDB $8, V2, V2, V17
	VO    V3, V17, V17    // v17: v2 | v3 -- a3 | a2 

	VSLDB $8, V8, V8, V19
	VO    V7, V19, V19    // v19: v8 | v7 -- b3 | b2 

	// start 5 prepare
	VSLDB $8, V1, V1, V20
	VO    V2, V20, V20    // v20: v1 | v2  -- 

	// start 6 prepare
	VSLDB $8, V9, V9, V4
	VO    V8, V4, V4    // v4: v9 | V8

	VSLDB $8, V7, V7, V7
	VO    V6, V7, V7    // v7: v7 | V6

	VMSLG V4, V20, V31, V22 // v22: v9 x v1 + v8 x v2
	VMSLG V7, V15, V31, V26 // v26: v7 x v3 + v6 x v4

	// start 7, prepare + VMSLG
	VMSLG V6, V3, V31, V23 // v23: v6 x v3
	VMSLG V19, V20, V31, V2 // v10: v8 x v1 + v7 x v2

	// start 8, prepare + VMSLG + adding
	VMSLG V7, V20, V31, V24 // v24: v7 x v1 + v6 x v2 (****)

	// start 9, prepare + VMSLG + adding
	VMSLG V6, V1, V31, V25 // v25: v6 x v1 (****)

	// start 1 prepare + VMSLG + adding
	VMSLG V10, V5, V31, V11 // v11: v10 x v5 (****)

	// start 2 VMSLG + adding
	VMSLG V12, V13, V31, V14 // v14: v10 x v4 + v9 x v5 (****)

	// start 3 VMSLG
	VMSLG V8, V5, V31, V16 // v16: v8 x v5
	VMSLG V12, V15, V31, V30 // v30: (10x3 + 9x4)

	// start 4 VMSLG
	VMSLG V12, V17, V31, V18 // v18: v10 x v2 + v9 x v3
	VMSLG V19, V13, V31, V29 // v29: v8 x v4 + v7 x v5

	// start 5 VMSLG
	VMSLG V6, V5, V31, V21 // v21: v6 x v5
	VMSLG V12, V20, V31, V28 // v28: v10 x v1 + v9 x v2
	VMSLG V19, V15, V31, V27 // v27: v8 x v3 + v7 x v4


	// move the addings here to avoid dependencies between VAQ VMSLG
	// start 3 adding
	VAQ V30, V16, V16 // v16: (10x3 + 9x4) + 8x5 (****)

	// start 4 adding
	VAQ V29, V18, V18 // v18: v10 x v2 + v9 x v3 + v8 x v4 + v7 x v5 (****)

	// start 5 adding
	VAQ V21, V28, V21
	VAQ V21, V27, V21 // v21: v10 x v1 + v9 x v2 + v8 x v3 + v7 x v4 + v6 x v5 (****)

	// start 6 adding
	VAQ V26, V22, V22 // v22: v7 x v3 + v6 x v4 + v9 x v1 + v8 x v2 (****)

	// start 7 adding
	VAQ V2, V23, V23 // v23: v8 x v1 + v7 x v2 + v6 x v3 (****)


	// generate the multiplication result
	// limb 1: right most 7 bytes of V11

	VZERO V12

	// limb 2: right most 7 bytes of V14
	VSLDB $9, V12, V11, V13
	VAQ   V13, V14, V14

	// limb 3: right most 7 bytes of V16
	VSLDB $9, V12, V14, V13
	VAQ   V13, V16, V16

	// limb 4: right most 7 bytes of V18
	VSLDB $9, V12, V16, V13
	VAQ   V13, V18, V18

	// limb 5: right most 7 bytes of V21
	VSLDB $9, V12, V18, V13
	VAQ   V13, V21, V21

	// limb 6: right most 7 bytes of V22
	VSLDB $9, V12, V21, V13
	VAQ   V13, V22, V22

	// limb 7: right most 7 bytes of V23
	VSLDB $9, V12, V22, V13
	VAQ   V13, V23, V23

	// limb 8: right most 7 bytes of V24
	VSLDB $9, V12, V23, V13
	VAQ   V13, V24, V24

	// limb 9: right most 7 bytes of V25. limb 10: left most 9 bytes of V25
	VSLDB $9, V12, V24, V13
	VAQ   V13, V25, V25

	VGBM  $0x007f, V1
	VGBM  $0x00ff, V2
	VZERO V3

	VN V11, V1, V11
	VN V14, V1, V14
	VN V16, V1, V16
	VN V18, V1, V18
	VN V21, V1, V21
	VN V22, V1, V22
	VN V23, V1, V23
	VN V24, V1, V24
	VN V25, V2, V25

	VSLDB $7, V14, V14, V14
	VO    V14, V11, V11
	VSLDB $14, V16, V3, V14
	VO    V14, V11, V11     // first rightmost 128bits of the multiplication result

	VSLDB $14, V3, V16, V16
	VSLDB $5, V18, V18, V18
	VO    V16, V18, V18
	VSLDB $12, V21, V3, V16
	VO    V16, V18, V18     // second rightmost 128bits of the multiplication result

	VSLDB $12, V3, V21, V21
	VSLDB $3, V22, V22, V22
	VO    V21, V22, V22
	VSLDB $10, V23, V3, V21
	VO    V21, V22, V22     // third rightmost 128bits of the multiplication result

	VSLDB $10, V3, V23, V23
	VSLDB $1, V24, V24, V24
	VO    V23, V24, V24
	VSLDB $8, V25, V25, V25
	VO    V25, V24, V25     // fourth rightmost 128bits of the multiplication result

	// First reduction, 96 bits
	VZERO V31
	VSLDB $4, V18, V11, V1
	VSLDB $4, V31, V18, V2
	OBSERVATION3(V11, V4, V3, V29, V28, V27)// Bill, results V4 | V3
	VACCQ V1, V3, V30
	VAQ   V1, V3, V5
	VACQ  V2, V4, V30, V6

	// Second reduction 96 bits
	VSLDB $4, V6, V5, V7
	VSLDB $4, V31, V6, V8
	OBSERVATION3(V5, V10, V9, V29, V28, V27)// Bill, results V10 | V9
	VACCQ V7, V9, V30
	VAQ   V7, V9, V12
	VACQ  V8, V10, V30, V13

	// Third reduction 64 bits
	VSLDB  $8, V13, V12, V14
	VSLDB  $8, V31, V13, V15
	OBSERVATION3A(V12, V17, V16, V29, V28)
	VACCQ  V14, V16, V30      // Bill, results V17 | V16
	VAQ    V14, V16, V19
	VACQ   V15, V17, V30, V20
	VACCQ  V19, V22, V30
	VAQ    V19, V22, V22
	VACCCQ V20, V25, V30, V26
	VACQ   V20, V25, V30, V25 // results V26 | V25 | V22

	// ---------------------------------------------------

	VL      16(CPOOL), P0
	VL      0(CPOOL), P1
	VZERO   V29
	VSCBIQ  P0, V22, V28       // CAR1
	VSQ     P0, V22, V27       // ADD1H
	VSBCBIQ V25, P1, V28, V24  // CAR2
	VSBIQ   V25, P1, V28, V23  // ADD2H
	VSBIQ   V26, V29, V24, V26

	// what output to use, ADD2H||ADD1H or T1||T0?
	VSEL V22, V27, V26, V22
	VSEL V25, V23, V26, V25
	VLR  V22, T0
	VLR  V25, T1

	VLM x-256(SP), V16, V31
	VL  x-272(SP), V15
	VLM x-336(SP), V0, V3
	RET