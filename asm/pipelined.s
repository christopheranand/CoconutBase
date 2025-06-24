* generated 2014-11-29 17:57:25.102131 (EST)
PIPELINED   CELQPRLG BASEREG=NONE,DSASIZE=MYDSASZ,LEAF=YES
PIPELINED   ALIAS C'pipelined'
    USING MYDSA,4         establish addressibility to DSA
    VSTM  V16,V23,VRSAVE    save caller's V16 - V23 (non_volatile)
    STD   8,FRSAVE        save caller's FPR 8 - 15 (non_volatile)
    STD   9,FRSAVE+8      only if you have clobbered F8 - F15 or
    STD   10,FRSAVE+16    V8 - V15
    STD   11,FRSAVE+24
    STD   12,FRSAVE+32
    STD   13,FRSAVE+40
    STD   14,FRSAVE+48
    STD   15,FRSAVE+56
    lgr   R5,R1 * move pOut so we can use r1 
    sgr   R15,R15 * zero R15
    lgfi  R0,64  * R0 hardcoded as counter
    llgf  R1,0(,R3) * load count into R3
    sllg  r1,R1,3(R0)  * 8 bytes per double

    larl   R6,CONSTANTS
    vl      v5,0(R6,R0) * TEST

    CIJL   r1,64,NLT8 * n < 8

S1 DS 0H

    vl      V1,0(R2,R15)
    vl      V2,16(R2,R15)
    vl      V3,32(R2,R15)
    vl      V4,48(R2,R15)

    agfi    r15,64
    agfi    r5,-64

    CIJNH   r1,127,S2N8

S1S2 DS 0H

    vfm     V6,V1,V5,3,0
    vfm     V7,V2,V5,3,0
    vfm     V8,V3,V5,3,0
    vfm     V9,V4,V5,3,0

    vl      V1,0(R2,R15)
    vl      V2,16(R2,R15)
    vl      V3,32(R2,R15)
    vl      V4,48(R2,R15)

    agfi    r15,64
    agfi    r5,-64

    CGFI    r1,196
    brc     4,S2S3 * blt

    agfi    R1,-64

S1S2S3 DS 0H

    vst     V6,0(R5,R15)
    vst     V7,16(R5,R15)
    vst     V8,32(R5,R15)
    vst     V9,48(R5,R15)

    vfm     V6,V1,V5,3,0
    vfm     V7,V2,V5,3,0
    vfm     V8,V3,V5,3,0
    vfm     V9,V4,V5,3,0

    vl      V1,0(R2,R15)
    vl      V2,16(R2,R15)
    vl      V3,32(R2,R15)
    vl      V4,48(R2,R15)

    brxle   15,R0,S1S2S3

S2S3 DS 0H

    vst     V6,0(R5,R15)
    vst     V7,16(R5,R15)
    vst     V8,32(R5,R15)
    vst     V9,48(R5,R15)

    vfm     V6,V1,V5,3,0
    vfm     V7,V2,V5,3,0
    vfm     V8,V3,V5,3,0
    vfm     V9,V4,V5,3,0

    agfi    r15,64

S3 DS 0H

    vst     V6,0(R5,R15)
    vst     V7,16(R5,R15)
    vst     V8,32(R5,R15)
    vst     V9,48(R5,R15)


    lgfi r7,56  *  = 0x7 * 8
    ngr  r1,r7   * and count to find remainder

* branch if not n div 8
    CIJNE  r1,0,TAILCASE

PRERETURN DS 0H

    VLM  V16,V23,VRSAVE
    LD   8,FRSAVE
    LD   9,FRSAVE+8
    LD   10,FRSAVE+16
    LD   11,FRSAVE+24
    LD   12,FRSAVE+32
    LD   13,FRSAVE+40
    LD   14,FRSAVE+48
    LD   15,FRSAVE+56
    CELQEPLG

S2N8 DS 0H

    vfm     V6,V1,V5,3,0
    vfm     V7,V2,V5,3,0
    vfm     V8,V3,V5,3,0
    vfm     V9,V4,V5,3,0

    agfi    r15,64
    agfi    r5,-64

    j       S3 * always

* only need scratch if < 8 elements

TAILCASE DS 0H

    agr  r5,r15
    agfi r5,64
    agr  r2,r15
    agfi r2,-64

NLT8 DS 0H

    CIJL   r1,1,PRERETURN   * r1 < 1

rREM     EQU R12

    lgr     r8,r1
    agfi    r8,-8

LOADSCRATCH DS 0H
    ld      0,0(R2,r8)
    std     0,FIFOOFF(r4,r8)
    agfi    r8,-8
    CGIJNL  r8,0,LOADSCRATCH

    vl      V1,FIFOOFF+0(,r4)
    vl      V2,FIFOOFF+16(,r4)
    vl      V3,FIFOOFF+32(,r4)
    vl      V4,FIFOOFF+48(,r4)

    vfm     V6,V1,V5,3,0
    vfm     V7,V2,V5,3,0
    vfm     V8,V3,V5,3,0
    vfm     V9,V4,V5,3,0

    vst     V6,FIFOOFF+0(,r4)
    vst     V7,FIFOOFF+16(,r4)
    vst     V8,FIFOOFF+32(,r4)
    vst     V9,FIFOOFF+48(,r4)

    lgr     r8,r1
    agfi    r8,-8
STORESCRATCH DS 0H
    ld      0,FIFOOFF(r4,r8)
    std     0,0(r5,r8)
    agfi    r8,-8
    CGIJNL  r8,0,STORESCRATCH

    j       PRERETURN * always


CONSTANTS   DS    0L
    DC    XL8'4000000000000000'
    DC    XL8'4000000000000000'
*  the following lines are not in ZHLASM/vrecip.2.s
MYDSA    DSECT
VRSAVE   DS    8L
FRSAVE   DS    8D              insert automatic data here as needed
FIFOOFF  EQU   *-MYDSA
FIFOS    DS    24L             * make this a min of 4 and needed
MYDSASZ  EQU   *-MYDSA

R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10       EQU   10
R11       EQU   11
R12       EQU   12
R13       EQU   13
R14       EQU   14
R15       EQU   15
V0       EQU   0
V1       EQU   1
V2       EQU   2
V3       EQU   3
V4       EQU   4
V5       EQU   5
V6       EQU   6
V7       EQU   7
V8       EQU   8
V9       EQU   9
V10       EQU   10
V11       EQU   11
V12       EQU   12
V13       EQU   13
V14       EQU   14
V15       EQU   15
V16       EQU   16
V17       EQU   17
V18       EQU   18
V19       EQU   19
V20       EQU   20
V21       EQU   21
V22       EQU   22
V23       EQU   23
V24       EQU   24
V25       EQU   25
V26       EQU   26
V27       EQU   27
V28       EQU   28
V29       EQU   29
V30       EQU   30
V31       EQU   31
    END
