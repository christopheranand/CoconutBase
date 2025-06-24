VCOS   CELQPRLG BASEREG=NONE,DSASIZE=MYDSASZ,LEAF=YES
VCOS   ALIAS C'vcos'
    USING MYDSA,4         * establish addressibility to DSA
    VSTM  V16,V23,VRSAVE   * save caller's V16 - V23 (non_volatile)
    STD   8,FRSAVE        * save caller's FPR 8 - 15 (non_volatile)
    STD   9,FRSAVE+8      * only if you have clobbered F8 - F15 or
    STD   10,FRSAVE+16    * V8 - V15
    STD   11,FRSAVE+24
    STD   12,FRSAVE+32
    STD   13,FRSAVE+40
    STD   14,FRSAVE+48
    STD   15,FRSAVE+56
    lgfi  R0,64  * R0 hardcoded as counter
    llgf  R3,0(,R3) * load size into R3
    sllg  R3,R3,3(R0)  * 8 bytes per double
    larl r6,CONSTANTS
    larl r14,table
MODULOB_INIT DS 0H
    lg r13,16(R6,R0)
    cgijnh r3,0,RETURN
MODULOB_S0 DS 0H
    vl v29,16(r14,R0)
    vl v31,0(r14,R0)
    vl v28,0(r2,r13)
    vl v30,32(R6,R0)
    vn v27,v28,v29
    vn v31,v28,v31
    verim v27,v31,v30,3,0 * [(VR,"27"),(VR,"31"),(VR,"30"),(VR,"27")]
MODULOB_LOOPHEAD DS 0H
    slgfi r3,16
    cgrjnl r13,r3,MODULOB_S1
MODULOB_S0S1 DS 0H
    vl v29,16(r14,R0)
    vl v30,0(r14,R0)
    vst v27,0(r1,r13)
    vl v28,0(r2,r13)
    vl v31,32(R6,R0)
    vn v27,v28,v29
    vn v30,v28,v30
    verim v27,v30,v31,3,0 * [(VR,"27"),(VR,"30"),(VR,"31"),(VR,"27")]
MODULOB_LOOPEND DS 0H
    lg r15,0(R6,R0)
    agrk r13,r13,r15
    j MODULOB_LOOPHEAD
MODULOB_S1 DS 0H
    vst v27,0(r1,r13)
RETURN DS 0H
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
CONSTANTS   DS   0L
    DC    XL8'10'
    DC    XL8'0'
    DC    XL8'0'
    DC    XL8'0'
    DC    XL8'ffffffffffffffff'
    DC    XL8'0'
table   DS   0L
    DC    XL8'1020304050607'
    DC    XL8'8090a0b0c0d0e0f'
    DC    XL8'1011121314151617'
    DC    XL8'18191a1b1c1d1e1f'
MYDSA    DSECT
VRSAVE   DS    8L
FRSAVE   DS    8D              insert automatic data here as needed
FIFOS    DS    16L
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
