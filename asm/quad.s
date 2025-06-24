    larl r6,CONSTANTS
    lg r5,0(R6,R0)
MODULOB_INIT DS 0H
    xgrk r14,r3,r3
    cgijnh r3,0,RETURN
MODULOB_PROLOGUE DS 0H
    vl v31,0(r2,r14)
MODULOB_LOOPHEAD DS 0H
    sgrk r15,r3,r5
    cgrjnl r14,r15,MODULOB_EPILOGUE
MODULOB_KERNEL DS 0H
    vfm v31,v31,v31,3,0
    vfm v31,v31,v31,3,0
    vfm v31,v31,v31,3,0
    vfm v31,v31,v31,3,0
    vfm v31,v31,v31,3,0
    vfm v31,v31,v31,3,0
    vst v31,0(r1,r14)
    vl v31,16(r2,r14)
MODULOB_LOOPEND DS 0H
    agrk r14,r14,r5
    j MODULOB_LOOPHEAD
MODULOB_EPILOGUE DS 0H
    vfm v31,v31,v31,3,0
    vfm v31,v31,v31,3,0
    vfm v31,v31,v31,3,0
    vfm v31,v31,v31,3,0
    vfm v31,v31,v31,3,0
    vfm v31,v31,v31,3,0
    vst v31,0(r1,r14)
