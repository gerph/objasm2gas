
; Extensions to the expression parser

        AREA    |.text|, CODE, READONLY

        MACRO
        SPLITREGS $rlist

        LCLS    first
        LCLS    rest
first   SETS    "$rlist" :REGLISTLEFT: 1
rest    SETS    "$rlist" :REGLISTSKIP: 1

        = "$first", 0
    [ "$rest" != ""
        SPLITREGS "$rest"
    ]
        MEND


        MACRO
        REVSPLITREGS $rlist

        LCLS    last
        LCLS    rest
last    SETS    "$rlist" :REGLISTRIGHT: 1
rest    SETS    "$rlist" :REGLISTTRIM: 1

        = "$last", 0
    [ "$rest" != ""
        REVSPLITREGS "$rest"
    ]
        MEND

; Split register list into r0
        SPLITREGS "r0"
; Split register list into r0, r1
        SPLITREGS "r0,r1"
; Split register list into r0, r1
        SPLITREGS "r0-r1"

; Split register list into r0, r1, r2, r3
        SPLITREGS "r0-r3"
; Split register list into r0, r1, r2, r3
        SPLITREGS "r0 - r3"

; Split register list into r0, r1, r2, r3, sp, lr
        SPLITREGS "r0-r3, sp, lr"

;;;;;;;; Now reverse

; Split register list into r0
        REVSPLITREGS "r0"
; Split register list into r0, r1
        REVSPLITREGS "r0,r1"
; Split register list into r0, r1
        REVSPLITREGS "r0-r1"

; Split register list into r0, r1, r2, r3
        REVSPLITREGS "r0-r3"
; Split register list into r0, r1, r2, r3
        REVSPLITREGS "r0 - r3"

; Split register list into r0, r1, r2, r3, sp, lr
        REVSPLITREGS "r0-r3, sp, lr"

        END
