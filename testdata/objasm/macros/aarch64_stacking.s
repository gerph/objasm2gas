
; Macro to push registers, like the ARM STMFD/LDMFD.
        AREA    |.text|, CODE, READONLY

; -------------------------------------------------
        MACRO
$label  Push    $reglist

        LCLS    rest
        LCLS    last
last    SETS    "$reglist" :REGLISTRIGHT: 2
rest    SETS    "$reglist" :REGLISTTRIM: 2
    [ ("$last" :REGLISTTRIM: 1) = ""
$label  STR     $last, [sp, #-16]!
    |
$label  STP     $last, [sp, #-16]!
    ]
    [ "$rest" != ""
        Push    "$rest"
    ]
        MEND

; -------------------------------------------------
        MACRO
$label  Pull    $reglist

        LCLS    rest
        LCLS    last
last    SETS    "$reglist" :REGLISTRIGHT: 2
rest    SETS    "$reglist" :REGLISTTRIM: 2

    [ "$rest" != ""
        Pull    "$rest"
    |
$label
    ]
    [ ("$last" :REGLISTTRIM: 1) = ""
        LDR     $last, [sp], #16
    |
        LDP     $last, [sp], #16
    ]
        MEND

; -------------------------------------------------
        GBLS    entry_reglist
        MACRO
$label  Entry   $reglist
entry_reglist SETS "$reglist"
$label  Push    "x29, lr"
        MOV     x29, sp
    [ "$reglist" != ""
        Push    "$reglist"
    ]
        MEND

; -------------------------------------------------
        MACRO
$label  EXIT
    [ "$entry_reglist" = ""
$label  Pull    "x29, lr"
    |
$label  Pull    "$entry_reglist, x29, lr"
    ]
        RET
        MEND


; Push registers x0
        Push    "x0"
; Push registers x0, x1
        Push    "x0, x1"
; Push registers x0, x1, x2
        Push    "x0, x1, x2"
; Push registers x0, x1, x2, x3
        Push    "x0, x1, x2, x3"

;------
; Pull registers x0
        Pull    "x0"
; Pull registers x0, x1
        Pull    "x0, x1"
; Pull registers x0, x1, x2
        Pull    "x0, x1, x2"
; Pull registers x0, x1, x2, x3
        Pull    "x0, x1, x2, x3"


;------
; Entry/Exit (none)
        Entry
        EXIT

; Entry/Exit x0
        Entry   "x0"
        EXIT

; Entry/Exit x0, x1
        Entry   "x0, x1"
        EXIT

; Entry/Exit x0, x1, x2
        Entry   "x0-x2"
        EXIT

        END
