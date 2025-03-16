; Check that the my PrintMessage macro works

        AREA    |.text|, CODE, READONLY

OS_WriteS * 1
OS_WriteI * 0x100

        MACRO
$label  SWI     $swi
        MOV     x10, #$swi
$label  SVC     #0
        MEND

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


; -------------------------------------------------------------------------
        MACRO
        PrintMessage    $message, $width
        SWI     OS_WriteS
        = "$message", 0
        ALIGN
    [ "$width" <> ""
    [ $width > :LEN: "$message"
        Push    "x1"
        MOV     x1, #$width - :LEN: "$message"
10
        SWI     OS_WriteI + ' '
        SUBS    x1, x1, #1
        BNE     %BT10
        Pull    "x1"
    ]
    ]
        MEND

; These macros with widths in should be expanded
entry
        PrintMessage "M#", 4
        PrintMessage "CV?", 4       ; check valid
        PrintMessage "RV?", 4       ; read valid
        RET

        END
