; Check that the common RISC OS 'SIGNATURE' macro works

        AREA    |.text|, CODE, READONLY

; -------------------------------------------------
        MACRO
$label  SIGNATURE
        ALIGN   4
        =       "$label",0
        ALIGN   4
        DCD     &FF000000+(:LEN:"$label"+4):AND::NOT:3
$label
        MEND

foo     SIGNATURE
        MOV     pc, lr

        END
