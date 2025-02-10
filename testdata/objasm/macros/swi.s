
; Example of a raw SWI call macro
        AREA    |.text|, CODE, READONLY

        MACRO
$label  SWI     $swi
$label  SVC     #$swi
        MEND

OS_WriteC           * 0
OS_Write0           * 2

        ADR     r0, %FT10
        SWI     OS_Write0
        MOV     r0, #65
        SWI     OS_WriteC
        SWI     &100+65

10
        = "Example", 0

        END
