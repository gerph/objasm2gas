
        AREA    |Test$$Code|, CODE

        GET     hdr.swis
        GET     hdr.macros
        GET     hdr.printmacros

read_a_key      ROUT
        LDRB    r0, [r1], #1
        TEQ     r0, #'e'
        BEQ     escape
        B       readc

readc SIGNATURE
        MOV     r0, #65
        SUBS    r0, r0, #1              ; set the carry flag
        SWI     OS_ReadC
        BCC     %FT10
        SWI     OS_WriteS
        =       "Carry set on exit (means an escape condition)", 0
        ALIGN
        SWI     OS_NewLine
10
        MOV     r1, r0

        PrintMessage "You pressed "

        MOV     r0, r1
        SWI     OS_WriteC
        SWI     OS_WriteI + ' '
        PrintInteger r1

        SWI     OS_NewLine
        MOV     pc, lr

message
        = "You pressed ", 0
        ALIGN

escape SIGNATURE
        XSWI    OS_Byte, &7D
        B       readc

        END
