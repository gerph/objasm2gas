
        AREA    |Test$$Code|, CODE

        GET     hdr.swis
        GET     hdr.macros
        GET     hdr.printmacros

        EXPORT  _entry

_entry
read_a_key      ROUT
        LDRB    w0, [x1], #1
        CMP     w0, #'e'
        BEQ     escape
        B       readc

readc SIGNATURE
        MOV     x0, #65
        SUBS    x0, x0, #1              ; set the carry flag
        SWI     OS_ReadC
        CMP     x1, #1
        BNE     %FT10
        SWI     OS_WriteS
        =       "x1=1 on exit (means an escape condition)", 0
        ALIGN
        SWI     OS_NewLine
        B       %FT20
10
        CMP     x1, #0
        BEQ     %FT20
        PrintMessage "x1="
        PrintInteger x1
        PrintLine " on exit (INVALID value)"

20
        MOV     x1, x0

        PrintMessage "You pressed "

        MOV     x0, x1
        SWI     OS_WriteC
        SWI     &100 + 32; OS_WriteI + ' '
        PrintInteger x1

        SWI     OS_NewLine
        RET

message
        = "You pressed ", 0
        ALIGN

escape SIGNATURE
        XSWI    OS_Byte, &7D
        B       readc

        END
