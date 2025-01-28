
        GET     printmacros.hdr

        AREA    |C$$code|, CODE, READONLY

OS_GenerateError * &2B


_entry  STP     x29, x30, [sp, #-16]!
        ADR     x0, message
        SWI     OS_Write0
        ADR     x1, end
        CMP     x0, x1
        BNE     bad_return
        SWI     OS_NewLine
        MOV     x0, #0      // no error return
        LDP     x29, x30, [sp], #16
        RET

bad_return
        ADR     x0, return_wrong
        SWI     OS_GenerateError

message
        = "Hello world", 0
end
        ALIGN

return_wrong
        DCD     1
        = "R0 on return from OS_Write0 was not correctly set to the terminator", 0

        END
