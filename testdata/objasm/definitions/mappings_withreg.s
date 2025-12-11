;
; Mappings take two forms - those that are with register and those without.
;

; With a register
                                    ^ 0, r7     ; Base and relative register
Constant_0                          # 1
Constant_1                          # 4
Constant_5                          # 1
Size_6                              * :INDEX: @

        AREA |Example$$code|, CODE, READONLY

        DCD     Constant_0
        DCD     Constant_1
        DCD     Constant_5

        LDR     r0, Constant_5
        LDRNE   r0, Constant_5
label   LDRB    r0, Constant_5

        ADR     r0, Constant_1
        LDR     r0, =Constant_1

        MOV     r0, #:INDEX: Constant_5

        END
