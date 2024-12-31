;
; Mappings take two forms - those that are with register and those without.
;

; Without a register
                                    ^ 0     ; Base
Constant_0                          # 1
Constant_1                          # 4
Constant_5                          # 1

                                    ^ &49000
Large                               # 4
Larger                              # 4

        AREA |Example$$code|, CODE, READONLY

        DCD     Constant_0
        DCD     Constant_1
        DCD     Constant_5

        LDR     r1, =Constant_1
        LDR     r2, =Large
        LDR     r2, =Larger

        ADD     r1, r1, #Large

        END
