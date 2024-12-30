;
; Mappings take two forms - those that are with register and those without.
;

; Without a register
                                    ^ 0     ; Base
Constant_0                          # 1
Constant_1                          # 4
Constant_5                          # 1

        AREA |Example$$code|, CODE, READONLY

        DCD     Constant_0
        DCD     Constant_1
        DCD     Constant_5

; FIXME: Add a register base example



        END
