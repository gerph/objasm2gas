;
; Constants are just simple assignments
;

Constant_0                          * 0
Constant_1                          * 1
Constant_5                          * 5

        AREA |Example$$code|, CODE, READONLY

        DCD     Constant_0
        DCD     Constant_1
        DCD     Constant_5

        END
