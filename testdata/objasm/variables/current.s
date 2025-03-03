
; Current address in ObjAsm

        AREA    |C$$code|, CODE, READONLY

; Write the value into a word
        DCD     .

; Write the value into a word (b)
        DCD     {PC}

; Assign to a constant
here    *       .

; Branch to the address
        B       .

; Relative positions as a table
table
        DCD     target - .
target


; Unlablled branch
        B       . + 8

; Extra expression (1)
        DCD     . + (8 * 2)

; Extra expression (1b)
        DCD     {PC} + (8 * 2)

; Extra expression (2)
        DCD     8 + (target - .)

; Extra expression (2b)
        DCD     8 + (target - {PC})

        END
