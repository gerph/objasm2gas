
        AREA        |Asm$Code|, CODE, READONLY

; Word data

data

; Single
        DCFS    1, 2, -5
; Single fractions
        DCFS    0.5, -0.5, -2.5

; Double
        DCFD    1, 2, -5
; Double fractions
        DCFD    0.5, -2.5

        END

