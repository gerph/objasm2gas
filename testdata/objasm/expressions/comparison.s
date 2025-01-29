; Some comparison statements

    AREA C, CODE, READONLY

; > expressions (0, 1, 0)
    =       1 > 2, 2 > 1, 1 > 1
; < expressions (1, 0, 0)
    =       1 < 2, 2 < 1, 1 < 1
; >= expressions (0, 1, 1)
    =       1 >= 2, 2 >= 1, 2 >= 2
; <= expressions (1, 0, 1)
    =       1 <= 2, 2 <= 1, 2 <= 2
; == expressions (1, 0)
    =       1 == 1, 1 == 2
; != expressions (0, 1)
    =       1 != 1, 1 != 2

; not 1>2
    [ 1 > 2
        = "1>2", 0
    |
        = "NOT 1>2", 0
    ]

; 1<2
    [ 1 < 2
        = "1<2", 0
    |
        = "NOT 1<2", 0
    ]


    END
