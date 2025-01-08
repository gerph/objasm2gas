; Some comparison statements

    AREA C, CODE, READONLY

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
