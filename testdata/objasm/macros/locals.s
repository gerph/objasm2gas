
; Example of local variables
        AREA    |.text|, CODE, READONLY

        MACRO
        CALCULATE $a, $b
        LCLA    doubled
doubled SETA    $a * 2
    [ $b > 0
        CALCULATE $b + $a, $b - 1
    ]
        DCD     $b, $a + doubled
        MEND

; Should give '0, 18', '1, 15'
        CALCULATE 5, 1

; Should give '0, 24', '1, 21', '2, 15'
        CALCULATE 5, 2

        END
