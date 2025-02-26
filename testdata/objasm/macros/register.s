
        AREA    |.text|, CODE, READONLY

        MACRO
        TEST    $a
        ADD     $a, $a, #1
        MEND

; ADD r2,r2,#1
        TEST    r2


        MACRO
        TEST2   $a, $b
        ADD     $a, $a, #$b
        MEND

; ADD r2,r2,#2
        TEST2   r2, 4


        MACRO
        TEST3   $a, $b
        ADD     $a, $a, $b
        MEND

; ADD r2,r2,#16
        TEST3   r2, #8*2


        MACRO
        TEST4   $b, $a
        ADD     $a, $a, $b
        MEND

; ADD r2,r2,#16
        TEST4   #8*2, r2

        END
