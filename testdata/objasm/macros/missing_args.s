
        AREA    |.text|, CODE, READONLY

        MACRO
        TEST    $a, $b, $c
    [ "$a" != ""
        = "A: ", $a
    ]
    [ "$b" != ""
        = "B: ", $b
    ]
    [ "$c" != ""
        = "C: ", $c
    ]
        MEND

; ---
        TEST
; --- (commas)
        TEST    ,,

; X--
        TEST    15

; -X-
        TEST    ,30

; XX-
        TEST    15,30

; --X
        TEST    ,,45

; X-X
        TEST    15,,45

; -XX
        TEST    ,30,45

; XXX
        TEST    15,30,45

        END
