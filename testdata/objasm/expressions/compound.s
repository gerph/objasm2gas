; Some compound expressions

    AREA C, CODE, READONLY

; Extract 'Hel'
    = "Hello" :LEFT: (:LEN: "Hello" - 2), 0

; join Hello and World, then perform operation
    = ( "Hello" :CC: "World" ) :RIGHT: 2

; Simple calculation 1013
    DCD 23 * 44 + 1

; Brackets 1035
    DCD 23 * (44 + 1)

    END
