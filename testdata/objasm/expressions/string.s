; Test some string expressions

    AREA C, CODE, READONLY

; left 3 chars of Hello
        = "Hello" :LEFT: 3

; left 0 chars of Hello
        = "Hello" :LEFT: 0

; left -1 chars of Hello
        = "Hello" :LEFT: -1

; right 3 chars of Hello
        = "Hello" :RIGHT: 3

; right 0 chars of Hello
        = "Hello" :RIGHT: 0

; right -1 chars of Hello
        = "Hello" :RIGHT: -1

; how long the string 'hello' is
        = :LEN: "Hello"

; how long the string '' is
        = :LEN: ""

; bracketted string
        = ("hello")

; join Hello and World
        = "Hello" :CC: "World"

; convert number 5 to a hex string
        = :STR: 5

; Convert Hello to upper case (not supported in ObjAsm)
        = :UPPERCASE: "Hello"

; Convert Hello to lower case (not supported in ObjAsm)
        = :LOWERCASE: "Hello"

; The character for code 65
        = :CHR: 65

        END

