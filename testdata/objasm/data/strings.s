
        AREA        |Asm$Code|, CODE, READONLY

; Simple strings

code
; Hello
        = "Hello"
; Hello zero-terminated
        = "Hello", 0
; Multiple strings
        = "Hello ", "World"
; Numbers
        = 1, 2, 3
; Mixed
        = "Hello", 32, "World", 0
; Zero-terminator inline
        = "Hello", 0, "World", 0

; DCB variant of strings
        DCB "Hello"
; DCB variant with numbers
        DCB 1, 2, 3

; Keep the comment
        = "Hello"                   ; greeting
; Keep the comment with multiple values
        = "Hello", "There"          ; greeting
; Keep the comment with multiple values and terminator
        = "Hello", "There", 0       ; greeting
; Keep the comment with numbers
        = 1, 2, 3                   ; greeting

        END

