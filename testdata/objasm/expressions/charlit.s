; Test some character literal expressions

    AREA C, CODE, READONLY

OS_WriteI * &100

; 'A'
        =       'A'

; OS_WriteI + 'A'
        MOV     r10, #OS_WriteI+'A'

        END

