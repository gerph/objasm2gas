        ALIGN        ; Ensures alignment.

dadd    FUNCTION     ; Without the ALIGN directive this might not be word-aligned.
        EXPORT  dadd
;        PUSH       {r4-r6,lr}    ; This line automatically word-aligned.
;        FRAME PUSH {r4-r6,lr}
        ; subroutine body
;        POP        {r4-r6,pc}
        ENDFUNC
func6   PROC {r4-r8,r12},{D1-D3} ; Non-AAPCS-conforming function.
        MOV     pc, lr
        ENDP

func7   FUNCTION {}  ; Another non-AAPCS-conforming function.
        MOV     pc, lr
        ENDFUNC

        END
