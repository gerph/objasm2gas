
; Test that local labels in a macro stay within the macro

        MACRO
        HANG
10
        B   %BT10
30      ; never used
        MEND


myfunc  ROUT

; here's our routine label
10      MOV     r0, #23
        ADR     r1, %FT30

; here's our macro with labels in
        HANG

; This should be in the myfunc context and have an incremented sequence number
20

; This should jump back to the MOV instruction

        B       %BT10

; This should be the target for the ADR
30
