
; Local labels, going forward


; Refer forwars to it
        B       %FT30

30
        MOV     r0, #0


; now refer forwards to the next 30 label
        B       %FT30

; second definition of the label
30
        MOV     r0, #1

        END
