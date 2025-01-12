
; Some tests of the local labels

; Simple local labels - alone, with code, with comment
10

20      MOV     r0, r5
30                                  // comment

; Backward branches in the common BT form - alone, with label
        B       %BT20
40      B       %BT20

; Replacement label and branch
20
        B       %BT20


; Forward references
        B       %FT30
        B       %FT30       // reuse of FT30
        B       %BT30       // back to the prior 30

; this is the 30 we went forward to
30
        B       %BT30       // back to the 30 we just created

        END
