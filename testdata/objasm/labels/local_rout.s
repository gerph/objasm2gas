
; Local labels used with ROUT

firstfunc       ROUT

10      MOV     r0, r5

20      B       %BT10

secondfunc      ROUT
10      B       %FT20
20      B       %BT10
