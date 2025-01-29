
; A set of plain labels

first
        MOV     pc,lr

second  DCD     5
third   = "String", 0
fourth                          // just label with comment
fifth   MOV     r0, lr          // instruction with comment

newrout ROUT
again   ROUT

        END
