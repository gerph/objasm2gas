
        AREA        |Asm$Code|, CODE, READONLY
; Actually AArch64, but meh
; Based on https://www.iconbar.com/news/uploaded/2023/advent/rooladvent_3.png
; but actually using a system call to print a string.

code
        DCD     &a9bf7bfd       ; STP       x29, lr, [sp, #-16]!
        DCD     &910003fd       ; MOV       x29, sp
        DCD     &100000c0       ; ADR       x0, &18
        DCD     &d280004a       ; MOV       x10, #2
        DCD     &d4000001       ; SVC (x10 = OS_Write0)
        DCD     &52800000       ; MOV       w0, #0
        DCD     &a8c17bfd       ; LDP       x29, lr, [sp], 16
        DCD     &d65f03c0       ; RET

message
        = "Hello world!", 10
        = 0

        END

