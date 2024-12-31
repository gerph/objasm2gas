
; Register renaming is useful to be able to keep semantic names for regs

pw          RN  r12
pwp         RN  r12

        LDR     pw, [pwp]
