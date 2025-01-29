
; Check that labels with numbers in aren't confused

; hex label
    ADR     r0, mylabel_0x1234
mylabel_0x1234

; binary labels
    ADR     r0, mylabel_2_1101
mylabel_2_1101
