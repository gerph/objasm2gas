
; simple code region
        AREA    |.text|, CODE, READONLY

        MOV     pc, lr

; C-linkable area (should just be .text)
        AREA    |C$$code|, CODE, READONLY

        MOV     pc, lr

; C-linkable data area
        AREA    |C$$data|, DATA

        DCD     42

; C-linkable readonly data
        AREA    |C$$data|, DATA, READONLY

        MOV     pc, lr
