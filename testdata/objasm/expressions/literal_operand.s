
        AREA    |Test$$Code|, CODE

equ_value   * 5

            ^ &400
map_value   # 1

            GBLA    gbl_value
gbl_value   SETA    27


; Test equ_value
        MOV     r0, #equ_value
; Test map value
        MOV     r0, #map_value
; Test gbl_value
        MOV     r0, #gbl_value

; Test inside register-relative (equ)
        LDR     r0, [r0, #equ_value]

; Test inside register-relative (map)
        LDR     r0, [r0, #map_value]

; Test inside register-relative (gbl)
        LDR     r0, [r0, #gbl_value]

;----- Now calculations

; Test equ_value
        MOV     r0, #equ_value :SHL: 1
; Test map value
        MOV     r0, #map_value :SHL: 1
; Test gbl_value
        MOV     r0, #gbl_value :SHL: 1

; Test inside register-relative (equ)
        LDR     r0, [r0, #equ_value :SHL: 1]

; Test inside register-relative (map)
        LDR     r0, [r0, #map_value :SHL: 1]

; Test inside register-relative (gbl)
        LDR     r0, [r0, #gbl_value :SHL: 1]

        END
