
; Simple global assignments

        AREA    |C$$code|, CODE, READONLY

; declare a string variable
        GBLS    MESSAGE
MESSAGE SETS    "Hello"

        = MESSAGE, 0

        END
