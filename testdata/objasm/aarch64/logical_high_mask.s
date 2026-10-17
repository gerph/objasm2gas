; Logical immediates with bit 63 set must keep their high bits
        AREA    |Mask$$Code|, CODE, READONLY
        EXPORT  high_bit
high_bit
        MOVN    X3, #0
        ANDS    X0, X3, #&8000000000000000
        ANDS    X0, X3, #&FFFF000000000000
        ANDS    X0, X3, #&FFFFFFFFFFFFFFF0
        ANDS    X0, X3, #&7FFFFFFFFFFFFFFF
        RET
        END
