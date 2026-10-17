; NOINIT areas are reserved space, not stored in the object
        AREA    |Matrix$$Zero|, DATA, NOINIT, ALIGN=4
        EXPORT  matrix_zero
matrix_zero
        SPACE   32
        END
