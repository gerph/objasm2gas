; Test some of the builtins

        AREA    |C$$code|, CODE, READONLY

; AREA: (not objasm, armasm only)
        = {AREANAME}, 0

; Filename (not objasm, armasm only)
        = {INPUTFILE}, 0

; Line number (twice, to see it change) (not objasm, armasm only)
        ALIGN
        DCD     {LINENUM}
        DCD     {LINENUM}

                ^ 23
here            # 1

; Mapping should be 24
        = {VAR}, 0

; Config should be 32
        = {CONFIG}, 0

; Architecture
        = {ARCHITECTURE}, 0

        END
