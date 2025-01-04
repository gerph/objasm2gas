; Test some of the builtins

        AREA    |C$$code|, CODE, READONLY

; AREA:
        = "{AREANAME}", 0

; Filename
        = "{INPUTFILE}", 0

; Line number (twice, to see it change)
        = "{LINENUM}", 0
        = "{LINENUM}", 0

                ^ 23
here            # 1

; Mapping should be 24
        = "{VAR}"

        END
