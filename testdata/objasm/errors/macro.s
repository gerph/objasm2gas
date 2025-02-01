
        AREA    |Test$$Code|, CODE

        MACRO
        FAULTY  $reg
        MUL     $reg, $reg, $reg, LSL $reg
        MEND

entry
        FAULTY  $reg

        END
