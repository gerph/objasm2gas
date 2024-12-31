;
; Constants that have expressions in them
;

XBit * 1<<17

SWIBase             * &46000
FirstSWI            * SWIBase + 0
SecondSWI           * SWIBase + 1

XFirstSWI           * FirstSWI :OR: XBit

        AREA |Example$$code|, CODE, READONLY

        DCD     FirstSWI
        DCD     SecondSWI

        END
