// Reserving space

            AREA    |.text|, CODE, READONLY

size        *       25

// Labelled
label       SPACE   25
label2      %       25

// Not labelled
            SPACE   25
            %       25

// Refers to variables
            %       size * 2 + {CONFIG}

            END
