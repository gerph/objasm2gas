; Ifdef conversion in conditionals

    [ :DEF: setting
        = "True", 0
    |
        = "False", 0
    ]

    [ :NOT: :DEF: setting
        = "True", 0
    |
        = "False", 0
    ]

    IF :DEF: setting
        = "True", 0
    ELSE
        = "False", 0
    ENDIF

    IF :NOT: :DEF: setting
        = "True", 0
    ELSE
        = "False", 0
    ENDIF

    END
