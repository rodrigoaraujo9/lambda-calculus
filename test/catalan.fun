let fact =
    fix (\g. \x.
        if iszero x then 1 else (x * (g (x - 1)))
    )
in let catalan =
    \x.
        (fact (2 * x)) / ((fact (x + 1)) * (fact x))
in catalan 10
