let fact = fix lambda f. lambda n. if iszero n then 1 else n * f (n - 1) in fact 4
