let fib =
  fix
    \f. \n.
      if iszero n then 0
      else
        if iszero (n - 1) then 1
        else ((f (n - 1)) + (f (n - 2)))
in fib 10
