let pell =
  fix \p.
    \n.
      if iszero n then 0
      else
        if iszero (n - 1) then 1
        else (((2 * (p (n - 1))) + (p (n - 2))))
in pell 6
