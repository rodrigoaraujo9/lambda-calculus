let gauss =
  fix \g.
    \n.
      if iszero n then 0
      else (n + (g (n - 1)))
in gauss 100
