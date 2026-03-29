let lucas =
  fix \luc.
    \n.
      if iszero n then 2
      else
        if iszero (n - 1) then 1
        else ((luc (n - 1)) + (luc (n - 2)))
in lucas 10
