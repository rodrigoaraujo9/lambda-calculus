module Evaluator where

import Combinatory

evaluate :: Comb -> Comb
evaluate c =
  case executeT (c, []) of
    (result, []) -> result
    (result, xs) -> foldl (:@) result xs

executeT :: State -> State
executeT st =
  case execute st of
    Just st' -> executeT st'
    Nothing  -> st

-- ---->
execute :: State -> Maybe State
execute st =
  case unwind st of
    (Prim Add, a:b:xs) ->
      case (evaluate a, evaluate b) of
        (Const x, Const y) -> Just (Const (x + y), xs)
        _ -> error "*runtime error* invalid arguments for addition"

    (Prim Sub, a:b:xs) ->
      case (evaluate a, evaluate b) of
        (Const x, Const y) -> Just (Const (x - y), xs)
        _ -> error "*runtime error* invalid arguments for subtraction"

    (Prim Mul, a:b:xs) ->
      case (evaluate a, evaluate b) of
        (Const x, Const y) -> Just (Const (x * y), xs)
        _ -> error "*runtime error* invalid arguments for multiplication"

    (Prim Div, a:b:xs) ->
      case (evaluate a, evaluate b) of
        (_, Const 0)       -> error "*runtime error* division by zero"
        (Const x, Const y) -> Just (Const (x `div` y), xs)
        _ -> error "*runtime error* invalid arguments for division"

    (IfZero, c:t1:t2:xs) ->
      case evaluate c of
        Const 0 -> Just (t1, xs)
        Const _ -> Just (t2, xs)
        _       -> error "*runtime error* invalid condition in ifzero"

    st' ->
      rewrite st'

unwind :: State -> State
unwind (e1 :@ e2, xs) = unwind (e1, e2 : xs)
unwind st             = st

rewrite :: State -> Maybe State
rewrite (I, x:xs)     = Just (x, xs)
rewrite (K, p:_:xs)   = Just (p, xs)
rewrite (S, p:q:r:xs) = Just (((p :@ r) :@ (q :@ r)), xs)
rewrite (C, p:q:r:xs) = Just (((p :@ r) :@ q), xs)
rewrite (B, p:q:r:xs) = Just (p :@ (q :@ r), xs)
rewrite (Y, f:xs)     = Just (f :@ (Y :@ f), xs)
rewrite _             = Nothing
