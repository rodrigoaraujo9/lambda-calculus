module Evaluator where

import Combinatory

-- Evaluates a stack to a weak-head normal form represented as a spine
evaluate :: Stack -> Spine
evaluate s =
  case unwind s of

    -- Arithmetic primitives
    (Prim Add : a : b : xs) ->
      case (evaluate [a], evaluate [b]) of
        ([Const x], [Const y]) -> evaluate (Const (x + y) : xs)
        _ -> error "*runtime error* invalid arguments for addition"
    (Prim Sub : a : b : xs) ->
      case (evaluate [a], evaluate [b]) of
        ([Const x], [Const y]) -> evaluate (Const (x - y) : xs)
        _ -> error "*runtime error* invalid arguments for subtraction"
    (Prim Mul : a : b : xs) ->
      case (evaluate [a], evaluate [b]) of
        ([Const x], [Const y]) -> evaluate (Const (x * y) : xs)
        _ -> error "*runtime error* invalid arguments for multiplication"
    (Prim Div : a : b : xs) ->
      case (evaluate [a], evaluate [b]) of
        ([Const _], [Const 0]) -> error "division by zero"
        ([Const x], [Const y]) -> evaluate (Const (x `div` y) : xs)
        _ -> error "*runtime error* invalid arguments for division"

    -- Conditional
    (IfZero : c : t1 : t2 : xs) ->
      case evaluate [c] of
        [Const 0] -> evaluate (t1 : xs)
        [Const _] -> evaluate (t2 : xs)
        _ -> error "*runtime error* invalid condition in ifzero"

    -- Rewrite until WHNF
    s'
      | rewrite s' == s' -> s'
      | otherwise        -> evaluate (rewrite s')


-- Flatten application spine
unwind :: Stack -> Spine
unwind ((e1 :@ e2): xs) = unwind (e1:e2:xs)
unwind e = e


-- Reduction rules
rewrite :: Spine -> Spine
rewrite (I:x:xs)     = x:xs
rewrite (K:p:_:xs)   = p:xs
rewrite (S:p:q:r:xs) = ((p :@ r) :@ (q :@ r)) : xs
rewrite (C:p:q:r:xs) = ((p :@ r) :@ q) : xs
rewrite (B:p:q:r:xs) = (p :@ (q :@ r)) : xs
rewrite (Y:f:xs)     = (f :@ (Y :@ f)) :xs
rewrite xs = xs
