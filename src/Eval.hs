{-# LANGUAGE BangPatterns #-}

module Eval where
import Ast

eval :: Term -> Term
eval (Const n)    = Const n
eval (Var _)      = error "free variable occurrence"
eval (Lambda x e) = Lambda x e
eval (App e1 e2) =
  let !e1' = eval e1
      !e2' = eval e2
  in case e1' of
    Lambda x e -> eval (subst e2' x e)
    _ -> error "application exception"
eval (e1 :+ e2) =
  case (eval e1, eval e2) of
    (Const e1', Const e2') -> Const (e1'+e2')
    _ -> error "type error in :+"
eval (e1 :- e2) =
  case (eval e1, eval e2) of
    (Const e1', Const e2') -> Const (e1'-e2')
    _ -> error "type error in :-"
eval (e1 :* e2) =
  case (eval e1, eval e2) of
    (Const e1', Const e2') -> Const (e1'*e2')
    _ -> error "type error in :*"
eval (IfZero e1 e2 e3) =
  case eval e1 of
    Const 0 -> eval e2
    Const _ -> eval e3
    _       -> error "type error in ifzero"
eval (Let x e1 e2) = eval (App (Lambda x e2) e1)
eval (Fix t) =
  case eval t of
    Lambda x e -> eval (subst (Fix t) x e)
    _ -> error "fix exception"
