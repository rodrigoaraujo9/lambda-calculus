module Combinatory where

import Data.List (union)

type Ident = String
type State = (Combinator, Stack)
type Stack = [Combinator]
type Spine = [Combinator]

data Op = Add | Sub | Mul | Div
    deriving (Eq, Show)

data Combinator
    = Var Ident                 -- variables
    | Combinator :@ Combinator  -- application
    | S | K | I                 -- basic combinators

    -- extensions
    | B | C                     -- extra combinators
    | Y                         -- fixpoint combinator
    | Const Int                 -- numbers
    | Prim Op                   -- primitive operations
    | IfZero                    -- conditional
    deriving (Eq, Show)

infixl 9 :@

fv :: Combinator -> [Ident]
fv (Var x)  = [x]
fv (p :@ q) = fv p `union` fv q
fv _        = []
