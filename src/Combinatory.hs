module Combinatory where

import Data.List (union)

type Ident = String
type Stack = [Comb]
type Spine = [Comb]

-- Primitive arithmetic operations
data Op = Add | Sub | Mul | Div
    deriving (Eq, Show)

-- Target language
data Comb
    = Var Ident                 -- variables
    | Comb :@ Comb              -- application
    | S | K | I                 -- basic combinators

    -- extensions
    | B | C                     -- extra combinators
    | Y                         -- fixpoint combinator
    | Const Int                 -- numbers
    | Prim Op                   -- primitive operations
    | IfZero                    -- conditional
    deriving (Eq, Show)

infixl 9 :@

-- Free variables of a combinator expression
fv :: Comb -> [Ident]
fv (Var x)  = [x]
fv (p :@ q) = fv p `union` fv q
fv _        = []
