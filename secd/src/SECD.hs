module SECD where

import Data.Map (Map)

data Instr
    = HALT
    | LDC Int
    | LD Int
    | ADD
    | SUB
    | MUL
    | DIV
    | AP
    | RTN
    | SEL [Instr] [Instr]
    | JOIN
    | LDF [Instr]
    | LDRF [Instr]
    deriving (Show, Eq)

data Value
    = I Int
    | A Addr
    deriving(Show, Eq)

type Code    = [Instr]
type Closure = (Code, Env)
type Addr    = Int
type Store   = Map Addr Closure
type Stack   = [Value]
type Env     = [Value]
type Dump    = [(Stack, Env, Code)]
type Conf    = (Stack, Env, Code, Dump, Store)
