# stateful-lambda-interpreter

A stateful lambda calculus interpreter written in Haskell, built for a programming languages course.
Teacher provided AST and evaluation strategy.

## What it does

Parses and evaluates lambda calculus expressions, supporting:

- Variables and lambda abstractions (`\x. x` or `lambda x. x`)
- Function application (`f x`)
- Integer constants and arithmetic (`+`, `-`, `*`)
- Conditionals (`if iszero e then e else e`)
- Local definitions (`let x = e in e`)
- Fixed-point operator (`fix`)

## Evaluation strategy

- **Applicative order** — arguments are evaluated before being passed to a function
- **Weak normal form** — evaluation stops at lambda abstractions (the body is not reduced)
- **No free variables allowed** — any free variable occurrence raises an error at runtime

## How to run

You need GHC, Cabal and Happy installed.

```
happy src/Parser.y
cabal run
```

Then type an expression and press `Ctrl+D`.

## Example

```
if iszero 0 then 99 else 5
let twice = lambda f . lambda x . f (f x) in twice (lambda x . x + 1) 42
let fact = fix lambda f . lambda n . if iszero n then 1 else n * f (n - 1) in fact 10
```
