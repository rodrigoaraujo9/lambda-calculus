module Run where

import Execute(executeT)
import SECD(Code, Value)

import qualified Data.Map as Map

run :: Code -> Value
run c =
  case final of
    [v] -> v
    []  -> error "empty final stack in runtime"
    _   -> error "machine halted with more than one value inside the stack in runtime"
  where
    trace = executeT ([], [], c, [], Map.empty)
    (final, _, _, _, _) = last trace
