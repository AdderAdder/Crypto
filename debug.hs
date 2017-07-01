-- Module for debugging the program.
module Debug (debug,printBinaryRepresentation) where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Debug.Trace (trace)

-- Constant variable indicating if we are in debug-mode.
debugConst = False

-- If debug-mode is enabled print message and execute the
-- expression. Otherwise don't print message.
debug :: String -> a -> a
debug message expression
  | debugConst = trace message expression
  | otherwise = expression

-- Prints the binary representation of 'num'.
printBinaryRepresentation num = showIntAtBase 2 intToDigit num ""
