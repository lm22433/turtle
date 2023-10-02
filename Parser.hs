module Parser where

import Data.Char qualified as Char
import Data.List qualified as List

-- Abstract syntax tree for Turtle programs
data Program = Cmd Command | Seq Command Program
  deriving (Eq, Show)

-- Abstract syntax tree nodes corresponding to atomic commands
data Command = Fd Int | Lt Int | Rt Int | Up | Dn
  deriving (Eq, Show)

-- `rmWhiteSpace s` evaluates to a string that is identical to `s`
-- except that all space, tab and newline characters are removed.
rmWhiteSpace :: String -> String
rmWhiteSpace s = filter (not . Char.isSpace) s

-- `digitsPrefix s` evaluates to a pair of strings `(s1, s2)` in
-- which `s1` is the largest prefix of `s` containing only digits
-- and `s2` is the remainder of `s` after removing this prefix.
--
--   e.g. digitsPrefix "8232);up" = ("8232", ";up")
--        digitsPrefix "up;down"  = ([], "up;down")
--        digitsPrefix "fd(100);down" = ([], "fd(100);down")
--
digitsPrefix :: String -> (String, String)
digitsPrefix s = List.span Char.isDigit s

-- If `s` is a string of shape `ds ++ t` with `ds` a sequence of
-- one or more digits, then `parseInt s` evaluates the pair `(n, t)`
-- where `n` is the integer corresponding to the substring `ds`.
--
--   e.g. digitsPrefix "8232);up" = (8232, ";up")
--        digitsPrefix "up;down"  = ERROR
--        digitsPrefix "fd(100);down" = ERROR
--
parseInt :: String -> (Int, String)
parseInt s =
  if not (null digits)
    then (read digits, rest)
    else error "Parse error: expected a digit."
  where
    (digits, rest) = digitsPrefix s

--
-- If `s` is a string of shape `'(' ++ ds ++ ') ++ t'` with `ds` a substring
-- consisting only of one or more digits, then `parseArg s` evaluates to
-- the pair `(n, t)` where `n` is the integer corresponding to the digits `ds`.
--
-- e.g. parseArg "(32);bk(2);up" = (32, ";bk(2);up")
--      parseArg "(6);right(90);fd(10);up" = (6, ";right(90);fd(10);up")
--      parseArg "(200)" = (200, [])
--      parseArg "200);up" = ERROR
--      parseArg "(200;up" = ERROR
--
parseArg :: String -> (Int, String)
parseArg s0 =
  case s0 of
    '(' : s1 ->
      let (n, s2) = parseInt s1
       in case s2 of
            ')' : s3 -> (n, s3)
            _ -> error "Parse error: expected ')'."
    _ -> error "Parse error: expected '('."

--
-- If string `s` has shape `cs ++ rest` with `cs` a substring corresponding
-- to one of the commands, then `parseCmd s` evaluates to `(c, rest)`
-- where `c` is the corresponding command in the abstract syntax tree
--
--   e.g. parseCmd "down;fd(20);right(60)" = (Dn, ";fd(20);right(60)")
--        parseCmd "fd(23);right(10);up" = (Fd 23, ";right(10);up")
--        parseCmd "left(100);right(10);up" = (Lt 100, ";right(10);up")
--        parseCmd "right(90)" = (Rt 90, [])
--
parseCmd :: String -> (Command, String)
parseCmd ('u' : 'p' : s) = (Up, s)
parseCmd ('d' : 'o' : 'w' : 'n' : s) = (Dn, s)
parseCmd ('f' : 'd' : s) = (Fd n, s1) where (n, s1) = parseArg s
parseCmd ('l' : 'e' : 'f' : 't' : s) = (Lt n, s1) where (n, s1) = parseArg s
parseCmd ('r' : 'i' : 'g' : 'h' : 't' : s) = (Rt n, s1) where (n, s1) = parseArg s
parseCmd _ = error "Parse error: expected a command."

-- If `s` is a valid Turtle program, `parseProg s` evaluates to
-- a pair `(p, [])` in which `p` is the AST of the program described by `s`.
parseProg :: String -> Program
parseProg s0
  | (c, ';' : s1) <- parseCmd s0 = Seq c (parseProg s1)
  | (c, s1) <- parseCmd s0 = Cmd c

-- If `s` is a valid Turtle program, `parse s` evaluates to the AST of `s`.
parse :: String -> Program
parse s = parseProg (rmWhiteSpace s)
