#!/usr/bin/env runhaskell

import System.Environment
import Data.List
import Data.Maybe
import Data.Char
import Data.Numbers.Primes

-- This is program takes in a number and converts it into Daniel C. Barker's
-- tic xenotation.
--
-- From http://hyperstition.abstractdynamics.org/archives/003538.html
-- Tic Xenotation works like this:
--
-- [I've used colons for Barker's tic dots and placed tic-clusters in quotes for clarity]
--
-- ':' counts as '2' or 'x 2', with a value exactly equivalent to '2' in a factor string
-- So:
-- ':' = 2, '::' = 4, ':::' = 8
-- The second notational element consists of implexions, where '(n)' = the nth prime.
-- Implexion raises the hyperprime index of any number by 1. Examples (from the hyprime 'mainlain'):
-- '(:)' = 3 (2nd prime),
-- '((:))' = 5 (3rd prime),
-- '(((:)))' = 11 (5th prime),
-- '((((:))))' = 31 (11th prime)
-- '(((((:)))))' = 127 (31st prime)
--
-- Numbers constellate as normal factor strings, i.e. 55 (5 x 11) is tic xenotated as '((:))(((:)))'

main :: IO ()
main = let
  helpMessage =
    putStrLn "Usage: tx <number>" >>
    putStrLn "Converts a number to or from tic xenotation" >>
    putStrLn "" >>
    putStrLn "Options:" >>
    putStrLn "  -h, --help    Show this help message and exit" >>
    putStrLn "" >>
    putStrLn "Examples:" >>
    putStrLn "  tx 14" >>
    putStrLn "  tx '((:))(((:)))'"
  encode arg = putStrLn $ xenotate $ read arg
  decode arg = putStrLn $ show $ unxenotate arg
  in do
  args <- getArgs
  case (length args) of
    0 -> helpMessage
    _ -> case arg of
      (x:xs) | (x:xs) == "-h"
            || (x:xs) == "--help" -> helpMessage
      (x:xs) | x == '('
            || x == ':'
            || x == ')' -> decode arg
      (x:xs) | all isDigit (x:xs) -> encode arg
      _ -> error "Input is not a valid number"
      where arg = args !! 0

-- Since TX indexes primes starting from 1, adding something to index 0 will
-- make the index of the prime factors match their TX representation
primes' :: [Int]
primes' = 1:primes

-- Procedure to convert a number to its TX representation
-- In this example we'll show how to convert 14 to its tic xenotation :(::)
-- 1. Find all its prime factors.     primeFactors 14 = [2, 7]
-- 2. If the list starts with 2, put a colon in the tic xenotation.
-- 3. for each prime factor that is not 2, find the index of the prime factor in the list of primes. 7 is the 4th prime
-- 4. Surround with brackets, and convert the prime factors of the index into xenotation recursively.

-- Convert a list of prime factors into TX
xenotate' :: [Int] -> String
xenotate' [] = ""
xenotate' (x:xs) | x == 2 = ':' : xenotate' xs
                 | True   = '(' : inner ++ ')' : xenotate' xs
                 where inner = let
                        n = fromJust $ elemIndex x primes'
                        in xenotate' $ primeFactors n

xenotate :: Int -> String
xenotate n | n == 0 = "((-P)):" -- 0 is a special case that's written like this for some reason (wtf Nick)
           | n == 1 = "(-P):"   -- 1 is also a special case, see above
           | True   = xenotate' $ primeFactors n


-- We can do the reverse of this by parsing the string and converting it back to a number
-- Using the shift-reduce algorithm
-- https://en.wikipedia.org/wiki/Shift-reduce_parser
data Token = Colon | LPar | RPar
          | Error Char
          | ParsedImplex Implex
  deriving (Show)

data Implex =  Num Int
  deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer ('(':xs) = LPar  : lexer xs
lexer (')':xs) = RPar  : lexer xs
lexer (':':xs) = Colon : lexer xs
lexer (x:xs)   = Error x : lexer xs -- Invalid character

sr :: [Token] -> [Token] -> [Token]
sr (Colon:xs) q = sr (ParsedImplex (Num 2):xs) q
sr (ParsedImplex (Num a):ParsedImplex (Num b):xs) q = sr (ParsedImplex (Num $ a * b):xs) q   -- Multiply adjacencies
sr (RPar:ParsedImplex (Num a):LPar:xs) q = sr (ParsedImplex (Num $ primes' !! a):xs) q  -- Prime implexion
sr s [] = s
sr s (i:q) = sr (i:s) q

unxenotate :: String -> Int
unxenotate "((-P)):" = 0
unxenotate "(-P):" = 1
unxenotate s = case sr [] $ lexer s of
                    [ParsedImplex (Num a)] -> a
                    _ -> error "Invalid tic xenotation"

