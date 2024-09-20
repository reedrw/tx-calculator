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
import Data.List
import Data.List hiding (union)
import Data.Maybe
import Data.List.Ordered

-- Just here because otherwise my editor complains
main :: IO ()
main = do
  putStrLn "Hello, World!"

-- Given a number, return a list of its prime factors
primeFactors :: Int -> [Int]
primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

-- Infinite list of prime numbers
primes :: [Int]
primes = 2 : minus [3..] (foldr (\p r-> p*p : union [p*p+p, p*p+2*p..] r)
                                 [] primes)

-- Procedure to convert a number to its tic xenotation
-- In this example we'll show how to convert 14 to its tic xenotation :(::)
-- 1. Find all its prime factors.     primeFactors 14 = [2, 7]
-- 2. If the list starts with 2, put a colon in the tic xenotation.
-- 3. for each prime factor that is not 2, find the index of the prime factor in the list of primes. 7 is the 4th prime
-- 4. Surround with brackets, and convert the prime factors of the index into xenotation recursively.

xenotate' :: [Int] -> String
xenotate' [] = ""
xenotate' (x:xs)  | x == 2 = ':' : xenotate' xs
                  | otherwise = '(' : inner ++ ")" ++ xenotate' xs
                  where inner = let
                         newPrime = (fromJust $ elemIndex x primes) + 1
                         in xenotate newPrime

xenotate :: Int -> String
xenotate 0 = "((-P)):" -- 0 is a special case that's written like this for some reason (wtf Nick)
xenotate 1 = "(-P):" -- 1 is also a special case, see above
xenotate n = xenotate' $ primeFactors n


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
lexer (':':xs) = Colon : lexer xs
lexer ('(':xs) = LPar : lexer xs
lexer (')':xs) = RPar : lexer xs
lexer (x:xs) = Error x : lexer xs -- Invalid character

sr :: [Token] -> [Token] -> [Token]
sr (Colon:xs) q = sr (ParsedImplex (Num 2) : xs) q
sr (ParsedImplex (Num a): ParsedImplex (Num b):xs) q = sr (ParsedImplex (Num (a * b)) : xs) q   -- Multiply adjacencies
sr (RPar: ParsedImplex (Num a): LPar:xs) q = sr (ParsedImplex (Num (primes !! (a - 1))) : xs) q -- Prime implexion
sr s [] = s
sr s (i:q) = sr (i:s) q

unxenotate :: String -> Int
unxenotate "((-P)):" = 0
unxenotate "(-P):" = 1
unxenotate s = case sr [] $ lexer s of
                    [ParsedImplex (Num a)] -> a
                    _ -> error "Invalid tic xenotation"

