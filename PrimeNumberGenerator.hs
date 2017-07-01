module PrimeNumberGenerator (generatePrime,powMod) where

import qualified System.Random as Random
import qualified Data.Bits as Bits

-- Const for how many random numbers to use when running the
-- Miller-Rabin algorithm.
millerRabinTestSize = 100

-- Try to generate a prime of 'b' bits using a random seed.
generatePrime :: Int -> Random.StdGen -> Integer
generatePrime b seed =
  let number = generateBitNumber b seed
      searchResult = searchForPrime number (2*b) seed
      newSeed = Random.mkStdGen (fst (Random.random seed))
  in if isPrime number millerRabinTestSize seed then number
     else if searchResult /= Nothing then (\(Just x) -> x) searchResult
     else generatePrime b newSeed

-- Generates a random number of 'b' bits by using a random seed.
generateBitNumber :: Int -> Random.StdGen -> Integer
generateBitNumber b seed = (Bits.setBit num 0) :: Integer
                 where
                 randPos = map (\x -> mod x b) (take b (Random.randoms seed))
                 num = foldl (\bits pos -> Bits.setBit bits pos) (Bits.bit (b-1)) randPos

-- Checks if there is a prime close to the 'number' argument.
-- Note that the 'iteration' argument should not be too large as searching is an
-- expensive procedure.
searchForPrime :: Integer -> Int -> Random.StdGen -> Maybe Integer
searchForPrime _ 0 _ = Nothing
searchForPrime number iteration seed
  | isPrime (number+2) 20 seed = Just (number+2)
  | otherwise = searchForPrime (number+2) (iteration-1) seed

-- Preformes mod (num^exp) n but faster through repeated squaring.
powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod num exp n = if mod exp 2 == 0 then tmp else mod (num*tmp) n
                  where tmp = powMod (mod (num*num) n) (div exp 2) n

-- Miller-Rabin primality test based on pseudocode found at
-- https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
isPrime :: Integer -> Int -> Random.StdGen -> Bool
isPrime num k seed = iterate randNums
                    where
                    randNums = map (\x -> 2 + (mod x (num-4))) (take k (Random.randoms seed))
                    (r,d) = factorize (num-1) 0

                    factorize :: Integer -> Integer -> (Integer,Integer)
                    factorize x r = if mod x 2 == 0 then factorize (div x 2) (r+1) else (r,x)

                    iterate :: [Integer] -> Bool
                    iterate [] = True
                    iterate (x:xs) = let calc = (powMod x d num)
                                     in if calc == 1 || calc == (num-1) then iterate xs
                                     else if not (check calc (r-1) num)
                                     then False else iterate xs

                    check :: Integer -> Integer -> Integer -> Bool
                    check _ (-1) _ = False -- Needed if r = 0 as (r-1) is the -1. This happens for example if num = 22
                    check _ 0 _ = False
                    check x times n = let xx = mod (x*x) n
                                      in if xx == 1 then False
                                      else if xx == (n-1) then True
                                      else check xx (times-1) n
