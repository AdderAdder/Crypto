module GenerateKey (generateKey) where

import qualified System.Random as Random
import PrimeNumberGenerator (generatePrime)

-- How big prime numbers to generate. Given in bits.
primeNumberBitLength = 1024 

-- Based on the code sample from https://rosettacode.org/wiki/Modular_inverse#Haskell
-- A more detailed description of how the algorithm works can be found at https://stackoverflow.com/questions/12544086/calculate-the-extended-gcd-using-a-recursive-function-in-python
-- Output is on the form (cofNum1,cofNum2,gcd)
extendedGCD :: Integer -> Integer -> (Integer,Integer,Integer)
extendedGCD num 0 = (1,0,num)
extendedGCD num1 num2 = let (quot,rem) = quotRem num1 num2
                            (cof1,cof2,gcdAns) = extendedGCD num2 rem
                        in (cof2,cof1-quot*cof2,gcdAns)

-- Based on the code sample from https://rosettacode.org/wiki/Modular_inverse#Haskell
-- Output the value x that solve num1*x = 1 (mod num 2)
-- Note that an error is thrown if no such x exist.
multiplicativeInverse :: Integer -> Integer -> Integer
multiplicativeInverse num1 num2 = let (x,_,gcdAns) = extendedGCD num1 num2
                                  in if gcdAns /= 1 then error "No multiplicative inverse found!" else x

-- Takes two random seeds to generate two prime numbers and returns a tuple
-- on the form (n,e,d). (n,e) is used for encryption and (n,d) is used for decryption.
generateKey :: Random.StdGen -> Random.StdGen -> (Integer,Integer,Integer)
generateKey seedFirstPrime randSeed =
  let p = generatePrime primeNumberBitLength seedFirstPrime
      seedSecondPrime = until (\x -> (generatePrime primeNumberBitLength x) /= p) (Random.mkStdGen . fst . Random.random) randSeed
      q = generatePrime primeNumberBitLength seedSecondPrime
      phi = (p-1)*(q-1)
      seedE = until (\x -> gcd (fst (Random.randomR (3,phi-1) x)) phi == 1) (Random.mkStdGen . fst . Random.random) randSeed
      e = (fst (Random.randomR (3,phi-1) seedE))
      tmp = multiplicativeInverse e phi
      d = if tmp < 0 then phi+tmp else tmp
  in (p*q,e,d)
