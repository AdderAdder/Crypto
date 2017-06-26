module PrimeNumberGenerator (generatePrime) where

import qualified System.Random as Random
import qualified Data.Bits as Bits

-- Naive prime checking. Should be changed to the Rabin-Miller function instead.
isPrime :: Integer -> Bool
isPrime number
  | mod number 2 == 0 = (number == 2)
  | mod number 3 == 0 = (number == 3)
  | otherwise = foldl (\prime val -> if (mod number (val-1) == 0) || (mod number (val+1) == 0) then prime && False else prime && True) True [6,12..sqrtNumber]
    where
    sqrtNumber = 1 + (truncate (sqrt (fromIntegral number)))

-- Try to generate a prime of b bits using a random seed.
generatePrime :: Int -> Random.StdGen -> Integer
generatePrime b seed =
  let number = generateBitNumber b seed
      searchResult = searchForPrime number (2*b)
      newSeed = Random.mkStdGen (fst (Random.random seed))
  in if isPrime number then number
     else if searchResult /= Nothing then (\(Just x) -> x) searchResult
     else generatePrime b newSeed

-- Generates a random number of b bits by using a random seed.
generateBitNumber :: Int -> Random.StdGen -> Integer
generateBitNumber b seed = (Bits.setBit num 0) :: Integer
                 where
                 randPos = map (\x -> mod x b) (take b (Random.randoms seed))
                 num = foldl (\bits pos -> Bits.setBit bits pos) (Bits.bit (b-1)) randPos

-- Searches if there is a prime close to the 'number' argument.
-- Note that the 'iteration' argument should not be too large as searching is an
-- expensive procedure.
searchForPrime :: Integer -> Int -> Maybe Integer
searchForPrime _ 0 = Nothing
searchForPrime number iteration
  | isPrime (number+2) = Just (number+2)
  | otherwise = searchForPrime (number+2) (iteration-1)
