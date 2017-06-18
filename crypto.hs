import qualified System.Environment as Sys
import qualified Data.List as List
import qualified System.Random as Random
import qualified Data.Bits as Bits
-- Just only for debugging purpose, remove in final version
-- To print binary representation use command: showIntAtBase 2 intToDigit number ""
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

main = do args <- Sys.getArgs
          process args

process :: [String] -> IO()
process (fileName:[]) = do file <- (readFile fileName)
                           randP <- Random.randomRIO (10,1000)
                           randQ <- Random.randomRIO (10,1000)
                           seedE <- Random.newStdGen
                           let [p,q,e] = generateKey randP randQ seedE
                           writeFile "key.txt" $ (show p) ++ " " ++ (show q) ++ " " ++ (show e)
                           rsa file [p,q,e]
process (fileName:p:q:e:[]) = do file <- (readFile fileName)
                                 rsa file $ (read p :: Integer):(read q :: Integer):(read e :: Integer):[]
process _ = putStrLn "Error when parsing argument.\nPlease enter filepath to file that you wish to encrypt.\nOptionally the key to use for encryption can also be specified on format 'fileToEncrypt p q e'."

rsa :: String -> [Integer] -> IO()
rsa content (p:q:e:[]) = print "Done"

generateKey :: Integer -> Integer -> Random.StdGen -> [Integer]
generateKey randP randQ seedE = [p,q,e]
                                where
                                p = getPrime randP
                                q = getPrime randQ
                                e = generateE ((p-1)*(q-1)) seedE 0

generateE :: Integer -> Random.StdGen -> Integer -> Integer
generateE phi seedE e
  | gcd e phi == 1 = e
  |otherwise = generateE phi newSeed newE
               where
               (newE,newSeed) = Random.randomR (3,phi-1) seedE

-- Modified implementation of the simple sieve of Eratosthenes for finding primes
-- based on the implementation given at https://wiki.haskell.org/Prime_numbers
-- To be changed to a non-deterministic prime generator using the Miller-Rabbit method.
getPrime :: Integer -> Integer
getPrime n
  | n < 1 = error "Input has to be larger than 0."
  | n == 1 = 2
  | otherwise = sieve n [3,5..10000]
                where
                sieve 2 (x:xs) = x
                sieve m (x:xs) = sieve (m-1) (xs List.\\ [x,x+x..10000])

isPrime :: Integer -> Bool
isPrime number
  | mod number 2 == 0 = (number == 2)
  | mod number 3 == 0 = (number == 3)
  | otherwise = foldl (\prime val -> if (mod number (val-1) == 0) || (mod number (val+1) == 0) then prime && False else prime && True) True [6,12..sqrtNumber]
    where
    sqrtNumber = 1 + (truncate (sqrt (fromIntegral number)))

generatePrime :: Int -> Random.StdGen -> Maybe Integer
generatePrime b seed =
  let number = generateBitNumber b seed
      searchResult = searchForPrime number (2*b) number
  in if isPrime number then Just number
     else Nothing
     --else if searchResult /= number then Just searchResult

generateBitNumber :: Int -> Random.StdGen -> Integer
generateBitNumber b seed = (Bits.setBit num 0) :: Integer
                 where
                 randPos = map (\x -> mod x b) (take b (Random.randoms seed))
                 num = foldl (\bits pos -> Bits.setBit bits pos) (Bits.bit (b-1)) randPos

searchForPrime :: Integer -> Int -> Integer -> Integer
searchForPrime number iteration basecase = basecase
