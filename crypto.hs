import qualified System.Environment as Sys
import qualified Data.List as List
import qualified System.Random as Random
import qualified Data.Bits as Bits
-- Used for debugging purpose, remove in final version.
-- To print binary representation use command: showIntAtBase 2 intToDigit number ""
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- Datastructure to hold if we are encrypting or decrypting
data Mode = Encrypt | Decrypt deriving (Read)

main = do args <- Sys.getArgs
          process args

-- Process the command line arguments and takes appropriate action for each scenario.
process :: [String] -> IO()
process (mode:fileName:[]) = do file <- (readFile fileName)
                                seedFirstPrime <- Random.newStdGen
                                seedSecondPrime <- Random.newStdGen
                                let p = generatePrime 42 seedFirstPrime
                                rsa (read mode) file (p) 5
process (mode:fileName:n:c:[]) = do file <- (readFile fileName)
                                    rsa (read mode) file (read n :: Integer) (read c :: Integer)
process _ = putStrLn "Error when parsing argument.\nPlease enter filepath to file that you wish to encrypt.\nOptionally the key to use for encryption can also be specified on format 'fileToEncrypt p q e'."

rsa :: Mode -> String -> Integer -> Integer -> IO()
rsa mode content n c = print "Done"

-- Naive prime checking. Should be changed to the Miller-Rabbit function instead.
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
      newSeed = generateNewSeed seed b
      generateNewSeed s 0 = s
      generateNewSeed s iter = generateNewSeed (snd (Random.random s)) (iter-1)
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
-- Note that the 'iteration' argument should not be too large as searching is a
-- expensive procedure.
searchForPrime :: Integer -> Int -> Maybe Integer
searchForPrime _ 0 = Nothing
searchForPrime number iteration
  | isPrime (number+2) = Just (number+2)
  | otherwise = searchForPrime (number+2) (iteration-1)
