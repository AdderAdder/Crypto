import qualified System.Environment as Sys
import qualified Data.List as List
import qualified System.Random as Random
import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as ByteS
-- Used for debugging purpose, remove in final version.
-- To print binary representation use command: showIntAtBase 2 intToDigit number ""
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Debug.Trace (trace)

data Mode = Encrypt | Decrypt deriving (Read,Eq)

primeNumberBitLength = 32

main = do args <- Sys.getArgs
          process args

-- Process the command line arguments and takes appropriate action for each scenario.
process :: [String] -> IO()
process (fileName:[]) = do file <- (ByteS.readFile fileName)
                           seedFirstPrime <- Random.newStdGen
                           randSeed <- Random.newStdGen
                           let (n,e,d) = generateKey seedFirstPrime randSeed
                           writeFile "rsaEncryptionKey.txt" $ (show n) ++ " " ++ (show e)
                           writeFile "rsaDecryptionKey.txt" $ (show n) ++ " " ++ (show d)
                           ByteS.writeFile ("encrypt" ++ fileName) (rsa Encrypt file n e)
process (mode:fileName:n:c:[]) = do file <- (ByteS.readFile fileName)
                                    ByteS.writeFile newFileName (rsa (read mode) file (read n :: Integer) (read c :: Integer))
                                    where
                                    newFileName = if (read mode) == Encrypt then "encrypt" ++ fileName else "decrypt" ++ fileName
process _ = putStrLn "Error when parsing argument.\nPlease enter a filepath to the file that you wish to encrypt.\nIf you want to specify the key to use for encryption (or decryption) use format 'Encrypt/Decrupt fileToEncrypt productOfPrime exponent'."

-- Returns a bit representation of the value passed as first argument.
-- Second argument shoule preferably be the result of 'sizeof val'.
--toBits :: (Bits.Bits b) => a -> Int -> b -> b
toBits _ (-1) ans = ans
toBits val bitPos bit = toBits val (bitPos-1) newBit
                        where newBit = if (Bits.testBit val bitPos) then Bits.setBit bit bitPos else Bits.clearBit bit bitPos

-- Encryption/decryption (depending on what mode is given as parameter) of the content.
-- Note that padding is doen to the content so it can be choped up into equally
-- sized pieces.
rsa :: Mode -> ByteS.ByteString -> Integer -> Integer -> ByteS.ByteString
rsa Encrypt file n e = ByteS.append encryptedChunk (rsa Encrypt (ByteS.drop (fromIntegral q) file) n e)
                      where
                      blockSize = ceiling $ logBase 2 $ fromIntegral n
                      (q,r) = quotRem blockSize 8
                      chunk = (ByteS.foldl (\bitRep w -> (toBits w 7 Bits.zeroBits) Bits..|. (Bits.shift bitRep 8)) Bits.zeroBits (ByteS.take (fromIntegral q) file))
                      tmpChunk = fromIntegral (Bits.shiftR (8-r) (Bits.shiftL (8-r) (mod (chunk^e) (fromIntegral n))))
                      encryptedChunk = tmpChunk :: ByteS.ByteString
                      --chunk = ByteS.take q file
                      --chunk = (ByteS.foldl (\bitRep w -> Bits.(.|.) w (Bits.shift bitRep 8)) Bits.zeroBits (take q file)) :: Integer
                      --encryptedChunk = mod (chunk^e) n

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
      d = multiplicativeInverse e phi
  in (p*q,e,d)

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
