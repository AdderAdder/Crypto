import qualified System.Environment as Sys
import qualified Data.List as List
import qualified System.Random as Random
import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as ByteS
import Data.Int (Int64)
import GenerateKey (generateKey)

-- Used for debugging purpose, remove in final version.
-- To print binary representation use command: showIntAtBase 2 intToDigit number ""
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Debug.Trace (trace)

-- Constant variable indicating if we are in debug-mode.
debugConst = False

debug :: String -> a -> a
debug message expression
  | debugConst = trace message expression
  | otherwise = expression

-- Here the actual program start.

data Mode = Encrypt | Decrypt deriving (Read,Eq)

main :: IO()
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
                           ByteS.writeFile ("encrypted_" ++ fileName) (rsa Encrypt file n e)
process (mode:fileName:n:c:[]) = do file <- (ByteS.readFile fileName)
                                    ByteS.writeFile newFileName (rsa (read mode) file (read n :: Integer) (read c :: Integer))
                                    where
                                    newFileName = if (read mode) == Encrypt then "encrypted_" ++ fileName else "decrypted_" ++ fileName
process _ = putStrLn "Error when parsing argument.\nPlease enter a filepath to the file that you wish to encrypt.\nIf you want to specify the key to use for encryption/decryption use format\n'Encrypt/Decrypt fileToEncrypt moduloNumber exponent'."

powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod num exp n = if mod exp 2 == 0 then rec else mod (num*rec) n
                  where rec = powMod (mod (num*num) n) (div exp 2) n

-- Encryption/decryption (depending on what mode is given as parameter) of the content.
rsa :: Mode -> ByteS.ByteString -> Integer -> Integer -> ByteS.ByteString
rsa mode file n exp = rsa' file n exp chunkSize byteSizeOfAns
                    where
                    blockSize = floor $ logBase 2 $ fromIntegral n
                    tmp = div blockSize 8
                    chunkSize = if mode == Encrypt then tmp else (tmp+1)
                    byteSizeOfAns = fromIntegral  $ if mode == Encrypt then chunkSize+1 else chunkSize-1

-- Core function of rsa that recursively takes a chunk of the data and apply rsa
-- encryption/decryption on that chunk. Then the new number is transformed into
-- a bytestring.
rsa' :: ByteS.ByteString -> Integer -> Integer -> Int64 -> Integer -> ByteS.ByteString
rsa' file n exp chunk bytes
 | file == ByteS.empty = ByteS.empty
 | otherwise = debug ("Append chunk of bytestring: " ++ show byteEncryptedNum) $ ByteS.append byteEncryptedNum (rsa' (ByteS.drop chunk file) n exp chunk bytes)
              where
              bitNum = debug ("Chunk of bytestring: " ++ show (ByteS.take chunk file)) $ ByteS.foldl (\acc w -> (fromIntegral w) Bits..|. (Bits.shiftL acc 8) :: Integer) Bits.zeroBits (ByteS.take chunk file)
              num = fromIntegral bitNum :: Integer
              encryptNum = debug ("Num: " ++ show num) $ fromIntegral (powMod num exp n) :: Integer
              byteEncryptedNum = debug ("Num after manipulation: " ++ show encryptNum) $ (ByteS.pack . map fromIntegral) $ numToByteArray encryptNum bytes

-- Transform a large integer to an array of ints (that are 1 byte each).
numToByteArray :: Integer -> Integer -> [Int]
numToByteArray num iter = reverse $ numToIntArray num iter
                        where
                        numToIntArray _ 0 = []
                        numToIntArray num iter = (fromIntegral (255 Bits..&. num)):(numToIntArray (Bits.shiftR num 8) (iter-1))
