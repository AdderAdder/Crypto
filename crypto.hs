module Main where

import qualified System.Environment as Sys
import qualified System.Random as Random
import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as ByteS
import Data.Word (Word8)
import Data.Int (Int64)
import GenerateKey (generateKey)
import PrimeNumberGenerator (powMod)
import Debug (debug)

data Mode = Encrypt | Decrypt deriving (Read,Eq)

main :: IO()
main = do args <- Sys.getArgs
          process args

-- Process the command line arguments and takes appropriate action for each scenario.
process :: [String] -> IO()
process (fileName:[]) = do file <- (ByteS.readFile fileName)
                           seed <- Random.newStdGen
                           let (n,e,d) = generateKey seed
                           writeFile "encryptionKey.txt" $ (show n) ++ " " ++ (show e)
                           writeFile "decryptionKey.txt" $ (show n) ++ " " ++ (show d)
                           ByteS.writeFile ("encrypted_" ++ fileName) (rsa Encrypt file n e)
process (m:fileName:n:c:[]) = do file <- (ByteS.readFile fileName)
                                 ByteS.writeFile newFileName $ rsa mode file (read n :: Integer) (read c :: Integer)
                                 where
                                 mode = read m :: Mode
                                 newFileName = if mode == Encrypt then "encrypted_" ++ fileName else "decrypted_" ++ fileName
process _ = putStrLn "Error when parsing argument.\nPlease use one of the follow formats for the arguments.\n1) 'fileToEncrypt' (will create new keys in the process and overwrite the old keys)\n2) 'Encrypt/Decrypt fileToEncrypt moduloNumber exponent'"

-- Encryption/decryption (depending on what mode is given as parameter) of the content.
rsa :: Mode -> ByteS.ByteString -> Integer -> Integer -> ByteS.ByteString
rsa mode file n exp = rsa' file n exp chunkSize ansChunkSize
                    where
                    bitSize = floor $ logBase 2 $ fromIntegral n
                    tmp = div bitSize 8
                    chunkSize = if mode == Encrypt then tmp else (tmp+1)
                    ansChunkSize = if mode == Encrypt then chunkSize+1 else chunkSize-1

-- Core function of rsa that recursively takes a chunk of the data (given by 'chunk') and apply rsa
-- encryption/decryption on that chunk. Then the new number is transformed into
-- a bytestring (size given by 'bytes').
rsa' :: ByteS.ByteString -> Integer -> Integer -> Int64 -> Int64 -> ByteS.ByteString
rsa' file n exp chunk bytes
 | file == ByteS.empty = ByteS.empty
 | otherwise = debug ("Append chunk of bytestring: " ++ show byteEncryptedNum) $ ByteS.append byteEncryptedNum $ rsa' (ByteS.drop chunk file) n exp chunk bytes
              where
              num = debug ("Chunk of bytestring: " ++ show (ByteS.take chunk file)) $ ByteS.foldl (\acc w -> (fromIntegral w) Bits..|. (Bits.shiftL acc 8)) Bits.zeroBits (ByteS.take chunk file) :: Integer
              encryptNum = debug ("Num: " ++ show num) $ powMod num exp n
              byteEncryptedNum = debug ("Num after manipulation: " ++ show encryptNum) $ ByteS.pack $ numToByteArray encryptNum bytes

-- Transform a large integer to an array of bytes.
numToByteArray :: Integer -> Int64 -> [Word8]
numToByteArray num iter = reverse $ numToWordArray num iter
                        where
                        numToWordArray _ 0 = []
                        numToWordArray num iter = (fromIntegral (255 Bits..&. num)):(numToWordArray (Bits.shiftR num 8) (iter-1))
