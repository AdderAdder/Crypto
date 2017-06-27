import qualified System.Environment as Sys
import qualified Data.List as List
import qualified System.Random as Random
import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as ByteS
import Data.ByteString.Builder (integerDec,toLazyByteString)
import GenerateKey (generateKey)
-- Used for debugging purpose, remove in final version.
-- To print binary representation use command: showIntAtBase 2 intToDigit number ""
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Debug.Trace (trace)

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
                           ByteS.writeFile ("encrypt" ++ fileName) (rsa Encrypt file n e)
process (mode:fileName:n:c:[]) = do file <- (ByteS.readFile fileName)
                                    ByteS.writeFile newFileName (rsa (read mode) file (read n :: Integer) (read c :: Integer))
                                    where
                                    newFileName = if (read mode) == Encrypt then "encrypt" ++ fileName else "decrypt" ++ fileName
process _ = putStrLn "Error when parsing argument.\nPlease enter a filepath to the file that you wish to encrypt.\nIf you want to specify the key to use for encryption (or decryption) use format 'Encrypt/Decrupt fileToEncrypt productOfPrime exponent'."

powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod num exp n = if mod exp 2 == 0 then rec else mod (num*rec) n
                  where rec = powMod (mod (num*num) n) (div exp 2) n

-- Encryption/decryption (depending on what mode is given as parameter) of the content.
-- Note that padding is done to the content so it can be choped up into bytes.
rsa :: Mode -> ByteS.ByteString -> Integer -> Integer -> ByteS.ByteString
rsa mode file n exp
 | file == ByteS.empty = ByteS.empty
 | otherwise = ByteS.append byteEncryptedNum (rsa Encrypt (ByteS.drop q file) n exp)
              where
              blockSize = floor $ logBase 2 $ fromIntegral n
              tmp = div blockSize 8
              q = if mode == Encrypt then tmp else (tmp+1)
              bitNum = ByteS.foldl (\acc w -> (fromIntegral w) Bits..|. (Bits.shiftL acc 8) :: Integer) Bits.zeroBits (ByteS.take q file)
              num = fromIntegral bitNum :: Integer
              encryptNum = fromIntegral (powMod num exp n) :: Integer
              byteSizeOfEncrypt = fromIntegral  $ if mode == Encrypt then q+1 else q-1
              byteEncryptedNum = trace ("Num after manipulation: " ++ show encryptNum) $ (ByteS.pack . map fromIntegral) $ reverse $ numToByteString encryptNum byteSizeOfEncrypt
              numToByteString :: Integer -> Integer -> [Int]
              numToByteString _ 0 = []
              numToByteString num iter = (fromIntegral (255 Bits..&. num)):(numToByteString (Bits.shiftR num 8) (iter-1))
