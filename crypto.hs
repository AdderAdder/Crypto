import qualified System.Environment as Sys
import qualified Data.List as List
import qualified System.Random as Random
import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as ByteS
import GenerateKey (generateKey)
-- Used for debugging purpose, remove in final version.
-- To print binary representation use command: showIntAtBase 2 intToDigit number ""
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Debug.Trace (trace)

data Mode = Encrypt | Decrypt deriving (Read,Eq)

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
