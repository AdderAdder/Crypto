import qualified System.Environment as Sys

main = do args <- Sys.getArgs
          process args

process :: [String] -> IO()
process (fileName:[]) = do file <- (readFile fileName)
                           rsa file generateKey

process (fileName:p:q:e:[]) = do file <- (readFile fileName)
                                 rsa file $ (read p :: Integer):(read q :: Integer):( read e :: Integer):[]

process _ = putStrLn "Error when parsing argument.\nPlease enter filepath to file that you wish to encrypt.\nOptionally the key to use for encryption can also be specified on format 'fileToEncrypt p q e'."

rsa :: String -> [Integer] -> IO()
rsa content (p:q:e:[]) = print "Done"

generateKey :: [Integer]
generateKey = [5,7,3]
