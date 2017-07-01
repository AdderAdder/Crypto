# Crypto

This is a simple test project for trying to implement an RSA encryption program in the haskell programming language. The purpose of the project was simply to learn the fundamentals of RSA encryption and how to create a naive implementation. Haskell was chosen because I felt I needed more experience in the language.

Fundamental knowledge about the encryption method and how to implement it was acquired through reading the following online materials
* [The wikipedia page on RSA](https://simple.wikipedia.org/wiki/RSA_(algorithm%29)
* [A post about implementing RSA by Sahand Saba](http://sahandsaba.com/cryptography-rsa-part-1.html)
* [Slides about implementing RSA in Java](http://db.cs.duke.edu/courses/cps001/summer04/lectures/Lecture16.pdf)
* [The wikipedia page on Miller-Rabin's primality test](https://en.wikipedia.org/wiki/Miller–Rabin_primality_test)

## Short description of project

The general program takes a file and encrypts/decrypts the content of the file. The program then stores the encrypted/decrypted content in a new file named either encrypted_ + OldFileName or decrypted_ + OldFileName. The program should work if you have the full Haskell Platform installed.

**Crypto.hs** is the main file and should be compiled. You can then run the program without any arguments to learn about the input format. When encryption keys are generated they are stored in the files encryptionKey.txt and decryptionKey.txt as plain text numbers separated by a whitespace. If new keys are generated these files will be overwritten and the old keys thus deleted (can be avoided by renaming the files before generating new keys).

**GenerateKey.hs** contains the code for generating the encryption keys. Here one can also modify the size of the primes used to generate the keys (currently set to 512 bits).

**PrimeNumberGenerator.hs** contains the code for generating prime numbers. The Miller-Rabin primality test is used to determine primality using 100 witnesses (can be changed by modifying the file).

**Debug.hs** contains code which has been used to debug the program.      

**test.txt** contains nonsense and was used to test the program.
