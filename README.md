Silently is a package that allows you to run an IO action and prevent it from writing to stdout, or any other handle, by using "silence". Or you can capture the output for yourself using "capture".

For example; the program
```haskell
 import System.IO.Silently

 main = do
   putStr "putStrLn: " >> putStrLn "puppies!"
   putStr "silenced: " >> silence (putStrLn "kittens!")
   putStrLn ""
   (captured, result) <- capture (putStr "wookies!" >> return 123)
   putStr "captured: " >> putStrLn captured
   putStr "returned: " >> putStrLn (show result)
```
will print
```
 putStrLn: puppies!
 silenced:
 captured: wookies!
 returned: 123
```
