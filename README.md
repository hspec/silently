[![Hackage version](https://img.shields.io/hackage/v/silently.svg?label=Hackage&color=informational)](http://hackage.haskell.org/package/silently)
[![silently on Stackage Nightly](https://stackage.org/package/silently/badge/nightly)](https://stackage.org/nightly/package/silently)
[![Stackage LTS version](https://www.stackage.org/package/silently/badge/lts?label=Stackage)](https://www.stackage.org/package/silently)
[![Cabal build](https://github.com/hspec/silently/workflows/Haskell-CI/badge.svg)](https://github.com/hspec/silently/actions)

# silently

Silently is a package that allows you to run an `IO` action and
prevent it from writing to `stdout`, or any other handle, by using
`silence`. Or you can capture the output for yourself using `capture`.

For example, the program
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
will print:
```
 putStrLn: puppies!
 silenced:
 captured: wookies!
 returned: 123
```

## Limitations

Capturing/silencing might not work as expected if the action uses the FFI
or conceals output under `unsafePerformIO` or similar unsafe operations.

Examples:
- FFI: https://github.com/hspec/silently/issues/3
- `unsafePerformIO`: https://github.com/bos/filemanip/issues/22
