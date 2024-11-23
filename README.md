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

## Not thread-safe

Since all threads of a process share the standard output handles `stdout` and `stderr`,
capturing output to these handle will capture the output of __all threads__,
not just the one running the action under `capture`.

In essence, this library does not work in a situation where multiple threads are writing to the handle
whose output _produced by the given action_ we want to capture.

See:
-  https://github.com/hspec/silently/issues/6

## Further limitations

Capturing/silencing might not work as expected if the action uses the FFI
or conceals output under `unsafePerformIO` or similar unsafe operations.

Examples:
- FFI: https://github.com/hspec/silently/issues/3
- `unsafePerformIO`: https://github.com/bos/filemanip/issues/22
