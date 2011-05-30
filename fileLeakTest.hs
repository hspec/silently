import System.IO.Silently (silence)
import System.IO (withFile, IOMode(..))
import System.Directory (removeFile)
import System.Info (os)

silentTest n m 
 | n == m    = return m
 | otherwise = safely n (silence (print "testing" >> silentTest (n+1) m))

fileTest n m 
 | n == m    = return m
 | otherwise = safely n (withFile (show n ++ ".tst") AppendMode (\ _ -> fileTest (n+1) m))

safely n io = catch io (\ _ -> return (n-1))

main = do
  let m = 10000
  putStrLn $ "Current OS: " ++ os
  fileMax <- fileTest 1 m
  putStrLn $ "Maximum nested 'withFile' calls: " ++ show fileMax
  silentMax <- silentTest 1 m
  putStrLn $ "Maximum nested 'silence' calls: " ++ show silentMax
  mapM_ (\ n -> removeFile (show n ++ ".tst")) [1..fileMax]