{-# LANGUAGE BlockArguments #-}

import Prelude hiding (getLine, putStr, putStrLn)

main :: IO ()
main = do
  strlen

getLine :: IO String
getLine = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- getLine
      return (x : xs)

putStr :: String -> IO ()
putStr [] = return ()
putStr (x : xs) = do
  putChar x
  putStr xs

putStrLn :: String -> IO ()
putStrLn xs = do
  putStr xs
  putChar '\n'

strlen :: IO ()
strlen = do
  putStr "Enter a string: "
  x <- getLine
  putStr "The string has "
  putStr (show (length x))
  putStrLn " charactoers"
