{-# LANGUAGE BlockArguments #-}

import GHC.IO.Handle (hSetEcho)
import System.IO (stdin)
import Prelude hiding (getLine, putStr, putStrLn)

main :: IO ()
main = do
  hangman

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

hangman :: IO ()
hangman = do
  putStrLn "Think of a word:"
  word <- sgetLine
  putStrLn "Try to geuess it:"
  play word

sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n'
    then do
      putChar x
      return []
    else do
      putChar '-'
      xs <- sgetLine
      return (x : xs)

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

play :: String -> IO ()
play word = do
  putStr "? "
  guess <- getLine
  if guess == word
    then putStrLn "You got it!"
    else do
      putStrLn (match word guess)
      play word

match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '-' | x <- xs]
