module Main where

import Lib

main :: IO ()
main = do
  putStrLn ("Ergebnis für 'new': " ++ show (accept "new"))
  putStrLn ("Ergebnis für 'not': " ++ show (accept "not"))
