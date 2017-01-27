module Main where

import System.Environment (getArgs)

import Colorscheme (processImage)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> processImage path
    _ -> putStrLn "Please provide a path"
