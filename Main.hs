module Main (main) where

import System.Console.GetOpt
import System.Environment
import System.Exit

import Config

main :: IO ()
main = do
    args <- getArgs
    (config, dir1, dir2) <- parseArgs args
    exitSuccess
