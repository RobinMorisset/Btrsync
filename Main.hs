module Main (main) where

import System.Console.GetOpt
import System.Environment
import System.Exit

import Config
import Hashing

main :: IO ()
main = do
    args <- getArgs
    (config, t1, t2) <- parseArgs args
    ExitSuccess
