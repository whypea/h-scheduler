module Main where
import CLI
import Events
import InputParsers
import Solver


main :: IO ()
main = do putStrLn "h-scheduler"
          cli
