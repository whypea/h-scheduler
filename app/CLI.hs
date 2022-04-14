module CLI where

import Control.Monad.Trans.State
import Data.Void
import Control.Applicative
import  Text.Megaparsec
import  Text.Megaparsec.Char
import Data.Time
import Options.Applicative 

type Parser = Parsec Void String 

data Opts = Opts {
    defaults :: [String]
    ,bfill    :: Bool
    ,verbose  :: Bool 
}


cliStart :: IO String
cliStart = do
    putStrLn "Welcome to h-scheduler, start scheduling with sch"
    input <- getLine
    return "unfinished"

help :: IO String
help = return "help"

scheduler :: IO String
scheduler = return ""



 