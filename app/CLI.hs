module CLI where

import Control.Monad.Trans.State
import Data.Void
import Control.Applicative
import  Text.Megaparsec
import  Text.Megaparsec.Char
import Data.Time
--import Options.Applicative 

type Parser = Parsec Void String 


cliStart :: IO String
cliStart = do
    putStrLn "Welcome to h-scheduler, start scheduling with sch"
    input <- getLine
    return "unfinished"

help :: IO String
help = return "help"

scheduler :: IO String
scheduler = return ""



 