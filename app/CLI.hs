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
    case maybe input of 
      Just "sch" -> scheduler
      Just "help" -> help
      Nothing -> putStrLn "Try again!"

help :: IO String
help = return "help"

scheduler :: IO String
scheduler = return ""
--TODO:
-- set defaults
-- options
----datetime format
----multiple events
----recurrance
----type of events
----
 