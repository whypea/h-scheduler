module CLI where

import Control.Monad.Trans.State
import Data.Void
import Control.Applicative
import Data.Semigroup
import  Text.Megaparsec
import  Text.Megaparsec.Char
import Data.Time
import qualified Options.Applicative as O

 

data Opts = Opts {
     filename :: String
    ,uidsuffix :: String
    ,bfill    :: Bool
    ,verbose  :: Bool
    ,input    :: Bool 
}

-- commands = 

 
sample :: O.Parser Opts
sample = Opts
      <$> O.strOption
          ( O.long "filename"
         <> O.metavar "FILE"
         <> O.value "MyCalendar"
         <> O.help "Filename for the calendar file" )
      <*> O.strOption
          ( O.long "Unique Identifier" 
         <> O.short 'u'
         <> O.metavar "UID"
         <> O.value "@h-scheduler"
         <> O.help "Identifier for the instance" ) 
      <*> O.switch
          ( O.long "bfill"
         <> O.short 'b'
         <> O.help "Do/don't ask for every single field")
      <*> O.switch
          ( O.long "verbose"
         <> O.short 'v'
         <> O.help "How many words to use")
      <*> O.switch
          ( O.long "input"
         <> O.short 'i'
         <> O.help "Make (True) or edit (False) file")



--TODO make it accept values (meaning, run commands)
--TODO make it quit
--TODO sleep and wakeup times? (should be implied honestly) 
cli :: IO () 
cli = do 
    options <- O.execParser opts
    return ()
  where
    opts = O.info (sample <**> O.helper) --(<**>) :: Applicative f => f a -> f (a -> b) -> f b
      ( O.fullDesc
     <> O.progDesc "Produce a ics file"
     <> O.header "h-scheduler" ) 

scheduler :: IO String
scheduler = return ""



 