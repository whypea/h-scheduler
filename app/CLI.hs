{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module CLI where

import Control.Monad.Trans.State
import Data.Void
import Control.Applicative
import Data.Semigroup
import  Text.Megaparsec
import  Text.Megaparsec.Char
import Data.Time
import qualified Options.Applicative as O
import Options.Applicative (switch)
 

data Opts = Opts {
     defaults :: String
    ,oUID     :: String
    ,bfill    :: Bool
    ,verbose  :: Bool
    ,input    :: Bool 
}
--parserInfo 
sample :: O.Parser Opts
sample = Opts
      <$> O.strOption
          ( O.long "defaults"
         <> O.metavar "TARGET"
         <> O.help "Target for the greeting" )
      <*> O.strOption
          ( O.long "Identification"
         <> O.metavar "oUID"
         <> O.help "Target for the greeting" )
      <*> O.switch
          ( O.long "bfill"
         <> O.short 'b'
         <> O.help "Do/don't ask for " )
      <*> O.switch
          ( O.long "verbose"
         <> O.short 'v'
         <> O.help "How many words to use")
      <*> O.switch
          ( O.long "input"
         <> O.short 'i'
         <> O.help "Edit a file or input one")



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



 