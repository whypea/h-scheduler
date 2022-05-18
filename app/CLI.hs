{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE BlockArguments      #-}
module CLI where

import InputParsers
import Events
import Files
import Common


import Control.Monad.Trans.State
import Data.Void
import Control.Applicative
import Data.Semigroup
import  Text.Megaparsec
import  Text.Megaparsec.Char
import Data.Time
import Data.Void
import Data.Maybe
import qualified Options.Applicative as O
import GHC.IO.FD (stdin)
import qualified Data.Text as T
import Control.Applicative (liftA)
import Options.Applicative (progDesc)
import Text.Megaparsec (parseMaybe)
import InputParsers (getStr)

 

data Opts = Opts {
    filename :: String
    ,uidsuffix :: String
    ,bfill    :: Bool
    ,verbose  :: Bool
    ,input    :: Bool
}

-- TODO commands, or rewrite the whole cli section 
--TODO make it accept values (meaning, run commands)
--TODO make it quit

data ActualCommands = EditFile Opts
              | CreateFile Opts
              | OrderedList Opts
              | DeadlineList Opts 
              | PrioritizedList Opts
              | TodoList [Todo]

data Commands = ParseString (Maybe String)
              | ParseNum (Maybe Int)
              | Exit
    deriving (Show)

-- createFile :: 

-- editFile :: 

-- orderedlist :: 

-- deadlinelist 

--A cleaner way to do command courtesy of 
--https://github.com/haskell-mafia/mafia/blob/master/src/Mafia/Options/Applicative.hs
command' :: String -> String -> O.Parser a -> O.Mod O.CommandFields a
command' label description parser =
  O.command label (O.info (parser <**> O.helper) (O.progDesc description))

-- command :: String -> O.ParserInfo a -> O.Mod O.CommandFields a 
-- command = subparser (command) 

-- commands :: [Commands]
-- commands = [O.command ]

commands :: O.Parser Commands
commands = O.subparser 
           (  O.command "str" 
              ((O.info (pure parseString) (O.progDesc "parse string")  ))
           <> O.command "num"
              ((O.info (pure parseNum) (O.progDesc "parse num"))  )
           )    

stringreader = O.strArgument 

commands' :: O.Parser Commands
commands' = O.subparser 
        ((command' "str" "parse a string" (pure parseString))
         <> (command' "num" "parse a number" (pure parseNum))
         <> (command' "e" "exit" (pure Exit)) ) 

parseString :: Commands 
parseString = ParseString $ parseMaybe (fromMaybe "" <$> getStr) "" 

parseNum :: Commands
parseNum = ParseNum $ parseMaybe (fromMaybe 0 <$> getNumType) "0"



-- parseString :: s -> Commands 
-- parseString a = ParseString ( parseMaybe (getStr) a)

-- parseNum :: Commands 
-- parseNum a = ParseNum <$> (parseMaybe (getMNum) a)

--Options are a parser (permutation)
topOptions :: O.Parser Opts
topOptions = Opts
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


--TODO get options first, then run go and do a lambda case?
cli :: IO () 
cli = do 
    options <- O.execParser (opts)
    go <- O.execParser (O.info commands (O.fullDesc <> O.progDesc "Description")) 
    case go of 
        ParseString a -> do
            print (a)
            print ("and also") 
            str <- getLine
            print $ fromMaybe "" (parseMaybe getStr' str)
        ParseNum a-> do
            print (a)
            str <- getLine
            print $ fromMaybe 0 (parseMaybe (getNumType') str)
        Exit -> return ()
    return ()
opts :: O.ParserInfo Opts
opts = O.info (topOptions <**> O.helper) 
    ( O.fullDesc
    <> O.progDesc "Produce a ics file"
    <> O.header "h-scheduler" ) 

-- parse :: O.Parser a
-- parse = O.subparser (O.command "string" (O.info O.auto O.parseCommand ))


     --   <*> O.auto
    --       ( O.long "Start Time" 
    --      <> O.short 'i'
    --      <> O.metavar "20220507"
    --      <> O.help "Start time for the calendar" ) 


