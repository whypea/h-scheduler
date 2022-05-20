{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE Arrows #-}
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
import Options.Applicative.Arrows (asA, runA, A (A))
import qualified Data.Text as T
import System.IO

import Text.Megaparsec (parseMaybe)
import InputParsers (getStr)
import Data.Maybe (fromMaybe)

data Opts = Opts {
     filename :: String
    ,uidsuffix :: String
    ,bfill    :: Bool
    ,verbose  :: Bool
    -- ,input    :: Bool
    --,command  :: Commands 
} 
    deriving Show 

data ActualCommands = EditFile (IO (Maybe Handle))  
              | CreateFile (IO Handle) 
              | OrderedList (Maybe Ordered)   
              | DeadlineList (Maybe Deadline) 
              | PrioritizedList (Maybe Prioritized)   
              | TodoList (Maybe Todo) 

data Commands = ParseString (Maybe String)
              | ParseNum (Maybe Int)
              | Exit
              | Options Opts
    deriving (Show)

data ACO = ACO ActualCommands Opts 

stringreader = O.strArgument 

--A cleaner way to do command courtesy of 
--https://github.com/haskell-mafia/mafia/blob/master/src/Mafia/Options/Applicative.hs
command' :: String -> String -> O.Parser a -> O.Mod O.CommandFields a
command' label description parser =
  O.command label (O.info (parser <**> O.helper) (O.progDesc description))

--read the name value
createFile :: ActualCommands 
createFile = CreateFile $ makeICS "name"

editFile :: ActualCommands
editFile = EditFile $ editICS ""

orderedlist :: ActualCommands
orderedlist = OrderedList $ parseMaybe (getOrdered) ""   

deadlinelist :: ActualCommands
deadlinelist = DeadlineList $ parseMaybe (getDeadline) ""

priolist :: ActualCommands
priolist = PrioritizedList $ parseMaybe (getPrioritized) ""

todolist :: ActualCommands
todolist = TodoList $ parseMaybe (getTodo) ""

actCommands :: O.Parser ActualCommands
actCommands =  O.subparser 
        ( (command' "ord" "Add a list of events with a set date, eg. meetings" (pure orderedlist))
         <> (command' "dl" "Add tasks with a certain date to finish by" (pure deadlinelist))
         <> (command' "prio" "Add tasks with some priority" (pure priolist)) 
         <> (command' "td" "Add tasks to be done sometime" (pure todolist)) ) 

-- actCommandsasA = asA actCommands 

--Options are a parser (permutation)
--TODO how to actually read the options
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
         <> O.value "h-scheduler"
         <> O.help "Identifier for the instance" )
      <*> O.switch
          ( O.long "bfill"
         <> O.short 'b'
         <> O.help "Do/don't ask for every single field")
      <*> O.switch
          ( O.long "verbose"
         <> O.short 'v'
         <> O.help "How many words to use")


opts :: O.ParserInfo Opts
opts = O.info (topOptions <**> O.helper) 
    ( O.fullDesc
    <> O.progDesc "Produce a ics file"
    <> O.header "h-scheduler" ) 

commandoptParser :: O.Parser ACO
commandoptParser = runA $ proc () -> do 
                       opts <- asA topOptions -< ()
                       cmds <- asA actCommands -< ()
                       A O.helper -< ACO cmds opts      --makes the 

--TODO Actual control structure with options, not just 
cli :: IO () 
cli = do 
    go <- O.execParser (O.info commandoptParser (O.fullDesc <> O.progDesc "Description")) 
    case go of 
        ACO (OrderedList a) _ -> do
            print (a)
        ACO (PrioritizedList a) _ -> do
            print (a)
        ACO (DeadlineList a) _ -> do
            print (a)
        ACO (TodoList a) _ -> do
            print (a)
    return ()

-- parse :: O.Parser a
-- parse = O.subparser (O.command "string" (O.info O.auto O.parseCommand ))





