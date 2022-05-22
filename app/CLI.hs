{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
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
-- import Data.Time
-- import Data.Void
import Data.Maybe
import qualified Options.Applicative as O
import qualified Options.Applicative.Types as O
import Options.Applicative.Arrows (asA, runA, A (A))
import qualified Data.Text as T
import System.IO

import Text.Megaparsec (parseMaybe)
import InputParsers (getStr, getOrdered, getPrioritized)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Reader ( ask, runReader )
import Options.Applicative
import qualified Options.Applicative as O
import qualified Options.Applicative.Common as O


--A cleaner way to do command courtesy of 
--https://github.com/haskell-mafia/mafia/blob/master/src/Mafia/Options/Applicative.hs
command' :: String -> String -> O.Parser a -> O.Mod O.CommandFields a
command' label description parser =
  O.command label (O.info (parser <**> O.helper) (O.progDesc description))


orderedlist :: String -> (Either (ParseErrorBundle String Void) Ordered)
orderedlist = runParser getOrdered "" --(runParser getOrdered "") 

deadlinelist :: String -> (Either (ParseErrorBundle String Void) Deadline)  
deadlinelist =  runParser getDeadline ""--(runParser getDeadline "") 

prioritylist :: String -> (Either (ParseErrorBundle String Void) Prioritized)
prioritylist =  runParser getPrioritized "" --(runParser getPrioritized "")  

todolist :: String -> (Either (ParseErrorBundle String Void) Todo)
todolist =  runParser getTodo ""

actCommands :: O.Parser ActualCommands
actCommands =  O.subparser 
        ( (command' "make" "make/overwrite a file 'filename' "  (pure MakeFile))
         <> (command' "edit" "edit the file named 'filename' " (pure EditFile)))

--Options are a parser (permutation) for a record type, can 

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
      <*> O.strOption
          ( O.long "Calendar Start"
         <> O.short 'c'
         <> O.help "Start of the calendar, used for starting a calendar")
      <*> O.strOption
          ( O.long "Wake-up time"
         <> O.short 'w'
         <> O.value "8:00"
         <> O.value "h-scheduler"
         <> O.help "When to wake up, hh:mm")
      <*> O.strOption
          ( O.long "Bedtime"
         <> O.short 'b'
         <> O.value "22:00"
         <> O.help "When to go to bed, hh:mm")

opts :: O.ParserInfo Opts
opts = O.info (topOptions <**> O.helper) 
    ( O.fullDesc
    <> O.progDesc "Produce a ics file"
    <> O.header "h-scheduler" ) 

commandoptParser :: O.Parser ACO
commandoptParser = runA $ proc () -> do 
                       opts <- asA topOptions -< ()
                       cmds <- asA actCommands -< ()
                       A O.helper -< ACO cmds opts      --makes the combined type

--TODO Actual control structure with options, not just tests
--TODO Add the solvers into this, reading in bed, wake etc. 
cli :: IO () 
cli = do 
    go <- O.execParser (O.info commandoptParser (O.fullDesc <> O.progDesc "Description")) 
    case go of 
        ACO MakeFile opts  -> do let a = makeICS (filename opts)
                                 return ()
        ACO EditFile opts   -> do let s = editICS (filename opts)
                                  a <- s
                                  case a of 
                                    Just h   -> do k <- workWithHandle (fromJust "" s)
                                                   return ()
                                    Nothing  -> do putStrLn $ "No file named " ++ (filename opts)   
                                                   return ()
    return ()

workWithHandle :: IO Handle -> IO Handle
workWithHandle hdl = do h <- hdl
                        print ( "Add a list of events with a set date, eg. meetings \n Format:  ")
                        ord <- getLine
                        case orderedlist ord of 
                            Left bdl -> print (errorBundlePretty bdl) 
                                        return hdl
                            Right ls -> hPrint hdl ls 
                         
                --                 print ("Add tasks with a certain date to finish by")
                --                 return hdl
                -- -- dline <- getLine
                -- case deadlinelist dline of 
                --     Left bdl -> print (errorBundlePretty bdl)
                --                 return hdl
                --     Right ls -> hPrint hdl ls
                -- print ("Add tasks with some priority")
                -- prio <- getLine
                -- case prioritylist prio of 
                --     Left bdl -> print (errorBundlePretty bdl)
                --     Right ls -> hPrint hdl ls
                -- print ("Add tasks to be done sometime")
                -- todol <- getLine
                -- case todolist todol of 
                --     Left bdl -> print (errorBundlePretty bdl)
                --                 return h
                --     Right ls -> hPrint ls

-- parse :: O.Parser a
-- parse = O.subparser (O.command "string" (O.info O.auto O.parseCommand ))





