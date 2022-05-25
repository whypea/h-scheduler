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
import Text.Megaparsec
    ( runParser, errorBundlePretty, ParseErrorBundle )
import  Text.Megaparsec.Char
-- import Data.Time
-- import Data.Void
import Data.Maybe
import qualified Options.Applicative as O
import qualified Options.Applicative.Types as O
import Options.Applicative.Arrows (asA, runA, A (A))
import qualified Data.Text as T
import System.IO

import InputParsers (getStr, getOrdered, getPrioritized)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Reader ( ask, runReader )
import Options.Applicative
import qualified Options.Applicative as O
import qualified Options.Applicative.Common as O
import Solver (econstrSolve, solveToCalendarMake)
import Control.Monad (liftM)
import Data.IORef (newIORef, IORef, modifyIORef, writeIORef, readIORef)

type OState = State [WithRule Ordered] 
type DState = State [Deadline] 
type PState = State [Prioritized]  
type TState = State [Todo]

initO :: OState ()
initO =  state (\x -> ((), x))

addO :: WithRule Ordered -> OState ()
addO x = modify (++[x])

initD :: DState ()
initD = state (\x -> ((), x))

addD :: Deadline -> DState ()
addD x = modify (++[x])

initP :: PState ()
initP = state (\x -> ((), x))

addP :: Prioritized-> PState ()
addP x = modify (++[x])

initT :: TState ()
initT = state (\x -> ((), x))

addT :: Todo -> TState ()
addT x = modify (++[x])
--A cleaner way to do command courtesy of 
--https://github.com/haskell-mafia/mafia/blob/master/src/Mafia/Options/Applicative.hs
command' :: String -> String -> O.Parser a -> O.Mod O.CommandFields a
command' label description parser =
  O.command label (O.info (parser <**> O.helper) (O.progDesc description))

ordered :: String -> (Either (ParseErrorBundle String Void) (WithRule Ordered))
ordered = runParser getOrderedandRule "" --(runParser getOrdered "") 

deadline :: String -> (Either (ParseErrorBundle String Void) Deadline)  
deadline =  runParser getDeadline ""--(runParser getDeadline "") 

priority :: String -> (Either (ParseErrorBundle String Void) Prioritized)
priority=  runParser getPrioritized "" --(runParser getPrioritized "")  

todo :: String -> (Either (ParseErrorBundle String Void) Todo)
todo =  runParser getTodo ""

-- orderedlist :: String -> (Either (ParseErrorBundle String Void) [Ordered])
-- orderedlist = runParser getOrderedList "" --(runParser getOrdered "") 

-- deadlinelist :: String -> (Either (ParseErrorBundle String Void) [Deadline])  
-- deadlinelist =  runParser getDeadlineList ""--(runParser getDeadline "") 

-- prioritylist :: String -> (Either (ParseErrorBundle String Void) [Prioritized])
-- prioritylist =  runParser getPrioritizedList "" --(runParser getPrioritized "")  

-- todolist :: String -> (Either (ParseErrorBundle String Void) [Todo])
-- todolist =  runParser getTodoList ""

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
         <> O.short 'f'
         <> O.value "MyCalendar"
         <> O.help "Filename for the calendar file" )
      <*> O.strOption
          ( O.long "Unique Identifier" 
         <> O.short 'u'
         <> O.metavar "UID"
         <> O.value "h-scheduler"
         <> O.help "Identifier for the instance" )
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
        ACO MakeFile opts  -> do make <- makeICS (filename opts)
                                 workWithHandle2 opts make
                                 return ()
        ACO EditFile opts   -> do let edit = editICS (filename opts)
                                  a <- edit
                                  case a of 
                                    Just h   -> do workWithHandle2 opts (h)
                                                   return ()
                                    Nothing  -> do putStrLn $ "No file named " ++ (filename opts)   
                                                   return ()
    return ()

workWithHandle2 :: Opts -> Handle -> IO ()
workWithHandle2 opts hdl = do
                              print ( "use commands ord, dl, pr, td to add various events")
                              o <- newIORef initO
                              d <- newIORef initD
                              p <- newIORef initP
                              s <- newIORef initT
                              h <- takeCommand opts o d p s hdl
                              return h


takeCommand :: Opts -> IORef (OState () )-> IORef (DState () )-> IORef (PState () )-> IORef (TState () )->  Handle -> IO ()
takeCommand opts o d p t h = do
                        command <- getLine
                        case words command of --Pattern borrowed from the first code example
                              ("ord" : []) -> do
                                              print ( "Add a list of events with a set date, eg. meetings Format:  ")
                                              print ("P(riority): Int; start: date-time ; end: date-time; desc: str [every month | every weekday | x times | No Rule]")
                                              dline <- getLine 
                                              case ordered dline of 
                                                Left bdl -> do print (T.pack $ errorBundlePretty bdl)
                                                               takeCommand opts o d p t h
                                                Right ls -> do  writeIORef o (addO ls)
                                                                takeCommand opts o d p t h
                              ("dl" : []) -> do
                                              print ( "Add tasks with a certain date to finish by ")
                                              print ("P(riority): Int; Deadline: date-time ; duration:hh:mm ")
                                              dline <- getLine 
                                              case deadline dline of 
                                                Left bdl -> do print (T.pack $ errorBundlePretty bdl)
                                                               takeCommand opts o d p t h
                                                Right ls -> do writeIORef d (addD ls)
                                                               takeCommand opts o d p t h
                              ("pr" : []) -> do
                                              print ( "Add tasks with some precedence")
                                              print ("P(recedence): Int; duration: hh:mm ; desc: String")
                                              dline <- getLine 
                                              case priority dline of 
                                                Left bdl -> do print  (T.pack $ errorBundlePretty bdl)
                                                               takeCommand opts o d p t h
                                                Right ls -> do  writeIORef p (addP ls)
                                                                takeCommand opts o d p t h
                              ("td" : []) -> do
                                              print ( "Add tasks to be done sometime")
                                              print ("P(riority): Int; duration: hh:mm; desc: String")
                                              dline <- getLine 
                                              case todo dline of 
                                                Left bdl -> do print (T.pack $ errorBundlePretty bdl)
                                                               takeCommand opts o d p t h
                                                Right ls -> do  writeIORef t (addT ls)
                                                                takeCommand opts o d p t h
                              ("solve" : []) -> do
                                                  ops <- readIORef o
                                                  dl <- readIORef d
                                                  pr <- readIORef p
                                                  td <- readIORef t
                                                  hPutStr h $ (show $ solveToCalendarMake opts (exS ops) (exS dl) (exS pr) (exS td))
                                                  hClose h
                                                  return ()
                              ("exit" : []) -> do
                                                  return ()    
                              _  -> do print "Invalid command" 
                                       takeCommand opts o d p t h
    where exS x = execState x []
                              

--econstrSolve .
--pass down the lists 
-- 
-- parse :: O.Parser a
-- parse = O.subparser (O.command "string" (O.info O.auto O.parseCommand ))





