{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase #-}

module Files where

import InputParsers
import Events


import System.IO
import System.Directory

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Time.LocalTime
import Data.Maybe

testCal = Vcalendar "123" (Version "2") GREGORIAN (TZ (TimeZone 0 False "UTC")) []

makeICS :: String -> IO Handle
makeICS name = openFile (name ++ ".ics") WriteMode 
                 

writeVCalendar :: Handle -> Vcalendar -> IO ()
writeVCalendar hdl cal = hPutStrLn hdl (show cal) 

doPrintTest :: String -> IO () 
doPrintTest name = do dfe <- doesFileExist (name ++ ".ics") 
                      if dfe 
                          then do h <- openFile (name ++ ".ics") ReadWriteMode
                                  writeVCalendar h testCal
                                  hClose h
                                  return ()
                          else do putStrLn "File does not exist"
                                  return () 
                      

    
-- doesFileExist (name ++ ".ics") >>= \case 
--                   True -> Just (openFile (name ++ ".ics") ReadWriteMode )
--                   False -> Nothing 



