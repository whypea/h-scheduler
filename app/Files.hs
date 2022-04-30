{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts   #-}

module Files where

import InputParsers
import Events


import System.IO
import System.Directory

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Time.LocalTime

-- getICS :: FilePath -> Maybe Handle 
-- getICS (fp) = fp 

testCal = Vcalendar "123" (Version "2") GREGORIAN (TZ (TimeZone 0 False "UTC")) []

makeICS :: String -> IO Handle
makeICS name = openFile (name ++ ".ics") WriteMode 
                 

writeVCalendar :: Handle -> Vcalendar -> IO ()
writeVCalendar hdl cal = hPutStrLn hdl (show cal) 


doPrintTest :: IO () 
doPrint = do h <- makeICS "test"
             writeVEvent h testCal
             hClose h
             return ()

-- getICS :: String -> Either IO ()
-- getICS name = writeFile name 

