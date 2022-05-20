{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase #-}

module Files where

import InputParsers
import Events


import System.IO
import System.Directory ( doesFileExist )

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Time.LocalTime
import Data.Maybe
import GHC.IO.Handle.Types (Handle(DuplexHandle))

testCal :: Vcalendar
testCal = Vcalendar (Prod "") (Version "2") GREGORIAN (TZ (TimeZone 0 False "UTC")) []

makeICS :: String -> IO Handle
makeICS name = openFile (name ++ ".ics") WriteMode 
                 
writeVCalendar :: Handle -> Vcalendar -> IO ()
writeVCalendar hdl cal = hPutStrLn hdl (show cal)

editICS :: String -> IO (Maybe Handle) 
editICS name = do dfe <- doesFileExist (name ++ ".ics") 
                  if dfe 
                          then do h <- openFile (name ++ ".ics") ReadWriteMode
                                  writeVCalendar h testCal
                                  return (Just h)
                          else do return Nothing
                                  

    




