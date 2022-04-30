{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts   #-}

module Files where

import InputParsers
import Events
import Solver

import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char


getICS :: FilePath -> IO String 
getICS = 

