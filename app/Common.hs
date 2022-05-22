{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE BlockArguments      #-}

module Common where

import Events
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Time
import Data.Void
import Control.Lens
import Data.Maybe

-----FROM SOlVER

getDT :: UTCTime -> DiffTime
getDT = utctDayTime

--Can store deadline in pSET_2 , zero for unscheduled values
utczero :: UTCTime
utczero = UTCTime (fromGregorian 1858 11 17) (0) 

getsStop :: Scheduled -> UTCTime
getsStop x = (pSET . sEvent $ x)^._2

getsStart :: Scheduled -> UTCTime
getsStart x = (pSET . sEvent $ x)^._1

getoStart :: Ordered -> UTCTime 
getoStart x = (pSET . oEvent $ x)^._1

getoStop :: Ordered -> UTCTime 
getoStop x = (pSET . oEvent $ x)^._2

getdStart :: Deadline -> UTCTime 
getdStart x = (pSET . dEvent $ x)^._1

getdStop :: Deadline -> UTCTime 
getdStop x = (pSET . dEvent $ x)^._2

getpStart :: Prioritized -> UTCTime 
getpStart x = (pSET . pEvent $ x)^._1

getpStop :: Prioritized -> UTCTime 
getpStop x = (pSET . pEvent $ x)^._2



