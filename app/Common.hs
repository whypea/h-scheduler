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

data ParseEvent = ParseEvent
    {event :: Vevent            --Event
    ,prio  :: Int               --Priority
    ,pSET  :: (UTCTime,UTCTime) --Start/end time
    ,dur   :: DiffTime          --estimated duration, relevant 
    } 

instance Show ParseEvent where 
    show (ParseEvent event prio pSet dur) = show prio ++ "-" ++ show (pSet^._1) ++ "-" ++ show (pSet^._2) ++ "-" ++ show dur

newtype Scheduled = Scheduled {sEvent :: ParseEvent} deriving (Show)
newtype Ordered = Ordered {oEvent :: ParseEvent} deriving (Show)           --Set date, eg. meetings   
newtype Deadline = Deadline {dEvent :: ParseEvent} deriving (Show)            --Certain date to finish by, 
newtype Prioritized = Prioritized {pEvent :: ParseEvent} deriving (Show)    --timed tasks with a priority, flexible
newtype Todo  = Todo {tEvent :: ParseEvent} deriving (Show)                 --Any time, priority has lower pecedence than above

-----FROM SOlVER
wake :: DiffTime
bed :: DiffTime
(wake,bed) = (secondsToDiffTime 32400, secondsToDiffTime 75600)

getDT :: UTCTime -> DiffTime
getDT = utctDayTime

--Can store deadline in pSET_2 , zero for unscheduled values
utczero :: UTCTime
utczero = UTCTime (fromGregorian 1858 11 17) (0) 

deadpCompare :: Deadline -> Deadline -> Ordering
deadpCompare p1 p2 = compare (prio . dEvent $ p1) (prio . dEvent $ p2)

priopCompare :: Prioritized -> Prioritized -> Ordering
priopCompare p1 p2 = compare (prio . pEvent $ p1) (prio . pEvent $ p2)

todopCompare :: Todo -> Todo -> Ordering
todopCompare p1 p2 = compare (prio . tEvent $ p1) (prio . tEvent $ p2)

--Find an open timeslot by checking if stoptime and starttime are less than the duration
hasTimetest :: UTCTime -> UTCTime -> DiffTime -> Bool
hasTimetest stop start dur = comp == LT -- || comp == EQ
    where comp = compare dur (getDT stop - getDT start) 

withinDay :: DiffTime -> Bool 
withinDay x = x > wake && x < bed 

--Compares time 
timeCompare :: ParseEvent -> ParseEvent -> Bool
timeCompare p1 p2 = diffUTCTime ((pSET $ p1)^._1) ((pSET $ p2)^._2) < 0 && diffUTCTime ((pSET $ p1)^._2) ((pSET $ p2)^._1) > 0 
                    && withinDay (utctDayTime ((pSET $ p1)^._1)) && withinDay (utctDayTime ((pSET $ p2)^._1))
                    && withinDay (utctDayTime ((pSET $ p1)^._2)) && withinDay (utctDayTime ((pSET $ p2)^._2))

getsStop :: Scheduled -> UTCTime
getsStop x = (pSET . sEvent $ x)^._2

getsStart :: Scheduled -> UTCTime
getsStart x = (pSET . sEvent $ x)^._1

getdStart :: Deadline -> UTCTime 
getdStart x= (pSET . dEvent $ x)^._1

getdStop :: Deadline -> UTCTime 
getdStop x= (pSET . dEvent $ x)^._2



