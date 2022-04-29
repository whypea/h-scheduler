{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts   #-}

module Events where
import Data.Void
import System.IO.Unsafe

import Control.Applicative
import Text.Megaparsec ( Parsec )
import  Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Time
import Data.Time.Format.ISO8601
import Data.Text ( Text, pack, intersperse )
import GHC.IO (unsafePerformIO)
import Class (Class)
import Data.Data
import Control.Monad.Trans.State (StateT)
import GHC.Conc (pseq)

newtype Datetime = DT UTCTime

instance Show Datetime where
    show (DT a) = filter (/= '-') (iso8601Show a) ++ ";"  



--Types for iCal
data Vevent = Vevent
    {
    eDTstamp :: Datetime
    ,eUID     :: UID
    ,eClass   :: Text
    ,eDTStart :: UTCTime
    ,eDTEnd   :: UTCTime 
    ,eDescription :: String
    ,ePrio :: MT Text
    ,eSeq :: MT Text
    ,eTimeTrans :: MT Text
    ,eRecur :: MT Text
    ,eAlarm :: MT Text
    ,eRRule :: VrRule
    }

newtype UID = UID String deriving (Show)

-- concat $zipWith (++) (fmap show [eDTstamp, eUID ,eClass,eDTStart,eDescription,ePrio,eSeq,eTimeTrans,eRecur,eAlarm,eRRule])
-- concat $zipWith (++) (fmap show [eDTstamp, eUID ,eClass,eDTStart,eDescription,ePrio,eSeq,eTimeTrans,eRecur,eAlarm,eRRule])
-- ["DATETIME","UID:", "CLASS:", "DTSTART:", "DESCRIPTION", "PRIORITY:", "SEQUENCE:", "TRANSP:", "RECUR:", "ALARM:", "RRULE:"]--TODO



instance Show Vevent where
     show (Vevent stamp uid eclass start end desc prio seq timet recur alarm rrule) =
         "BEGIN=" ++ "VEVENT=" ++ "DATETIME= "++ show stamp ++ ";" ++  "UID=" ++ show uid ++ ";"  ++ show uid
         ++ "CLASS=" ++ show eclass ++ ";" ++ "DTSTART=" ++ show start ++ ";"++ "DTEND="
         ++ show end ++ ";"  ++ "DESCRIPTION=" ++ show desc ++ ";" ++ "PRIORITY=" ++
         show prio ++ ";" ++ "SEQUENCE=" ++ show seq ++ ";"++ "TRANSP=" ++ show timet++ ";" ++
         "RECUR=" ++ show recur ++ ";" ++ "ALARM=" ++ show alarm ++ ";" ++ "RRULE=" ++ show rrule

newtype MT a= MT {a :: Maybe a}

instance Show a => Show (MT a) where
    show (MT (Just a)) = show a
    show (MT Nothing) = show ""

data VrRule = VrRule
    {
    rFreq      :: Vrfreq
    ,rUntil    :: Until
    ,rReoccur  :: Reoccur
    ,rInterval :: Interval
    ,rbyMonth  :: ByMonth
    ,rbyDay    :: ByDay
    }

newtype Until = Until (Maybe Day)

instance Show Until where
    show (Until (Just day)) = "UNTIL=" ++ filter (/= '-') (showGregorian day) ++ ";"
    show (Until Nothing)    = ""

newtype Reoccur = Reoccur (Maybe Integer)

instance Show Reoccur where
    show (Reoccur (Just a)) = "COUNT=" ++ show a ++ ";"
    show (Reoccur Nothing) = ""

newtype Interval = Interval (Maybe Integer)

instance Show Interval where
    show (Interval (Just a)) = "INTERVAL=" ++ show a ++ ";"
    show (Interval Nothing) = ""

newtype ByMonth = ByMonth (Maybe Integer)

instance Show ByMonth where
    show (ByMonth (Just a)) = "BYMONTH=" ++ show a ++ ";"
    show (ByMonth Nothing) = ""

newtype ByDay = ByDay (Maybe Integer)

instance Show ByDay where
    show (ByDay (Just a)) = "BYDAY=" ++ show a ++ ";"
    show (ByDay Nothing) = ""

instance Show VrRule where
    show (VrRule freq until (Reoccur Nothing) interval mon day) =
     show freq ++ show until ++ show interval ++ show mon ++ show day
    show (VrRule freq (Until Nothing) reoccur interval mon day) =
     show freq ++ show reoccur ++ show interval ++ show mon ++ show day
    show (VrRule freq (Until Nothing) (Reoccur Nothing) interval mon day) =
     show freq  ++ show interval ++ show mon ++ show day
    show VrRule{..} = ""
-- The UNTIL or COUNT rule parts are OPTIONAL, but they MUST NOT occur in the same 'recur'.
--replace with something like intersperse 


--Cutting out some of the choices
data Vrfreq = HOURLY | DAILY | WEEKLY | MONTHLY | YEARLY
 deriving (Show, Eq, Ord)

data Vcalendar = Vcalendar
    {
    cProdId       :: Text
    , cVersion    :: String
    , cScale      :: String
    , cTimeZones  :: String
    , cEvents     :: [Vevent]
    }

instance Show Vcalendar where
    show (Vcalendar cProdId version scale tz events) =
     "BEGIN:\n " ++ "VERSION="
     ++ show version ++ "\n" ++ "SCALE=" ++ show scale ++ "TIMEZONE="++ show tz ++ "EVENTS="++ show events

--printers

makeUDI ::  UTCTime
makeUDI = unsafePerformIO getCurrentTime





