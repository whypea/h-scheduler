{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Events where

import Data.Void ()
import System.IO.Unsafe ()
import Control.Applicative ()
import Text.Megaparsec ( Parsec )
import Text.Megaparsec.Char ()
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Time

import Data.Time.Format.ISO8601 ()
import qualified Data.Text as T 
import GHC.IO (unsafePerformIO)
import Data.Data ()
import Control.Lens ()
import Control.Monad.Trans.State (StateT)
import Data.Time.Calendar.Julian (DayOfYear)
import Data.Time.Calendar.OrdinalDate (WeekOfYear)
import GHC.Read (Read(readPrec))
import TextShow 

newtype Datetime = DT UTCTime

instance Show Datetime where
    show (DT a) = formatTime defaultTimeLocale "%Y%m%dT%H%M%S" a    

dtCond :: Char -> Bool
dtCond = \x -> x /= '-' && x /= ':'

--TODO: Maybe possibility for self-defined events?
-- data EventCat = Ord | Dead | Prio | Todo 

--Event as gotten from the parser
--TODO Pevent -> Solver types -> Vevent -> Print


--Types for iCal
data Vevent = NoEvent | Vevent
    {
     eDTstamp :: Datetime        --P
    ,eUID     :: UID            --P (takes Datetime)
    ,eClass   :: EClass         --P
    ,eDTStart :: DateStart      --P
    ,eDTEnd   :: DateStop       --P
    ,eDuration :: Duration      --P
    ,eDescription :: Desc       --P? ()
    ,ePrio :: Priority          --P
    ,eSeq :: EvtSequence        --P
    ,eTimeTrans :: Maybe Transp --P 
    ,eRRule :: Maybe VrRule     --P
    } 

data Transp = TRANSPARENT | OPAQUE deriving (Show, Enum)

data DateStart = DateStart UTCTime 

--TODO make the correct formatting with formatTime
instance Show (DateStart) where
    show (DateStart a) = "DTSTART=" ++ formatTime defaultTimeLocale "%Y%m%dT%H%M%S" a ++ "\n"

instance TextShow DateStart where 
    showb a = fromString (show a) 

data DateStop = DateStop (Maybe UTCTime) 

-- instance TextShow DateStop where
--     showb (Just a) = T.pack
--     showb (Just)

instance Show (DateStop) where
    show (DateStop (Just a)) = "DTSTOP=" ++ formatTime defaultTimeLocale "%Y%m%dT%H%M%S" a    ++ "\n"
    show (DateStop (Nothing)) = ""

data Desc = Desc (Maybe String)

instance Show (Desc) where
    show (Desc (Just a)) = "DESCRIPTION= " ++ show a ++ "\n"
    show (Desc (Nothing)) = ""

data EClass = PUBLIC | PRIVATE | CONFIDENTIAL | IANA String | XNAME String 

instance Show (EClass) where 
    show PUBLIC = "PUBLIC"
    show PRIVATE = "PRIVATE"
    show CONFIDENTIAL = "CONFIDENTIAL"
    show (IANA a) = "IANA" ++ show a
    show (XNAME a) = "XNAME" ++ show a 

newtype UID = UID String deriving (Show)

data Priority = Priority (Maybe Int) 

instance Show Priority where 
    show (Priority (Just a)) = "PRIORITY=" ++ show a ++ "\n"
    show (Priority Nothing) = "" 

data EvtSequence = EvtSequence (Maybe Int)

instance Show EvtSequence where 
    show (EvtSequence (Just a)) = "SEQUENCE=" ++ show a
    show (EvtSequence (Nothing)) = ""

data Duration = Duration (Maybe DiffTime)
    
instance Show Duration where
    show (Duration (Just a)) = "DURATION=" ++ show a 
    show (Duration (Nothing)) = ""

-- instance Show Duration where 
--     show (Duration (Just a)) = 

-- concat $ zipWith (++) (fmap show [eDTstamp, eUID ,eClass,eDTStart,eDescription,ePrio,eSeq,eTimeTrans,eRecur,eAlarm,eRRule])
-- ["DATETIME","UID:", "CLASS:", "DTSTART:", "DESCRIPTION", "PRIORITY:", "SEQUENCE:", "TRANSP:", "RECUR:", "ALARM:", "RRULE:"]--TODO

--TODO this is disgusting
instance Show Vevent where
     show NoEvent = "No Event"
     show (Vevent stamp uid eclass start (DateStop Nothing) duration desc prio seq timet rrule) =
         "BEGIN: VEVENT=\n" ++ show stamp ++ "\n" ++ show uid ++ "\n" ++ "CLASS=" 
         ++ show eclass ++ ";" ++ show start ++ ";" ++ show duration ++ "\n" ++ "DESCRIPTION=" 
         ++ show desc ++ "\n"  ++ show prio ++ "\n" ++ show seq ++ "\n"
         ++ "TRANSP=" ++ show timet ++ "\n" ++ "RECUR=" ++ show rrule ++ "\n" 
         
     show (Vevent stamp uid eclass start stop (Duration Nothing) desc prio seq timet rrule) =
         "BEGIN= VEVENT=\n" ++ "DATETIME="++ show stamp ++ "\n" ++  "UID=" ++ show uid ++ "\n" ++ "CLASS=" 
         ++ show eclass ++ ";" ++ "DTSTART=" ++ show start ++ ";"++ "DTEND="
         ++ show stop ++ "\n" ++ "DESCRIPTION=" ++ show desc ++ "\n" ++ "PRIORITY=" ++
         show prio ++ "\n" ++"SEQUENCE=" ++ show seq ++ "\n" ++ "TRANSP=" ++ show timet ++ "\n" 
         ++ "RECUR=" ++ show rrule ++ "\n"

data VrRule =  NoRule | VrRule   --TODO: NoRule for testing
    {
    rFreq      :: Vrfreq       --P (Freq)
    ,rUntil    :: Until        --P (Day)
    ,rReoccur  :: Countr       --P (Integer) 
    ,rInterval :: Interval     --P (Integer)
    ,rbyMonth  :: ByMonth      --P (Integer)
    ,rbyDay    :: ByDay        --P (Integer)
    ,rbyMonthDay :: MonthDay   --P (Integer)
    ,rByYearDay :: YearDay     --P (Integer)
    ,rByWeekNo :: WeekNo       --P (Integer)
    }

--TODO Generalise the show instances
-- data MShow (a :: Maybe b) where
--     ShowMaybe :: Show b => MShow a
data Vrfreq = HOURLY | DAILY | WEEKLY | MONTHLY | YEARLY
 deriving (Eq, Ord)

instance Show Vrfreq where 
    show (HOURLY) = "FREQ=HOURLY"
    show (DAILY) = "FREQ=DAILY"
    show (WEEKLY) = "FREQ=WEEKLY"
    show (MONTHLY) = "FREQ=MONTHLY"
    show (YEARLY) = "FREQ=YEARLY"


newtype Until = Until (Maybe UTCTime)

instance Show Until where
    show (Until (Just a)) = "UNTIL=" ++ formatTime defaultTimeLocale "%Y%m%dT%H%M%S" a ++ ";"
    show (Until Nothing)    = ""

newtype Countr = Countr (Maybe Integer)

instance Show Countr where
    show (Countr (Just a)) = "COUNT=" ++ show a ++ ";"
    show (Countr Nothing) = ""

newtype Interval = Interval (Maybe Integer)

instance Show Interval where
    show (Interval (Just a)) = "INTERVAL=" ++ show a ++ ";"
    show (Interval Nothing) = ""

newtype ByMonth = ByMonth (Maybe Int)

instance Show ByMonth where
    show (ByMonth (Just a)) = "BYMONTH=" ++ show a ++ ";"
    show (ByMonth Nothing) = ""

newtype ByDay = ByDay (Maybe DayOfWeek) 

instance Show ByDay where
    show (ByDay (Just Monday)) = "BYDAY=MO;"
    show (ByDay (Just Tuesday)) = "BYDAY=TU;"
    show (ByDay (Just Wednesday)) = "BYDAY=WE;"
    show (ByDay (Just Thursday)) = "BYDAY=TH;"
    show (ByDay (Just Friday)) = "BYDAY=FR;"
    show (ByDay (Just Saturday)) = "BYDAY=SA;"
    show (ByDay (Just Sunday)) = "BYDAY=SU;"
    show (ByDay Nothing) = ""

newtype MonthDay = MonthDay (Maybe DayOfMonth)

instance Show MonthDay where
    show (MonthDay (Just a)) = "MONTHDAY=" ++ show a ++ ";"
    show (MonthDay Nothing) = ""

newtype YearDay  = YearDay (Maybe DayOfYear)

instance Show YearDay where
    show (YearDay (Just a)) = "YEARDAY=" ++ show a ++ ";"
    show (YearDay Nothing) = ""

newtype WeekNo = WeekNo (Maybe WeekOfYear)

instance Show WeekNo where
    show (WeekNo (Just a)) = "WEEKNO=" ++ show a ++ ";"
    show (WeekNo Nothing) = ""

--TODO: replace with something like intersperse 
instance Show VrRule where
    show (NoRule) = "No Rule"
    show (VrRule freq until (Countr Nothing) interval mon day md yd wn) =       --
     show freq ++ show until ++ show interval ++ show mon ++ show day ++ show md ++ show yd ++ show wn 
    show (VrRule freq (Until Nothing) reoccur interval mon day md yd wn) =
     show freq ++ show reoccur ++ show interval ++ show mon ++ show day  ++ show md ++ show yd ++ show wn 
    -- show (VrRule freq (Until Nothing) (Countr Nothing) interval mon day _ _ _) =
    --  show freq  ++ show interval ++ show mon ++ show day
    show VrRule{..} = ""

data Vcalendar = Vcalendar
    {
    cProdId       :: String   --TODO needs a generator 
    , cVersion    :: Version
    , cScale      :: Gregorian
    , cTimeZones  :: TZ 
    , cEvents     :: [Vevent]
    }

data Gregorian = GREGORIAN deriving (Show, Read, Enum)

newtype Version = Version String

instance Show Version where
    show (Version a) = "2.0" --Change if the standards update 

newtype TZ = TZ TimeZone --offset in minutes 

instance Show TZ where
    show (TZ a) = show (timeZoneMinutes a) 

instance Show Vcalendar where
    show (Vcalendar cProdId version scale tz events) =
     "BEGIN:VCALENDAR\n" ++ "VERSION="
     ++ show version ++ "\n" ++ "SCALE=" ++ show scale ++ "\n" 
     ++ "TIMEZONE="++ show tz ++ "\n" ++ "EVENTS="++ printList events

printList :: Show a => [a] -> String
printList [] = ""
printList (x:xs) = show x ++ printList xs

--printers

--TODO Change this to an option passed down from CLI
uidID :: [Char]
uidID = "@h-scheduler"


