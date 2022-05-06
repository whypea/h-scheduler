{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}


module Events where
import Data.Void
import System.IO.Unsafe

import Control.Applicative
import Text.Megaparsec ( Parsec )
import  Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Time
import Data.Time.Format.ISO8601
import qualified Data.Text as T ( Text, pack, intersperse )
import GHC.IO (unsafePerformIO)
import Data.Data
import Control.Monad.Trans.State (StateT)
import Data.Time.Calendar.Julian (DayOfYear)
import Data.Time.Calendar.OrdinalDate (WeekOfYear)
 
newtype Datetime = DT UTCTime

instance Show Datetime where
    show (DT a) = filter (dtCond) (iso8601Show a) ++ ";"  

dtCond :: Char -> Bool
dtCond = \x -> x /= '-' && x /= ':'

--TODO: Maybe possibility for self-defined events?
data EventCat = Ord | Dead | Prio | Todo 

--Priority and DTStart/DTEnd
data Pevent = Pevent 
    {event :: Vevent
    ,prio  :: Int 
    ,pSET  :: (UTCTime,UTCTime) --Start/end time
    }

--Types for iCal
data Vevent = Vevent
    {
    eDTstamp :: Datetime --P
    ,eUID     :: UID    --P (takes Datetime)
    ,eClass   :: T.Text
    ,eDTStart :: UTCTime --P
    ,eDTEnd   :: UTCTime --P
    ,eDescription :: String --P? ()
    ,ePrio :: MT T.Text       --P
    ,eSeq :: MT T.Text        --P
    ,eTimeTrans :: Transp     --TODO 
    ,eAlarm :: MT T.Text      --TODO 
    ,eRRule :: VrRule
    }

data Transp = TRANSPARENT | OPAQUE deriving (Show, Enum)

data EClass token = PUBLIC | PRIVATE | CONFIDENTIAL | IANA token | XNAME token 

instance Show a => Show (EClass a) where 
    show PUBLIC = "PUBLIC"
    show PRIVATE = "PRIVATE"
    show CONFIDENTIAL = "CONFIDENTIAL"
    show (IANA a) = show a
    show (XNAME a) = show a 

newtype UID = UID String deriving (Show)

-- concat $zipWith (++) (fmap show [eDTstamp, eUID ,eClass,eDTStart,eDescription,ePrio,eSeq,eTimeTrans,eRecur,eAlarm,eRRule])
-- concat $zipWith (++) (fmap show [eDTstamp, eUID ,eClass,eDTStart,eDescription,ePrio,eSeq,eTimeTrans,eRecur,eAlarm,eRRule])
-- ["DATETIME","UID:", "CLASS:", "DTSTART:", "DESCRIPTION", "PRIORITY:", "SEQUENCE:", "TRANSP:", "RECUR:", "ALARM:", "RRULE:"]--TODO


--TODO this is disgusting
instance Show Vevent where
     show (Vevent stamp uid eclass start end desc prio seq timet  alarm rrule) =
         "BEGIN=" ++ "VEVENT=" ++ "DATETIME= "++ show stamp ++ ";" ++  "UID=" ++ show uid ++ ";"  ++ show uid
         ++ "CLASS=" ++ show eclass ++ ";" ++ "DTSTART=" ++ show start ++ ";"++ "DTEND="
         ++ show end ++ ";"  ++ "DESCRIPTION=" ++ show desc ++ ";" ++ "PRIORITY=" ++
         show prio ++ ";" ++ "SEQUENCE=" ++ show seq ++ ";"++ "TRANSP=" ++ show timet++ ";" ++
         "RECUR=" ++ show rrule ++ ";" ++ "ALARM=" ++ show alarm ++ ";"

newtype MT a= MT {a :: Maybe a}

instance Show a => Show (MT a) where
    show (MT (Just a)) = show a
    show (MT Nothing) = show ""

data VrRule = VrRule
    {
    rFreq      :: Vrfreq       --P (Freq)
    ,rUntil    :: Until        --P (Day)
    ,rReoccur  :: Countr       --P (Integer) 
    ,rInterval :: Interval     --P (Integer)
    ,rbyMonth  :: ByMonth      --P (Integer)
    ,rbyDay    :: ByDay        --P (Integer)
    ,rbyMonthDay :: MonthDay   --P
    ,rByYearDay :: YearDay     --P (Integer)
    ,rByWeekNo :: WeekNo       --P (Integer)
    }

--TODO Generalise the show instances
-- data MShow (a :: Maybe b) where
--     ShowMaybe :: Show b => MShow a

newtype Until = Until (Maybe Day)

instance Show Until where
    show (Until (Just day)) = "UNTIL=" ++ filter (/= '-') (showGregorian day) ++ ";"
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
    show (ByDay (Just a)) = "BYDAY=" ++ show a ++ ";"
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
instance Show VrRule  where
    show (VrRule freq until (Countr Nothing) interval mon day _ _ _) =       --
     show freq ++ show until ++ show interval ++ show mon ++ show day
    show (VrRule freq (Until Nothing) reoccur interval mon day _ _ _) =
     show freq ++ show reoccur ++ show interval ++ show mon ++ show day
    show (VrRule freq (Until Nothing) (Countr Nothing) interval mon day _ _ _) =
     show freq  ++ show interval ++ show mon ++ show day
    show VrRule{..} = ""



--Cutting out some of the choices like "minutely" and "secondly"
data Vrfreq = HOURLY | DAILY | WEEKLY | MONTHLY | YEARLY
 deriving (Show, Eq, Ord)

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

--The "VTIMEZONE" calendar component MUST include the "TZID" 
--property and at least one definition of a "STANDARD" or "DAYLIGHT"
--sub-component.  The "STANDARD" or "DAYLIGHT" sub-component MUST
--include the "DTSTART", "TZOFFSETFROM", and "TZOFFSETTO"
--properties.
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

makeUID :: [Char]
makeUID = foldr1 (++) [filter dtCond (show $ DT $ unsafePerformIO getCurrentTime), uidID] --Documentation doesn't give any side-effects


