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
import Data.Data
import Control.Monad.Trans.State (StateT)



newtype Datetime = DT UTCTime

instance Show Datetime where
    show (DT a) = filter (/= '-' ) (iso8601Show a) ++ ";"  



--Types for iCal
data Vevent = Vevent
    {
    eDTstamp :: Datetime --P
    ,eUID     :: UID
    ,eClass   :: Text
    ,eDTStart :: UTCTime --P
    ,eDTEnd   :: UTCTime --P
    ,eDescription :: String --P? ()
    ,ePrio :: MT Text
    ,eSeq :: MT Text
    ,eTimeTrans :: Transp
    ,eRecur :: MT Text
    ,eAlarm :: MT Text
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


--this is disgusting
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
    rFreq      :: Vrfreq --P (Freq)
    ,rUntil    :: Until --P (Day)
    ,rReoccur  :: Reoccur
    ,rInterval :: Interval
    ,rbyMonth  :: ByMonth --P
    ,rbyDay    :: ByDay   --P
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
    cProdId       :: Text   --needs a generator 
    , cVersion    :: Version
    , cScale      :: Gregorian
    , cTimeZones  :: TZ 
    , cEvents     :: [Vevent]
    }

data Gregorian = GREGORIAN deriving (Show, Read, Enum)

newtype Version = Version String

instance Show Version where
    show (Version a) = "2.0" --DWAI

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
     "BEGIN:\n " ++ "VERSION="
     ++ show version ++ "\n" ++ "SCALE=" ++ show scale ++ "TIMEZONE="++ show tz ++ "EVENTS="++ show events


--printers



makeUDI ::  UTCTime
makeUDI = unsafePerformIO getCurrentTime --Documentation doesn't give any side-effects





