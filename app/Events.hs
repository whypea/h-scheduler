{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts   #-}

module Events where
import Data.Void
import Control.Applicative
import Text.Megaparsec ( Parsec )
import  Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Time
import Data.Text ( Text, pack, intersperse )
import GHC.IO (unsafePerformIO)
import Class (Class)
import Data.Data
import Control.Monad.Trans.State (StateT)
import GHC.Conc (pseq)

type Datetime = Day

type UID = Text
--Types for iCal
data Vevent = Vevent
    {
    eDTstamp :: Datetime
    ,eUID     :: UID
    ,eClass   :: Text
    ,eDTStart :: Datetime
    ,eDescription :: MT Text
    ,ePrio :: MT Text
    ,eSeq :: MT Text
    ,eTimeTrans :: MT Text
    ,eRecur :: MT Text
    ,eAlarm :: MT Text
    ,eRRule :: MT Text
    }
-- concat $zipWith (++) (fmap show [eDTstamp, eUID ,eClass,eDTStart,eDescription,ePrio,eSeq,eTimeTrans,eRecur,eAlarm,eRRule])
-- concat $zipWith (++) (fmap show [eDTstamp, eUID ,eClass,eDTStart,eDescription,ePrio,eSeq,eTimeTrans,eRecur,eAlarm,eRRule])
-- ["DATETIME","UID:", "CLASS:", "DTSTART:", "DESCRIPTION", "PRIORITY:", "SEQUENCE:", "TRANSP:", "RECUR:", "ALARM:", "RRULE:"]--TODO

instance Show Vevent where
     show (Vevent stamp uid eclass start desc prio seq timet recur alarm rrule) =
         "BEGIN: VEVENT" ++ "DATETIME: "++ show stamp ++ ";" ++  "UID:" ++ ";"  ++ show uid 
         ++ "CLASS:" ++ show eclass ++ ";" ++ "DTSTART:" 
         ++ show start ++ ";" ++ "DESCRIPTION:" ++ show desc ++ ";" ++ "PRIORITY:" ++ 
         show prio ++ ";" ++ "SEQUENCE:" ++ show seq ++ ";"++ "TRANSP:" ++ show timet++ ";" ++ 
         "RECUR:" ++ show recur ++ ";" ++ "ALARM:" ++ show alarm ++ ";" ++ "RRULE:" ++ show rrule 

newtype MT a= MT {a :: Maybe a}

instance Show a => Show (MT a) where
    show (MT (Just a)) = show a
    show (MT Nothing) = show ""

data VrRule = VrRule
    {
    rFreq      :: Vrfreq
    ,rUntil    :: Day
    ,rReoccur  :: Reoccur
    ,rInterval :: Interval
    ,rbyMonth  :: ByMonth
    ,rbyDay    :: ByDay
    }

newtype Reoccur = Reoccur (Maybe Integer)

instance Show Reoccur where
    show (Reoccur (Just a)) = "COUNT:" ++ show a
    show (Reoccur Nothing) = ""

newtype Interval = Interval (Maybe String)

instance Show Interval where
    show (Interval (Just a)) = "INTERVAL:" ++ show a
    show (Interval Nothing) = ""

newtype ByMonth = ByMonth (Maybe Integer)

instance Show ByMonth where
    show (ByMonth (Just a)) = "BYMONTH:" ++ show a
    show (ByMonth Nothing) = ""


newtype ByDay = ByDay (Maybe Integer)

instance Show ByDay where
    show (ByDay (Just a)) = "BYDAY:" ++ show a
    show (ByDay Nothing) = ""

-- instance Show VrRule where
--     show (VrRule rFreq rUntil rReoccur rInterval rbyMonth rbyDay) = 

data VRule = R Vrfreq (MT Day) (MT Integer) (MT Integer) (MT Integer) (MT Integer)

-- The UNTIL or COUNT rule parts are OPTIONAL, but they MUST NOT occur in the same 'recur'.
--replace with something like intersperse 
instance Show VRule where
  show (R freq reocc (MT Nothing) int mon day) =
      "RRULE:" ++ "FREQ:" ++ show freq ++ ";" ++ "UNTIL:" ++ show reocc  ++ ";"
       ++  ";" ++ "INTERVAL:" ++ show int ++ ";" ++"BYMONTH:"
      ++ show mon ++ ";" ++ "BYDAY:" ++ show day
  show (R freq d reocc int mon day) = 
      "RRULE:" ++ "FREQ:" ++ show freq ++ ";" ++ "UNTIL:" ++ show reocc  ++ ";"
      ++ "INTERVAL:" ++ show int ++ ";" ++"BYMONTH:"
      ++ show mon ++ ";" ++ "BYDAY:" ++ show day
--Cutting out some of the choices
data Vrfreq = HOURLY | DAILY | WEEKLY | MONTHLY | YEARLY
 deriving (Show, Eq, Ord)

data Vcalendar = Vcalendar
    {
    cProdId       :: Text
    , cVersion    :: Text
    , cScale      :: Text
    , cTimeZones  :: Text
    , cEvents     :: [Vevent]
    }

instance Show Vcalendar where
    show (Vcalendar cProdId version scale tz events) = "BEGIN:" ++ "VERSION: " ++ show version ++ "SCALE: "
type TParser = Parsec Void String
type ShParser = Parsec Void Text
type MyStack a = StateT Day TParser a

--printers
pBegin :: IO Text
pBegin = pure "BEGIN:"

pEND :: IO Text
pEND = pure "END:"

pSemiC :: Text -> Text
pSemiC = intersperse ';'







