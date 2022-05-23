{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Events where

import System.Random 
import System.IO.Unsafe 
import System.IO
import Control.Applicative ()
import Text.Megaparsec (ParseErrorBundle)
import Text.Megaparsec.Char 
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Time
import Data.Void 
import Control.Lens
import Data.Time.Format.ISO8601 
import qualified Data.Text as T 
import GHC.IO (unsafePerformIO)
import Data.Data 
import Control.Lens
import Control.Monad.Trans.State 
import Data.Time.Calendar.Julian (DayOfYear)
import Data.Time.Calendar.OrdinalDate (WeekOfYear)
import TextShow
import Options.Applicative as O 

dtCond :: Char -> Bool
dtCond = \x -> x /= '-' && x /= ':'

data ParseEvent = ParseEvent
    {desc :: String               --Event
    ,prio  :: Int               --Priority
    ,pSET  :: (UTCTime,UTCTime) --Start/end time
    ,dur   :: DiffTime          --estimated duration, relevant
    } 
data WithRule a = WithRule {event :: a, rule :: Maybe VrRule}

instance (Show a) => Show (WithRule a) where
    show (WithRule a (Just rule)) = show a ++ show rule
    show (WithRule a Nothing) = show a

wrnoRule a= WithRule a Nothing

newtype Scheduled = Scheduled {sEvent :: ParseEvent}         --
newtype Ordered = Ordered {oEvent :: ParseEvent}             --Set date, eg. meetings   
newtype Deadline = Deadline {dEvent :: ParseEvent}           --Certain date to finish by, 
newtype Prioritized = Prioritized {pEvent :: ParseEvent}     --timed tasks with a priority, flexible
newtype Todo  = Todo {tEvent :: ParseEvent}                  --Any time, priority has lower pecedence than above

instance Show ParseEvent where 
    show (ParseEvent event prio pSet dur) =show event ++ show prio ++ "-" ++ show (pSet^._1) ++ "-" ++ show (pSet^._2) ++ "-" ++ show dur -- ++ show (pget $ rule)

instance Show Scheduled where
    show (Scheduled s) = show s

instance Show Ordered where
    show (Ordered s) = show s

instance Show Deadline where
    show (Deadline s) = show s

instance Show Prioritized where
    show (Prioritized s) = show s

instance Show Todo where
    show (Todo s) = show s

data Opts = Opts {
     filename      :: String
    ,uidsuffix     :: String
    -- ,calendarStart :: String
    ,wake          :: String
    ,bed           :: String
    --,command  :: ActualCommands 
} 
    deriving Show 
defaultopts = Opts "MyCalendar" "@h-scheduler" "07:00" "22:00"

data InsideCommands = OrderedList  (Either (ParseErrorBundle String Void) Ordered)              --(O.ReadM Ordered)
                    | DeadlineList (Either (ParseErrorBundle String Void) Deadline)             
                    | PrioritizedList (Either (ParseErrorBundle String Void) Prioritized)       --(Either (ParseErrorBundle String Void) Deadline)    
                    | TodoList     (Either (ParseErrorBundle String Void) Todo)                 --(Maybe Todo) 
--TODO
data ActualCommands = EditFile 
                    | MakeFile  

data ILists         = ILists {ordlist     :: [(Either (ParseErrorBundle String Void) Ordered)] 
                             ,dllist      :: [(Either (ParseErrorBundle String Void) Deadline)] 
                             ,priolist    :: [(Either (ParseErrorBundle String Void) Prioritized)] 
                             ,tdlist      :: [(Either (ParseErrorBundle String Void) Todo)]  }

data ACO = ACO ActualCommands Opts 

--Types for iCal
data Vevent = NoEvent | Vevent
    {
     eDTstamp :: Datetime       --P
    ,eUID     :: UID            --P (takes Datetime)
    ,eClass   :: EClass         --P
    ,eDTStart :: DateStart      --P
    ,eDTEnd   :: DateStop       --P
    ,eDuration :: Duration      --P
    ,eDescription :: Desc       --P? ()
    ,eSummary :: Summary
    ,ePrio :: Priority          --P
    ,eSeq :: EvtSequence        --P
    ,eTimeTrans :: Maybe Transp --P 
    ,eRRule :: Maybe VrRule     --P
    } 

newtype Datetime = DT UTCTime

instance Show Datetime where
    show (DT a) = "DATETIME:" ++ formatTime defaultTimeLocale "%Y%m%dT%H%M%S" a

instance TextShow Datetime where 
    showb a =  fromString (show a)

newtype UID = UID String deriving (Show)

instance TextShow UID where
    showb a = fromString (show a)  

data Transp = TRANSPARENT | OPAQUE

instance Show Transp where
    show TRANSPARENT = "TRANSP:TRANSPARENT"
    show OPAQUE = "TRANSP:OPAQUE" 

instance TextShow Transp where
    showb a = showb (show a)

data DateStart = DateStart UTCTime 

--TODO make the correct formatting with formatTime
instance Show (DateStart) where
    show (DateStart a) = "DTSTART:" ++ formatTime defaultTimeLocale "%Y%m%dT%H%M%S" a

instance TextShow DateStart where 
    showb a = fromString (show a) 

data DateStop = DateStop (Maybe UTCTime) 

instance Show (DateStop) where
    show (DateStop (Just a)) = "DTSTOP:" ++ formatTime defaultTimeLocale "%Y%m%dT%H%M%S" a
    show (DateStop (Nothing)) = ""

instance TextShow DateStop where
    showb a = fromString (show a)  

data Desc = Desc (Maybe String)

instance Show (Desc) where
    show (Desc (Just a)) = "DESCRIPTION:" ++ show a
    show (Desc (Nothing))  = ""

instance TextShow Desc where
    showb a = fromString (show a)  

data Summary = Summary (Maybe String)

instance Show (Summary) where
    show (Summary (Just a)) = "SUMMARY:" ++ show a
    show (Summary (Nothing))  = ""

instance TextShow Summary where
    showb a = fromString (show a)  

data EClass = PUBLIC | PRIVATE | CONFIDENTIAL | IANA String | XNAME String 

instance Show (EClass) where 
    show PUBLIC = "CLASS:PUBLIC"
    show PRIVATE = "CLASS:PRIVATE"
    show CONFIDENTIAL = "CLASS:CONFIDENTIAL"
    show (IANA a) = "IANA" ++ show a
    show (XNAME a) = "XNAME" ++ show a 

instance TextShow EClass where
    showb a = fromString (show a)  

data Priority = Priority (Maybe Int) 

instance Show Priority where 
    show (Priority (Just a)) = "PRIORITY:" ++ show a
    show (Priority Nothing) = "" 

instance TextShow Priority where
    showb a = fromString (show a)  

data EvtSequence = EvtSequence (Maybe Int)

instance Show EvtSequence where 
    show (EvtSequence (Just a)) = "SEQUENCE:" ++ show a
    show (EvtSequence (Nothing)) = ""

instance TextShow EvtSequence where
    showb a = fromString (show a)  

data Duration = Duration (Maybe DiffTime)
    
instance Show Duration where
    show (Duration (Just a)) = "DURATION:" ++ show a
    show (Duration (Nothing)) = ""

instance TextShow Duration where
    showb a = fromString (show a)  

-- concat $ zipWith (++) (fmap show [eDTstamp, eUID ,eClass,eDTStart,eDescription,ePrio,eSeq,eTimeTrans,eRecur,eAlarm,eRRule])
-- ["DATETIME","UID:", "CLASS:", "DTSTART:", "DESCRIPTION", "PRIORITY:", "SEQUENCE:", "TRANSP:", "RECUR:", "ALARM:", "RRULE:"]--TODO

instance Show Vevent where
    show NoEvent = "No Event"
    show (Vevent stamp uid eclass start (DateStop Nothing) duration desc sum prio seq timet rrule) =
     unlines ["BEGIN: VEVENT" ,show stamp, show uid, 
         show eclass,show start, show duration,show desc ,show prio,show seq
        ,show timet,"RECUR:",show rrule,"END:VEVENT"]       
    show (Vevent stamp uid eclass start stop (Duration Nothing) desc sum prio seq timet rrule) =
     unlines ["BEGIN: VEVENT",show stamp, "UID:",show uid,show eclass,";",show start
        ,show stop ,show desc,show prio ,show seq,show timet ,"RECUR:",show rrule,"END:VEVENT"]
         
        

instance TextShow Vevent where
    showb (Vevent stamp uid eclass start (DateStop Nothing) duration desc sum prio seq timet rrule) = showb stamp <> showb uid <> showb eclass<> showb start 
                                                                                                 <> showb (DateStop Nothing) <> showb duration <> showb desc 
                                                                                                 <> showb prio <> showb seq  <> showb timet <> showb (fromString $ show rrule) 
    showb (Vevent stamp uid eclass start stop (Duration Nothing) desc sum prio seq timet rrule) = showb stamp <> showb uid <> showb eclass<> showb start 
                                                                                                 <> showb (DateStop Nothing) <> showb (Duration Nothing) <> showb desc 
                                                                                                 <> showb prio <> showb seq  <> showb timet <> showb (fromString $ show rrule)                                                                                            

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



defvr :: VrRule
defvr = VrRule (DAILY) (Until Nothing) (Countr Nothing) (Interval Nothing) (ByMonth Nothing) (ByDay Nothing) (MonthDay Nothing) (YearDay Nothing) (WeekNo Nothing)
 

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
     unwords [show freq, show until, show interval, show mon, show day, show md, show yd, show wn] 
    show (VrRule freq (Until Nothing) reoccur interval mon day md yd wn) =
     unwords [show freq, show reoccur, show interval, show mon, show day, show md, show yd, show wn]
    -- show (VrRule freq (Until Nothing) (Countr Nothing) interval mon day _ _ _) =
    --  show freq  ++ show interval ++ show mon ++ show day
    show VrRule{..} = ""

data Vcalendar = Vcalendar
    {
    cProdId       :: ProdID String  
    , cVersion    :: Version
    , cScale      :: Gregorian
    , cTimeZones  :: TZ 
    , cEvents     :: [Vevent]
    }

data ProdID a = Prod a

-- instance Show a => Show (ProdID a) where
--     show (Prod a) = show a ++ "//a haskell scheduler//EN"

instance Show (ProdID a) where
    show (Prod a) = "INF329 //a haskell scheduler//EN"

data Gregorian = GREGORIAN deriving (Show, Read, Enum)

newtype Version = Version String

instance Show Version where
    show (Version a) = "2.0" --Change if the standards update 

newtype TZ = TZ TimeZone --offset in minutes s

instance Show TZ where
    show (TZ a) = show (timeZoneMinutes a) 

-- 
instance Show Vcalendar where
    show (Vcalendar prodid version scale tz events) =
     unlines ["BEGIN:VCALENDAR",show prodid,"VERSION:","SCALE=",show scale 
              ,"TIMEZONE=" ,show tz, show version,printList events,"END:VCALENDAR"]

printList :: Show a => [a] -> String
printList [] = ""
printList (x:xs) = show x ++ "\n" ++ printList xs

--printers

--TODO Change this to an option passed down from CLI (Reader get)
uidID ::UTCTime -> Opts -> String
uidID a opts = formatTime defaultTimeLocale "%Y%m%dT%H%M%S" a ++ show (evalState (uidNumber) $ mkStdGen seed )  ++ uidsuffix opts
    where seed = ceiling $ toRational $ utctDayTime a

uidNumber :: State StdGen Int
uidNumber = state $ uniformR (10000 :: Int, 99999 :: Int)
