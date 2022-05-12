{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE BlockArguments      #-}
module InputParsers where
import System.IO.Unsafe
import Events
import Solver
-- import Control.Applicative
import  Text.Megaparsec
import  Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char
import Data.Void
import Data.Time
import Data.Time.Calendar.MonthDay
import qualified Data.Text as T

import Control.Lens
import Control.Monad.Trans.State 
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative.Permutations 

type MParser = Parsec Void String 

---- !Helpers
emptySingle :: (MonadParsec e s m, Token s ~ Char) => Token s -> m () --from source of space 
emptySingle x = void $ single x

makeUID :: [Char] 
makeUID = foldr1 (++) [filter dtCond (show $ DT $ unsafePerformIO getCurrentTime), uidID] --Documentation doesn't give any side-effects

date :: IO Day -- :: (year,month,day)
date = getCurrentTime >>=  return . utctDay

--TODO.. (See if this can't be made safe?)
unsafeCurrentTime :: UTCTime
unsafeCurrentTime = unsafePerformIO getCurrentTime 

gregYear :: UTCTime -> Integer 
gregYear x = (toGregorian $ utctDay x)^._1

gregMonth :: UTCTime -> Int 
gregMonth x = (toGregorian $ utctDay x)^._2

gregDay :: UTCTime -> Int 
gregDay x = (toGregorian $ utctDay x)^._3

makeUTCTime :: Day -> DiffTime -> UTCTime
makeUTCTime day time = UTCTime (day) (time)

getNumType :: Num a => MParser (Maybe a)
getNumType = Just <$> L.decimal 

getCNumType :: (Num a, Ord a) => a -> MParser (Maybe a)
getCNumType c = Just <$> (fmap (clip c) L.decimal)

clip ::(Num a,Ord a) => a -> a -> a 
clip c x = if x > c then c else x

nextWeekday :: DayOfWeek -> Day -> Day -- -> UTCTime -> UTCTime 
nextWeekday wd now = addDays x now
 where x = if diff < 0 then (-toInteger diff) else 7 -(toInteger diff)
       diff = (fromEnum $ dayOfWeek now) - (fromEnum wd)

getStr :: MParser (Maybe String)
getStr = Just <$> (some alphaNumChar <* eof)

getDateTime :: MParser UTCTime  
getDateTime = UTCTime <$> (choice [getISO, getDateMonth]) <*> (getTimeDay) 

--TODO Fix this
-- getStringType :: String -> MParser (Maybe a)
-- getStringType str = do a <- string' str
--                        return (Just a)

---- !Parsers
--DONE?: Put together the other parsers in a permutation, handles failures by returning "Nothing" 
getrRule :: MParser VrRule
getrRule =   intercalateEffect (char ' ') $ VrRule
             <$> toPermutation (getrFreq)
             <*> toPermutationWithDefault (Until Nothing) getUntil
             <*> toPermutationWithDefault (Countr Nothing) getCountr
             <*> toPermutationWithDefault (Interval Nothing) getInterval
             <*> toPermutationWithDefault (ByMonth Nothing) getByMonth
             <*> toPermutationWithDefault (ByDay Nothing) getByDay
             <*> toPermutationWithDefault (MonthDay Nothing) getMonthDay
             <*> toPermutationWithDefault (YearDay Nothing) getYearDay
             <*> toPermutationWithDefault (WeekNo Nothing) getWeekNo 

getFilerRule :: MParser VrRule
getFilerRule =   intercalateEffect (char ' ') $ VrRule
             <$> toPermutation (getrFreq)
             <*> toPermutationWithDefault (Until Nothing) getUntilE
             <*> toPermutationWithDefault (Countr Nothing) getCountrE
             <*> toPermutationWithDefault (Interval Nothing) getIntervalE
             <*> toPermutationWithDefault (ByMonth Nothing) getByMonthE
             <*> toPermutationWithDefault (ByDay Nothing) getByDayE
             <*> toPermutationWithDefault (MonthDay Nothing) getMonthDayE
             <*> toPermutationWithDefault (YearDay Nothing) getYearDayE
             <*> toPermutationWithDefault (WeekNo Nothing) getWeekNoE

getrFreq :: MParser Vrfreq --List of tuples
getrFreq = do choice 
 [ HOURLY  <$ string' "hourly"
 , HOURLY  <$ string' "every hour" 
 , DAILY   <$ string' "daily"        
 , DAILY   <$ string' "every day"        
 , WEEKLY  <$ string' "weekly"       
 , WEEKLY  <$ string' "every week"       
 , MONTHLY <$ string' "monthly"      
 , MONTHLY <$ string' "every month"      
 , YEARLY  <$ string' "yearly"       
 , YEARLY  <$ string' "every year"] 

getByMonth :: MParser ByMonth
getByMonth = ByMonth <$> (string' "ByMonth " *> fmap Just getMonth)

getByDay :: MParser ByDay
getByDay = ByDay <$> (string' "ByDay " *> fmap Just getDay)

getInterval :: MParser Interval 
getInterval = Interval <$> (string' "Interval " *> getNumType)

getUntil :: MParser Until 
getUntil = Until <$> (string' "Until " *> fmap Just getDateTime)

getCountr :: MParser Countr 
getCountr = Countr <$> ( getNumType <* string' " times")

getYearDay :: MParser YearDay 
getYearDay = YearDay <$> (string' "YearDay " *> getCNumType 365)

getMonthDay :: MParser MonthDay 
getMonthDay = MonthDay <$> (string' "MonthDay " *> getCNumType 31)

getWeekNo :: MParser WeekNo 
getWeekNo = WeekNo <$> (string' "WeekNo " *> getCNumType 52)

getByMonthE :: MParser ByMonth
getByMonthE = ByMonth <$> (string' "BYMONTH:" *> fmap Just getMonth)

getByDayE :: MParser ByDay
getByDayE = ByDay <$> (string' "BYDAY:" *> fmap Just getDay)

getIntervalE :: MParser Interval 
getIntervalE = Interval <$> (string' "INTERVAL:" *> getNumType)

getUntilE :: MParser Until 
getUntilE = Until <$> (string' "UNTIL:" *> fmap Just getDateTime)

getCountrE :: MParser Countr 
getCountrE = Countr <$> (string' "COUNT:" *> getNumType )

getYearDayE :: MParser YearDay 
getYearDayE = YearDay <$> (string' "YEARDAY:" *> getCNumType 365)

getMonthDayE :: MParser MonthDay 
getMonthDayE = MonthDay <$> (string' "MONTHDAY:" *> getCNumType 31)

getWeekNoE :: MParser WeekNo 
getWeekNoE = WeekNo <$> (string' "WEEKNO:" *> getCNumType 52)


----RRULES
--Some day every week
-- getWeeklyDate :: MParser VrRule
-- getWeeklyDate =
--getMonthlyDate ::

----VEVENT

getVevent :: MParser Vevent
getVevent = intercalateEffect (char ' ') $ Vevent
             <$> toPermutation getDTStamp
             <*> toPermutation getUID
             <*> toPermutationWithDefault (PRIVATE) getClass
             <*> toPermutation getDateStart
             <*> toPermutationWithDefault (DateStop Nothing) getDateStop
             <*> toPermutationWithDefault (Duration Nothing) getDuration
             <*> toPermutationWithDefault (Desc Nothing) getDesc
             <*> toPermutationWithDefault (Priority Nothing) getPrio
             <*> toPermutationWithDefault (EvtSequence Nothing) getEvtSeq 
             <*> toPermutationWithDefault (Nothing) (Just <$> getTransp)
             <*> toPermutationWithDefault (Nothing) (Just <$>getrRule)

--TODO
getFileVevent :: MParser Vevent
getFileVevent =  intercalateEffect (char ' ') $ Vevent
                 <$> toPermutation getDTStampE
                 <*> toPermutation getUIDE
                 <*> toPermutationWithDefault (PRIVATE) getClassE
                 <*> toPermutation getDateStartE
                 <*> toPermutationWithDefault (DateStop Nothing) getDateStopE
                 <*> toPermutationWithDefault (Duration Nothing) getDurationE
                 <*> toPermutationWithDefault (Desc Nothing) getDescE
                 <*> toPermutationWithDefault (Priority Nothing) getPrioE
                 <*> toPermutationWithDefault (EvtSequence Nothing) getEvtSeqE 
                 <*> toPermutationWithDefault (Nothing) (Just <$> getTranspE)
                 <*> toPermutationWithDefault (Nothing) (Just <$>getrRuleE)

getDTStamp :: MParser Datetime
getDTStamp = DT <$> getDateTime

getUID :: MParser UID
getUID = pure (UID (makeUID :: String))

getClass :: MParser EClass
getClass = choice 
 [ PUBLIC <$ string' "public"
 , PRIVATE <$ string' "private"
 , CONFIDENTIAL <$ string' "Confidential"
 --, IANA <$ string' "public"
 --, XNAME <$ string' "public"  
 ]

getDateStart :: MParser DateStart
getDateStart =  DateStart <$> (string' "Start" *> getDateTime)

getDateStop :: MParser DateStop
getDateStop =   DateStop <$> (string' "Stop" *> (Just <$> getDateTime))

getDesc :: MParser Desc
getDesc = Desc <$> (string' "Desc" *>  getStr)

getDuration :: MParser Duration
getDuration = Duration <$> (string' "Duration" *> (Just <$> getTimeDay))

getPrio :: MParser Priority
getPrio = Priority <$> (string' "Priority " *> getNumType)

getEvtSeq :: MParser EvtSequence
getEvtSeq = EvtSequence <$> getNumType

getTransp :: MParser Transp
getTransp = choice 
 [TRANSPARENT <$ string' "Transparent"
 , OPAQUE <$ string' "Opaque"
 ]


getDTStampE :: MParser Datetime
getDTStampE = DT <$> getDateTime

getUIDE :: MParser UID
getUIDE = pure (UID (makeUID :: String))

getClassE :: MParser EClass
getClassE = choice 
 [ PUBLIC <$ string' "public"
 , PRIVATE <$ string' "private"
 , CONFIDENTIAL <$ string' "Confidential"
 --, IANA <$ string' "public"
 --, XNAME <$ string' "public"  
 ]

getDateStartE :: MParser DateStart
getDateStartE =  DateStart <$> (string' "DTSTART:" *> getDateTime <* "\n")

getDateStopE :: MParser DateStop
getDateStopE =   DateStop <$> (string' "DTSTOP:" *> (Just <$> getDateTime) <* "\n")

getDescE :: MParser Desc
getDescE = Desc <$> (string' "DESCRIPTION:" *>  getStr <* "\n")

getDurationE :: MParser Duration
getDurationE = Duration <$> (string' "DURATION:" *> (Just <$> getTimeDay) <* "\n")

getPrioE :: MParser Priority
getPrioE = Priority <$> (string' "PRIORITY:" *> getNumType <* "\n")

getEvtSeqE :: MParser EvtSequence
getEvtSeqE = EvtSequence <$> getNumType

getTranspE :: MParser Transp
getTranspE = choice 
 [TRANSPARENT <$ string' "Transparent"
 , OPAQUE <$ string' "Opaque"
 ]
getrRuleE :: MParser VrRule
getrRuleE = string "RRULE:" *> getrRule 

------Pevent
--How to get from interpreter

getUTCPair :: MParser (UTCTime, UTCTime)
getUTCPair = (,) <$> (string' "start: " *> getDateTime ) <*> (string' "end: " *> getDateTime)

-- getPeventNew :: MParser ParseEvent 
-- getPeventNew = ParseEvent <$> NoEvent 
--                <*> L.decimal 
--                <*> getUTCPair
--                <*> (fmap .diffTimeToPicoseconds getTimeDay) 
 
-- getPeventEdit :: MParser ParseEvent
-- getPeventEdit = undefined

--Grammar: 
getOrdered :: MParser Ordered
getOrdered = Ordered <$> pure NoEvent 
            <*> string' "Priority:" *> L.decimal 
            <*> getUTCPair 
            <*> getTimeDay

-- getDeadline :: MParser Deadline
-- getDeadline = Deadline dEvent <$> pure NoEvent 
--             <*> string' "Priority:" *> L.decimal 
--             <*> getUTCPair 
--             <*> getTimeDay

-- getDeadlineUTC :: MParser (UTCTime, UTCTime)
-- getDeadlineUTC = string' "Deadline:" *> (,) <$> utczero <*> getDateTime 

-- getPrioritized :: MParser Prioritized 
-- getPrioritized = Prioritized <$> ParseEvent <$> pure NoEvent 
--             <*> string' "Priority:" *> L.decimal 
--             <*> getUTCPair 
--             <*> getTimeDay

-- getTodo :: MParser Todo
-- getTodo = Todo <$> ParseEvent <$> pure NoEvent 
--             <*> string' "Priority:" *> L.decimal 
--             <*> getUTCPair 
--             <*> getTimeDay


getDay :: MParser DayOfWeek
getDay = do choice 
 [ Monday    <$ string' "mon" <* many alphaNumChar
 , Tuesday   <$ string' "tue"<* many alphaNumChar 
 , Wednesday <$ string' "wed" <* many alphaNumChar 
 , Thursday  <$ string' "thu" <* many alphaNumChar 
 , Friday    <$ string' "fri"<* many alphaNumChar 
 , Saturday  <$ string' "sat" <* many alphaNumChar 
 , Sunday    <$ string' "sun" <* many alphaNumChar ]

getMonth :: MParser Int
getMonth = do choice 
 [ 1   <$ string' "Jan" <* many alphaNumChar
 , 2   <$ string' "Feb"<* many alphaNumChar 
 , 3   <$ string' "Mar" <* many alphaNumChar 
 , 4   <$ string' "Apr" <* many alphaNumChar 
 , 5   <$ string' "May"<* many alphaNumChar 
 , 6   <$ string' "Jun" <* many alphaNumChar 
 , 7   <$ string' "Jul" <* many alphaNumChar 
 , 8   <$ string' "Aug" <* many alphaNumChar 
 , 9   <$ string' "Sep" <* many alphaNumChar 
 , 10  <$ string' "Oct" <* many alphaNumChar 
 , 11  <$ string' "Nov" <* many alphaNumChar 
 , 12  <$ string' "Dec" <* many alphaNumChar ]

      

-- rfreqmap :: String -> MParser Vrfreq
-- rfreqmap s = [fmap [HOURLY, DAILY, WEEKLY, MONTHLY, YEARLY]]   

getTimeDay :: MParser DiffTime 
getTimeDay = do x <- getHour
                string' ":"
                y <- getMinute
                return(x+y)

getHour :: MParser DiffTime
getHour = ((secondsToDiffTime 3600*) <$> L.decimal)

getMinute :: MParser DiffTime
getMinute = ((secondsToDiffTime 60*) <$> L.decimal) 

getISO :: MParser Day
getISO = do year <- getYear
            try $ emptySingle '-' <|> space1
            month <- L.decimal
            try $ emptySingle '-' <|> space1
            day <- L.decimal
            return (fromGregorian year month day) --Will clip the date  

getDateMonth :: MParser Day
getDateMonth = do month <- L.decimal
                  try $ emptySingle '-' <|> space1
                  day <- L.decimal
                  return (fromGregorian (gregYear unsafeCurrentTime) month day) 

getnDay :: MParser Day
getnDay = do day <- string' "next " *> getDay
             return (nextWeekday day  $ unsafePerformIO date)

-- getnMonth :: MParser Day 
-- getnMonth = do month <- string' "next " *> getMonth  
--                return (fromGregorian (gregYear unsafeCurrentTime) (month+1) (gregDay unsafeCurrentTime))

getnWeek :: MParser Day
getnWeek = do month <- string' "next week"  
              return (addDays 7 $ fromGregorian (gregYear uct) (gregMonth uct) (gregDay uct))
      where uct = unsafeCurrentTime

getYearHelper :: MParser String
getYearHelper = takeP (Just "four") 4 <|> takeP (Just "two") 2  <* eof

getYear :: MParser Integer --(>=>) might be useful
getYear = do a <- getYearHelper
             setInput a       --cheating function, lets you combine parsers. Might have weird effects though      
             b <- L.decimal
             return (if b > 100 then b else (b+2000))

      
-- getICSFile ::  MParser VCalendar
-- getICSFile = 

