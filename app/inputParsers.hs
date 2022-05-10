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
getCountr = Countr <$> (string' "Countr " *> getNumType)

getYearDay :: MParser YearDay 
getYearDay = YearDay <$> (string' "YearDay " *> getCNumType 365)

getMonthDay :: MParser MonthDay 
getMonthDay = MonthDay <$> (string' "MonthDay " *> getCNumType 31)

getWeekNo :: MParser WeekNo 
getWeekNo = WeekNo <$> (string' "WeekNo " *> getCNumType 52)

----RRULES
--Some day every week
-- getWeeklyDate :: MParser VrRule
-- getWeeklyDate =
--getMonthlyDate ::

----VEVENT

getVevent :: MParser Vevent
getVevent =   intercalateEffect (char ' ') $ Vevent
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

getDateTime :: MParser UTCTime  
getDateTime = UTCTime <$> (choice [getISO, getDateMonth]) <*> (getTimeDay) 

------Pevent
getEventCat :: MParser EventCat
getEventCat = do choice 
 [ Ord <$ string' "ordered"
 , Dead <$ string' "deadline"
 , Prio <$ string' "priority"
 , Todo <$ string' "Todo"
 ]

-- getUTCPair :: MParser (UTCTime, UTCTime)
-- getUTCPair = (,) <$> (string' "start: " *> getDateTime ) <*>(string' "end: " *> getDateTime)

-- getPeventNew :: MParser ParseEvent 
-- getPeventNew = ParseEvent <$> NoEvent 
--                <*> L.decimal 
--                <*> getUTCPair
--                <*> (fmap .diffTimeToPicoseconds getTimeDay) 
 
-- getPeventEdit :: MParser ParseEvent
-- getPeventEdit = undefined

-- getScheduled :: MParser ParseEvent
-- getScheduled = 

-- getDeadline :: MParser ParseEvent 

-- getPrio ::MParser ParseEvent 

-- getTodo :: MParser ParseEvent

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
             setInput a       --cheating function, lets you combine parsers
             b <- L.decimal
             return (if b > 100 then b else (b+2000))

      
-- getICSFile ::  MParser VCalendar
-- getICSFile = 

