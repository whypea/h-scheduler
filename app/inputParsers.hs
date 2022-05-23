{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE BlockArguments      #-}
module InputParsers where
import System.IO.Unsafe
import Events
import Common

import  Text.Megaparsec
import  Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char
import Data.Void
import Data.Time
import Data.Time.Calendar.MonthDay
import qualified Data.Text as T
import Data.Maybe
import Data.List

import Control.Lens
import Control.Monad.Trans.State 
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative.Permutations 

emptySingle :: (MonadParsec e s m, Token s ~ Char) => Token s -> m () --from source of space 
emptySingle x = void $ single x

date :: IO Day -- :: (year,month,day)
date = getCurrentTime >>=  return . utctDay

type MParser a = Parsec Void String a

wakeO :: Opts -> DiffTime
wakeO opts = fromMaybe 25200 $ parseMaybe getTimeDay (wake opts) 

bedO :: Opts ->  DiffTime
bedO opts = fromMaybe 75600 $ parseMaybe getTimeDay (bed opts)

----------------------
unsafeCurrentTime :: UTCTime
unsafeCurrentTime = unsafePerformIO getCurrentTime 
----------------------
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

getNumType' :: MParser Int
getNumType' = L.decimal 

getCNumType :: (Num a, Ord a) => a -> MParser (Maybe a)
getCNumType c = Just <$> (fmap (clip c) L.decimal)

clip ::(Num a,Ord a) => a -> a -> a 
clip c x = if x > c then c else x

nextWeekday :: DayOfWeek -> Day -> Day -- -> UTCTime -> UTCTime 
nextWeekday wd now = addDays x now
 where x = if diff < 0 then (-toInteger diff) else 7 -(toInteger diff)
       diff = (fromEnum $ dayOfWeek now) - (fromEnum wd)

getStr :: MParser (Maybe String)
getStr = Just <$> (some alphaNumChar <* choice [space, eof])

getStr' :: MParser String
getStr' = (some alphaNumChar <* choice [space, eof])

getDateTime :: MParser UTCTime  
getDateTime = UTCTime <$> (choice [getISO, getDateMonth, getnDay, getnWeek]) <*> (space1 *> getTimeDay) 

getTimeDay :: MParser DiffTime 
getTimeDay = do x <- getHour
                string' ":"
                y <- getMinute
                return(x+y)

getHour :: MParser DiffTime
getHour = ((secondsToDiffTime 3600*) <$> L.decimal)

getMinute :: MParser DiffTime
getMinute = ((secondsToDiffTime 60*) <$> L.decimal) 

getDateMonth :: MParser Day
getDateMonth = do month <- L.decimal
                  try $ emptySingle '-' <|> space1
                  day <- L.decimal
                  return (fromGregorian (gregYear unsafeCurrentTime) month day) 

getISO :: MParser Day
getISO = do year <- getYear'
            try $ emptySingle '-' <|> space1
            month <- L.decimal
            try $ emptySingle '-' <|> space1
            day <- L.decimal
            return (fromGregorian year month day) --Will clip the date  

---- !Parsers
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

getrRuleE :: MParser VrRule
getrRuleE =   intercalateEffect (char ' ') $ VrRule
             <$> toPermutation (getrFreq)
             <*> toPermutationWithDefault (Until Nothing) getUntilE
             <*> toPermutationWithDefault (Countr Nothing) getCountrE
             <*> toPermutationWithDefault (Interval Nothing) getIntervalE
             <*> toPermutationWithDefault (ByMonth Nothing) getByMonthE
             <*> toPermutationWithDefault (ByDay Nothing) getByDayE
             <*> toPermutationWithDefault (MonthDay Nothing) getMonthDayE
             <*> toPermutationWithDefault (YearDay Nothing) getYearDayE
             <*> toPermutationWithDefault (WeekNo Nothing) getWeekNoE

getrFreq :: MParser Vrfreq 
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
getWeeklyDate :: MParser VrRule
getWeeklyDate = do a <- string' "every " *> getDay
                   return (defvr{rFreq = WEEKLY, rbyDay =  ByDay $ Just a})

getMonthlyDate :: MParser VrRule 
getMonthlyDate = do a <- string' "every " *> getMonth
                    return (defvr{rFreq = MONTHLY})

getIntervalDef :: MParser VrRule 
getIntervalDef = do a <- getInterval <* string' " times"
                    return (defvr{rInterval = a})

getUntilDef :: MParser VrRule 
getUntilDef = do a <- string' "Until " *> getDateTime 
                 return (defvr{rUntil = Until . Just $ a})

--TODOMake a big parser which combines these
----VEVENT

-- getVevent :: MParser Vevent
-- getVevent = intercalateEffect (char ' ') $ Vevent
--              <$> toPermutation getDTStamp
--              <*> toPermutation getUID
--              <*> toPermutationWithDefault (PRIVATE) getClass
--              <*> toPermutation getDateStart
--              <*> toPermutationWithDefault (DateStop Nothing) getDateStop
--              <*> toPermutationWithDefault (Duration Nothing) getDuration
--              <*> toPermutationWithDefault (Desc Nothing) getDesc
--              <*> toPermutationWithDefault (Summary Nothing) getDesc
--              <*> toPermutationWithDefault (Priority Nothing) getPrio
--              <*> toPermutationWithDefault (EvtSequence Nothing) getEvtSeq 
--              <*> toPermutationWithDefault (Nothing) (Just <$> getTransp)
--              <*> toPermutationWithDefault (Nothing) (Just <$>getrRule)


getVeventE :: MParser Vevent
getVeventE =  intercalateEffect (char ' ') $ Vevent
                 <$> toPermutation getDTStampE
                 <*> toPermutation getUIDE
                 <*> toPermutationWithDefault (PRIVATE) getClassE
                 <*> toPermutation getDateStartE
                 <*> toPermutationWithDefault (DateStop Nothing) getDateStopE
                 <*> toPermutationWithDefault (Duration Nothing) getDurationE
                 <*> toPermutationWithDefault (Desc Nothing) getDescE
                 <*> toPermutationWithDefault (Summary Nothing) getSummaryE
                 <*> toPermutationWithDefault (Priority Nothing) getPrioE
                 <*> toPermutationWithDefault (EvtSequence Nothing) getEvtSeqE 
                 <*> toPermutationWithDefault (Nothing) (Just <$> getTranspE)
                 <*> toPermutationWithDefault (Nothing) (Just <$> getrRuleE)

getDTStampE :: MParser Datetime
getDTStampE = DT <$> getDateTime

getUIDE :: MParser UID
getUIDE = UID <$> getStr' <* crlf
 
getClassE :: MParser EClass
getClassE = choice 
 [ PUBLIC <$ string' "public" 
 , PRIVATE <$ string' "private"
 , CONFIDENTIAL <$ string' "Confidential"
 --, IANA <$ string' "public"
 --, XNAME <$ string' "public"  
 ]

-- "%YYYY%mm%ddT%HH%MM%SS" a", use this for the parser below

getICS :: MParser Vcalendar
getICS = do string' "Begin:VCALENDAR"
            prodid  <- string' "PRODID:" *> many printChar <* crlf
            version <- string' "VERSION:" *> many printChar <* crlf
            body    <- many (getVeventE <* crlf) 
            string "END:VCALENDAR"
            return (Vcalendar (Prod prodid) (Version version) GREGORIAN (TZ (TimeZone 0 False "UTC")) body)

getDateTimeE :: MParser UTCTime
getDateTimeE = do year <- replicateM 4 numberChar -- <*> getCNumType <*> getCNumType <*> (char '*') <*> 
                  month <- replicateM 2 numberChar
                  day <- replicateM 2 numberChar <* char 'T'
                  hour <- replicateM 2 numberChar
                  minute <- replicateM 2 numberChar
                  second <- replicateM 2 numberChar
                  return (UTCTime (fromGregorian (read year::Integer)  (read month:: Int) (read day :: Int)) 
                                  (secondsToDiffTime $ (read hour :: Integer)*3600 + (read minute :: Integer)*60 + (read second :: Integer)) )

getDateStartE :: MParser DateStart
getDateStartE =  DateStart <$> (string' "DTSTART:" *> getDateTimeE <* crlf)

getDateStopE :: MParser DateStop
getDateStopE =   DateStop <$> (string' "DTSTOP:" *> (Just <$> getDateTimeE) <* crlf)

getDescE :: MParser Desc
getDescE = Desc <$> (string' "DESCRIPTION:" *>  getStr <* crlf)

getSummaryE :: MParser Summary
getSummaryE = Summary <$> (string' "SUMMARY:" *>  getStr <* crlf)

getDurationE :: MParser Duration
getDurationE = Duration <$> (string' "DURATION:" *> (Just <$> getTimeDay) <* crlf)

getPrioE :: MParser Priority
getPrioE = Priority <$> (string' "PRIORITY:" *> getNumType <* crlf)

getEvtSeqE :: MParser EvtSequence
getEvtSeqE = EvtSequence <$> getNumType

getTranspE :: MParser Transp
getTranspE = choice 
 [TRANSPARENT <$ string' "Transparent"
 , OPAQUE <$ string' "Opaque"
 ]
 
------Pevent
--How to get from interpreter

getUTCPair :: MParser (UTCTime, UTCTime)
getUTCPair = (,) <$> (string' "start: " *> getDateTime ) <*> (space1 *> string' "end: " *> getDateTime)

--Grammar: Type; prio; utcpair; 

getDeadlineUTC :: MParser (UTCTime, UTCTime)
getDeadlineUTC = do string' "Deadline: "            -- <$> string' "Deadline:" *> utczero <*> getDateTime 
                    dead <- (space1 *> getDateTime)
                    return(utczero, dead)

getOrdered :: MParser Ordered
getOrdered = do prio <- string' "Priority: " *> L.decimal 
                pair <- (space1 *> getUTCPair) 
                desc <- (space1 *> getStr <* space)
                return(Ordered (ParseEvent (fromMaybe "" desc) prio pair (utctDayTime(pair^._2) -  utctDayTime(pair^._1))))

-- Priority: Int; start: date-time ; end: date-time; desc: str [every month | every weekday | x times | No Rule]
getOrderedandRule :: MParser (WithRule Ordered) 
getOrderedandRule = do ord <- getOrdered
                       rule <- space1 *> choice [Just <$> getWeeklyDate, Just <$> getMonthlyDate, Just <$> getIntervalDef, getNothing]  
                       return (WithRule ord rule)

getNothing :: MParser (Maybe a)
getNothing = Nothing <$ eof

getDeadline :: MParser Deadline
getDeadline = do prio <- string' "Priority: " *> L.decimal 
                 pair <- (space1 *>  getDeadlineUTC)
                 dur  <- (space1 *> getTimeDay)
                 desc <- (space1 *> getStr <* space)
                 return(Deadline (ParseEvent (fromMaybe "" desc) prio pair dur))

getPrioritized :: MParser Prioritized
getPrioritized = do prio <- string' "Priority: " *> L.decimal 
                    dur  <- (space1 *> getTimeDay)
                    desc <- (space1 *> getStr <* space)
                    return (Prioritized (ParseEvent (fromMaybe "" desc) prio (utczero,utczero) dur))

getTodo :: MParser Todo
getTodo = do prio <- string' "Priority:" *> L.decimal
             dur  <- (space1 *> getTimeDay)
             desc <- (space1 *> getStr <* space)
             return (Todo (ParseEvent (fromMaybe "" desc) prio (utczero,utczero) dur))

getOrderedList :: MParser [Ordered]
getOrderedList = do a <- many getOrdered
                    return a

getDeadlineList :: MParser [Deadline]
getDeadlineList = do a <- many getDeadline
                     return a

getPrioritizedList :: MParser [Prioritized]
getPrioritizedList = do a <- many getPrioritized
                        return a

getTodoList :: MParser [Todo]
getTodoList = do a <- many getTodo
                 return a       

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

getnDay :: MParser Day
getnDay = do day <- string' "next " *> getDay
             return (nextWeekday day  $ unsafePerformIO date)

getwDay :: MParser Day
getwDay = do day <- getDay
             return (nextWeekday day  $ unsafePerformIO date)

getnWeek :: MParser Day
getnWeek = do month <- string' "next week"  
              return (addDays 7 $ fromGregorian (gregYear uct) (gregMonth uct) (gregDay uct))
      where uct = unsafeCurrentTime

getYear' :: MParser Integer 
getYear' = do year <- L.decimal 
              return (year)

getYear :: MParser Integer 
getYear = do year <- replicateM 4 numberChar   
             return (read year :: Integer)

--Here to not cause a dependency cycle
vrtest = fromMaybe NoRule (parseMaybe (getrRule) "Daily Until 2014-12-31 14:15 Interval 3")

--TODO parse the Vcalendar file
--TODO withRule parser


-----Clutter
getDTStamp :: MParser Datetime
getDTStamp = DT <$> getDateTime 

getUID :: MParser UID
getUID = UID <$> getStr'

getClass :: MParser EClass
getClass = choice 
 [ PUBLIC <$ string' "public"
 , PRIVATE <$ string' "private"
 , CONFIDENTIAL <$ string' "Confidential"
 --, IANA <$ string' "public"
 --, XNAME <$ string' "public"  
 ]

getDateStart :: MParser DateStart
getDateStart =  DateStart <$> (string' "Start " *> getDateTime)

getDateStop :: MParser DateStop
getDateStop =   DateStop <$> (string' "Stop " *> (Just <$> getDateTime))

getDesc :: MParser Desc
getDesc = Desc <$> (string' "Desc " *>  getStr)

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