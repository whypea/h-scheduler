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
import Data.Text

import Control.Lens
import Control.Monad.Trans.State 
import Control.Monad
import Control.Applicative.Permutations 
type MParser = Parsec Void String 

---- !Helpers
emptySingle :: (MonadParsec e s m, Token s ~ Char) => Token s -> m () --from source of space 
emptySingle x = void $ single x

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
getIntegerType = do a <- L.decimal
                    return (Just a)

nextWeekday :: DayOfWeek -> Day -> Day -- -> UTCTime -> UTCTime 
nextWeekday wd now = addDays x now
 where x = if diff < 0 then (-toInteger diff) else 7 -(toInteger diff)
       diff = (fromEnum $dayOfWeek now) - (fromEnum wd)

--TODO Fix this
-- getStringType :: String -> MParser (Maybe a)
-- getStringType str = do a <- string' str
--                        return (Just a)

---- !Parsers
--TODO: Put together the other parsers in a permutation 
getEvent :: MParser Pevent 
getEvent = do fmap Interval getIntegerType
              fmap ByMonth getIntegerType
              fmap ByDay getIntegerType
              fmap Prio getIntegerType
              

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
 [ 1    <$ string' "Jan" <* many alphaNumChar
 , 2   <$ string' "Feb"<* many alphaNumChar 
 , 3      <$ string' "Mar" <* many alphaNumChar 
 , 4      <$ string' "Apr" <* many alphaNumChar 
 , 5        <$ string' "May"<* many alphaNumChar 
 , 6       <$ string' "Jun" <* many alphaNumChar 
 , 7          <$ string' "Jul" <* many alphaNumChar 
 , 8     <$ string' "Aug" <* many alphaNumChar 
 , 9  <$ string' "Sep" <* many alphaNumChar 
 , 10    <$ string' "Oct" <* many alphaNumChar 
 , 11   <$ string' "Nov" <* many alphaNumChar 
 , 12   <$ string' "Dec" <* many alphaNumChar ]

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

-- rfreqmap :: String -> MParser Vrfreq
-- rfreqmap s = [fmap [HOURLY, DAILY, WEEKLY, MONTHLY, YEARLY]]   

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
getnDay = do day <- string' "next "  *> getDay
             return (nextWeekday day  $unsafePerformIO date)

getnMonth :: MParser Day 
getnMonth = do month <- string' "next " *> getMonth  
               return (fromGregorian (gregYear unsafeCurrentTime) month (gregDay unsafeCurrentTime))

getnWeek :: MParser Day
getnWeek = do month <- string' "next week"  
              return (addDays 7 $fromGregorian (gregYear uct) (gregMonth uct) (gregDay uct))
      where uct = unsafeCurrentTime
getYearHelper :: MParser String
getYearHelper = takeP (Just "four") 4 <|> takeP (Just "two") 2  <* eof

getYear :: MParser Integer --(>=>) might be useful
getYear = do a <- getYearHelper
             setInput a       --cheating function, lets you combine parsers
             b <- L.decimal
             return b

getReoccur :: MParser Reoccur 
getReoccur = do a <- getIntegerType 
                return (Reoccur a)

--getICSFile :: Handle -> MParser  





-- getMonth :: MParser 
-- getMonth = undefined
