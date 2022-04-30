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
import Data.Tuple.Select
import Data.Text

import Control.Lens
import Control.Monad.Trans.State 
import Control.Monad

type MParser = Parsec Void String 

---- !Helpers
emptySingle :: (MonadParsec e s m, Token s ~ Char) => Token s -> m () --from source of space 
emptySingle x = void $ single x

date :: IO Day -- :: (year,month,day)
date = getCurrentTime >>=  return . utctDay

unsafeCurrentTime :: UTCTime
unsafeCurrentTime = unsafePerformIO getCurrentTime 

gregYear :: UTCTime -> Integer 
gregYear x = (toGregorian $utctDay x).^1

gregMonth :: UTCTime -> Int 
gregMonth x = (toGregorian $utctDay x).^2

gregDay :: UTCTime -> Int 
gregDay x = (toGregorian $utctDay x).^3

makeUTCTime :: Day -> DiffTime -> UTCTime
makeUTCTime day time = UTCTime (day) (time)  

---- !Parsers
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

getrFreq :: MParser Vrfreq
getrFreq = do choice 
 [ HOURLY  <$ string' "hourly"
 , HOURLY  <$ string' "every hour" 
 , DAILY   <$ string' "daily"       -- <|> string "every day"), 
 , DAILY   <$ string' "every day"       -- <|> string "every day"), 
 , WEEKLY  <$ string' "weekly"      -- <|> string "every week"), 
 , WEEKLY  <$ string' "every week"      -- <|> string "every week"), 
 , MONTHLY <$ string' "monthly"     -- <|> string "every month"), 
 , MONTHLY <$ string' "every month"     -- <|> string "every month"), 
 , YEARLY  <$ string' "yearly"       --  <|> string "every year")]
 , YEARLY  <$ string' "every year"]       --  <|> string "every year")]

getISO :: MParser Day
getISO = do year <- L.decimal
            try $emptySingle '-' <|> space1
            month <- L.decimal
            try $emptySingle '-' <|> space1
            day <- L.decimal
            return (fromGregorian year month day) --Will clip the date  

getDateMonth :: MParser Day
getDateMonth = do month <- L.decimal
                  try $emptySingle '-' <|> space1
                  day <- L.decimal
                  return (fromGregorian (gregYear unsafeCurrentTime) month day) 

getnDay :: MParser Day
getnDay = do day <- string' "next "  *> getDay
             return (nextWeekday day  $unsafePerformIO date)

getnMonth :: MParser Day 
getnMonth = do month <- string' "next " *> getMonth  
               return (fromGregorian (gregYear unsafeCurrentTime) month (gregDay unsafeCurrentTime))

getYearHelper :: MParser String
getYearHelper = takeP (Just "four") 4 <|> takeP (Just "two") 2  <* eof

getYear :: MParser Integer
getYear = do a <- getYearHelper
             setInput a       --cheating function, lets you combine parsers
             b <- L.decimal
             return b

getReoccur :: MParser Reoccur
getReoccur = do r <- L.decimal
                return r

--getICSFile :: Handle

nextWeekday :: DayOfWeek -> Day -> Day -- -> UTCTime -> UTCTime 
nextWeekday wd now = addDays x now
 where x = if diff < 0 then (-toInteger diff) else 7 -(toInteger diff)
       diff = (fromEnum $dayOfWeek now) - (fromEnum wd)


-- getMonth :: MParser 
-- getMonth = undefined
