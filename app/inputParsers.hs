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

import Data.Void
import Data.Time
import Data.Time.Calendar.MonthDay
import Data.Tuple.Select
import Data.Text ( Text, pack, intersperse )
import Control.Monad.Trans.State (StateT)


type MParser = Parsec Void String 

date :: IO Day -- :: (year,month,day)
date = getCurrentTime >>=  return . utctDay

gregYear :: UTCTime -> Integer 
gregYear x = sel1 (toGregorian $utctDay x)


getDay :: MParser DayOfWeek
getDay = do choice 
 [ Monday    <$ string' "mon" <* many alphaNumChar
 , Tuesday   <$ string' "tue"<* many alphaNumChar 
 , Wednesday <$ string' "wed" <* many alphaNumChar 
 , Thursday  <$ string' "thu" <* many alphaNumChar 
 , Friday    <$ string' "fri"<* many alphaNumChar 
 , Saturday  <$ string' "sat" <* many alphaNumChar 
 , Sunday    <$ string' "sun" <* many alphaNumChar ]


getrFreq :: MParser Vrfreq
getrFreq = do choice 
 [ HOURLY  <$ string' "hourly"
 , HOURLY  <$ string' "every hour" 
 , DAILY   <$ string' "daily"       -- <|> string "every day"), 
 , WEEKLY  <$ string' "weekly"      -- <|> string "every week"), 
 , MONTHLY <$ string' "monthly"     -- <|> string "every month"), 
 , YEARLY  <$ string' "yearly"]       --  <|> string "every year")]

getISO :: MParser Day
getISO = do year <- L.decimal
            try single '-' <|> space1
            month <- L.decimal
            try single '-' <|> space1
            --satisfy (\x -> (x <= 12)) month
            day <- L.decimal
            return (fromGregorian year month day)  

getDateMonth :: MParser Day
getDateMonth = do month <- L.decimal
                  space1
                  day <- L.decimal
                  return (fromGregorian ((gregYear $unsafePerformIO getCurrentTime) month day)) 

getnDay :: MParser Day
getnDay = do day <- string' "next "  *> getDay
             return (nextWeekday day  $unsafePerformIO date)

getnMonth :: MParser Day 
getnMonth = do day <- string' "next" *> 
               return (fromGregorian )


 --how to extract value from the parser? A: parse will return the AST wrapped in Either 

nextWeekday :: DayOfWeek -> Day -> Day -- -> UTCTime -> UTCTime 
nextWeekday wd now = addDays x now
 where x = if diff < 0 then (-toInteger diff) else 7 -(toInteger diff)
       diff = (fromEnum $dayOfWeek now) - (fromEnum wd)


-- getMonth :: MParser 
-- getMonth = undefined

--nextMonth :: Day -> Day 
--next