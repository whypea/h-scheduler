{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds      #-}
module InputParsers where

import Events
import Data.Void
-- import Control.Applicative
import  Text.Megaparsec
import  Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Time

import Data.Text ( Text, pack, intersperse )
import Control.Monad.Trans.State (StateT)


type MParser = Parsec Void String 

getDay :: MParser DayOfWeek
getDay = do gday <- choice [
             Monday <$ string' "mon" <* many alphaNumChar,
             Tuesday <$ string' "tue"<* many alphaNumChar , 
             Wednesday <$ string' "wed" <* many alphaNumChar, 
             Thursday <$ string' "thu" <* many alphaNumChar, 
             Friday <$ string' "fri"<* many alphaNumChar, 
             Saturday <$ string' "sat" <* many alphaNumChar, 
             Sunday <$ string' "sun" <* many alphaNumChar]
            return gday

-- getrFreq :: MParser Vrfreq
-- getrFreq = do freq <- choice [ 
--                HOURLY  <$ hour, 
--                DAILY   <$ (try string' "daily" <|> string "every day"), 
--                WEEKLY  <$ (try string' "weekly" <|> string "every week"), 
--                MONTHLY <$ (try string' "monthly" <|> string "every month"), 
--                YEARLY  <$ (try string' "yearly" <|> string "every year")]
--               return freq
--             where hour = string' "hourly" <|> string' "every hour"
-- getISO :: MParser UTCTime
-- getISO = do 
-- 


--how to extract value from the parser? 

nextWeekday :: DayOfWeek -> Day -> Day -- -> UTCTime -> UTCTime 
nextWeekday wd now = addDays x now
 where x = if diff < 0 then (-toInteger diff) else 7 - (toInteger diff)
       diff =(fromEnum wd) - (fromEnum $dayOfWeek now)

--day of week on date . then toEnum on day .  (UTCTime)
-- getMonth :: MParser 
-- getMonth = undefined

--nextMonth :: Day -> Day 
--next