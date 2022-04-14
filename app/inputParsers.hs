{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
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

--(State [String])

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

--how to extract value from the parser? 

nextWeekday :: DayOfWeek -> Day -> Day -- -> UTCTime -> UTCTime 
nextWeekday wd now = addDays now x
 where diff = fromEnum wd - fromEnum . dayOfWeek now
       x = if diff < 0 then -diff else 7 - diff

--day of week on date . then toEnum on day .  (UTCTime)
-- getMonth :: MParser 
-- getMonth = undefined

--nextMonth :: Day -> Day 
--next