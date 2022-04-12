{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module InputParsers where
import Events
import Data.Void
import Control.Applicative
import  Text.Megaparsec
import  Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Time
import Data.Text ( Text, pack, intersperse )
import Control.Monad.Trans.State (StateT)


type MParser = ParsecT Void Text (State [String])


getDay :: Mparser Text
getDay = do choice [string "mon" <* string "day",
string "tue" <* string "day"
string "wen" <* string "day"
string "thurs" <* string "day"
string "fri" <* string "day"
string "satur" <* string "day"
string "sun" <* string "day"
] where 


--nextDay :: Text -> Day

--nextMonth :: Day -> Day 
--next