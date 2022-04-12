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
--nextDay :: Text -> Day

--nextMonth :: Day -> Day 
--next