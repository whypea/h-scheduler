

module Events where
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}


import Data.Void
import Control.Applicative
import  Text.Megaparsec
import  Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Time
import Data.Text ( Text, pack, intersperse )
import GHC.IO (unsafePerformIO)
import Class (Class)
import Data.Data
import Control.Monad.Trans.State (StateT)
import GHC.Conc (pseq)


data Event a = Calendar a | Event a| Todo a
data Datetime = Dt Integer Integer Integer deriving (Show, Eq, Ord)

type UID = String
--Types for iCal
data Vevent = Vevent
    {
    eDTstamp :: Datetime
    ,eUID     :: UID
    ,eClass   :: Text
    ,eDTStart :: Datetime
    ,eDescription :: Maybe Text
    ,ePrio :: Maybe Text
    ,eSeq :: Maybe Text
    ,eTimeTrans :: Maybe Text
    ,eRecur :: Maybe Text
    ,eAlarm :: Maybe Text
    ,eRRule :: Maybe Text
    } deriving (Show, Eq, Ord)

data VrRule = VrRule
    {
    rFreq    :: Vrfreq
    ,rUntil  :: Day
    ,rReoccur :: Integer
    ,rInterval :: Integer
    ,rbyMonth :: Maybe Integer
    ,rbyDay :: Maybe Integer
    }
--Cutting out some of the choices
data Vrfreq = HOURLY | DAILY | WEEKLY | MONTHLY | YEARLY deriving (Show, Eq, Ord)


data Vcalendar = Vcalendar
    { cProdId     :: Text
    , cVersion    :: Text
    , cScale      :: Text
    , cTimeZones  :: Text
    , cEvents     :: [Vevent]
    } deriving (Show, Eq, Ord, Typeable)


type TParser = Parsec Void String
type ShParser = Parsec Void Text
type MyStack a = StateT Day TParser a

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay


--printers
pBegin :: IO Text
pBegin = pure "BEGIN:"

pEND :: IO Text
pEND = pure "END:"

pSemiC :: Text -> Text
pSemiC = intersperse ";" 


-- dayP :: Tparser String
-- dayP =satisfy (\x -> )
--take utc time as day-month-year, get timezone convert into something workable

-- dayParser :: String -> TParser Integer
-- dayParser s= do
--              let str = s

timeParser :: TParser Text
timeParser = do
    let utctime = unsafePerformIO $ show <$> date


    return (pack utctime)





