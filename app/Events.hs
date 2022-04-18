{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts   #-}

module Events where
import Data.Void
import Control.Applicative
import Text.Megaparsec ( Parsec )
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

type UID = Text  
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
    }

newtype MT a= MT {a :: Maybe a}

instance Show a => Show (MT a) where
    show (MT (Just a)) = show a
    show (MT Nothing) = show ""


-- data VrRule = VrRule
--     {
--     rFreq    :: Vrfreq
--     ,rUntil  :: Day
--     ,rReoccur :: Integer
--     ,rInterval :: Integer
--     ,rbyMonth :: Maybe Integer
--     ,rbyDay :: Maybe Integer
--     }

data VRule = R Vrfreq (MT Day) (MT Integer) (MT Integer) (MT Integer) (MT Integer) 

--more pattern matches for mutual exclusivity?
instance Show VRule where
  show (R freq d reocc int mon day) = 
      "RRULE:" ++ "FREQ:" ++ show freq ++ ";" ++ "UNTIL:" ++ show d  ++ ";" ++ "COUNT:" ++ show reocc ++  ";" ++ "INTERVAL:" ++ show int ++ ";" ++"BYMONTH:" ++ show mon ++ ";" ++ "BYDAY:" ++ show day

--Cutting out some of the choices
data Vrfreq = HOURLY | DAILY | WEEKLY | MONTHLY | YEARLY 
 deriving (Show, Eq, Ord)

data Test = Test (Maybe Int) deriving (Show, Read)

data Vcalendar = Vcalendar
    { 
    cProdId       :: Text
    , cVersion    :: Text
    , cScale      :: Text
    , cTimeZones  :: Text
    , cEvents     :: [Vevent]
    }

--instance Show Vcalendar where

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
pSemiC = intersperse ';' 


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





