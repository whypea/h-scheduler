module Datetests where

import Events 
import Common

import Data.Time 

fg :: Integer -> Int -> Int -> Day
fg = fromGregorian

stdt :: Integer -> DiffTime
stdt = secondsToDiffTime 

utcdates = [(UTCTime (fg 2014 12 12) (stdt 200),UTCTime (fg 2014 12 12) (stdt 230)),
            (UTCTime (fg 2014 12 12) (stdt 190), UTCTime (fg 2014 12 12) (stdt 360)),
            (UTCTime (fg 2014 12 13) (stdt 220), UTCTime (fg 2014 12 13) (stdt 250))]

--clashing time works, but removes both dates
orddates :: [Ordered]
orddates = [Ordered (ParseEvent NoEvent 4 (UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)) 3000),
            Ordered (ParseEvent NoEvent 4 (UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)) 3000),
            Ordered (ParseEvent NoEvent 4 (UTCTime (fg 2014 12 12) (stdt 72100), UTCTime (fg 2014 12 12) (stdt 75700)) 3000), 
            Ordered (ParseEvent NoEvent 4 (UTCTime (fg 2014 12 12) (stdt 44200), UTCTime (fg 2014 12 12) (stdt 47200)) 3000) 
           ] 

--sleep seems to work, but checks for the deadline instead of the greedy chosen time
dldates :: [Deadline]
dldates =  [Deadline (ParseEvent NoEvent 4 (utczero, UTCTime (fg 2014 12 13) (stdt 230)) 3600), --before sleep
            Deadline (ParseEvent NoEvent 4 (utczero, UTCTime (fg 2014 12 13) (stdt 40400)) 3600), --asserting
            Deadline (ParseEvent NoEvent 4 (utczero, UTCTime (fg 2014 12 15)  (stdt 75700)) 3600), --past sleep
            Deadline (ParseEvent  NoEvent 3 (utczero,UTCTime (fg 2014 12 12) (stdt 44000)) 3600), --same time as ordered event
            Deadline (ParseEvent NoEvent 2 (utczero,UTCTime (fg 2014 12 14) (stdt 72000)) 3600),
            Deadline (ParseEvent NoEvent 4 (utczero,UTCTime (fg 2014 12 16) (stdt 54000)) 7200)
            ]

priodates :: [Prioritized]
priodates = [Prioritized (ParseEvent NoEvent 3 (utczero, utczero) 3000),
            Prioritized (ParseEvent NoEvent 5 (utczero, utczero) 5000),
            Prioritized (ParseEvent NoEvent 4 (utczero, utczero) 3000)
           ] 

--same as ordered but put in this list for convenience s
scheduleddates :: [Scheduled]
scheduleddates = [Scheduled (ParseEvent NoEvent 4 (UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)) 3000),
            Scheduled (ParseEvent NoEvent 4 (UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)) 3000),
            Scheduled (ParseEvent NoEvent 4 (UTCTime (fg 2014 12 12) (stdt 72100), UTCTime (fg 2014 12 12) (stdt 75700)) 3000), 
            Scheduled (ParseEvent NoEvent 4 (UTCTime (fg 2014 12 12) (stdt 44200), UTCTime (fg 2014 12 12) (stdt 47200)) 3000) 
           ] 

-- vrtest = VrRule HOURLY (Until Just (UTCTime (fg 2014 12 13) (stdt 40400))) (Countr (Just 5)) (Interval (Just 2)) (ByMonth Nothing) (ByDay Nothing) (MonthDay Nothing) (YearDay Nothing) (WeekNo Nothing)

-- schdates :: [Scheduled]
-- schdates = [Scheduled ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)), dur=3000},
--             Scheduled ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)), dur=3000},
--             Scheduled ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 40400), UTCTime (fg 2014 12 12) (stdt 44000)), dur=3000}
--            ] 