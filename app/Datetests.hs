module Datetests where

import Events 
import Common
import Solver
import InputParsers
import Data.Time 
import InputParsers (vrtest)

fg :: Integer -> Int -> Int -> Day
fg = fromGregorian

stdt :: Integer -> DiffTime
stdt = secondsToDiffTime 
wrvrrtest :: a -> WithRule a
wrvrrtest a= WithRule a (Just vrtest)
utcdates =  [(UTCTime (fg 2014 12 12) (stdt 200),UTCTime (fg 2014 12 12) (stdt 230)),
            (UTCTime (fg 2014 12 12) (stdt 190), UTCTime (fg 2014 12 12) (stdt 360)),
            (UTCTime (fg 2014 12 13) (stdt 220), UTCTime (fg 2014 12 13) (stdt 250))]

--clashing time works, but removes both dates
orddates :: [WithRule Ordered]
orddates =  fmap wrvrrtest [ Ordered $ (ParseEvent ("ord") 4 (UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)) 3000 ),
            Ordered $ (ParseEvent ("ord1") 4 (UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)) 3000 ),
            Ordered $ (ParseEvent ("ord2") 4 (UTCTime (fg 2014 12 12) (stdt 72100), UTCTime (fg 2014 12 12) (stdt 75700)) 3000 ), 
            Ordered $ (ParseEvent ("ord3") 4 (UTCTime (fg 2014 12 12) (stdt 44200), UTCTime (fg 2014 12 12) (stdt 47200)) 3000 ) 
           ] 

dldates :: [Deadline]
dldates =   [Deadline (ParseEvent ("dead") 4 (utczero, UTCTime (fg 2014 12 13) (stdt 230)) 3600 ), --before sleep
            Deadline (ParseEvent ("dead1") 4 (utczero, UTCTime (fg 2014 12 13) (stdt 40400)) 3600 ), --asserting
            Deadline (ParseEvent ("dead2") 4 (utczero, UTCTime (fg 2014 12 15)  (stdt 75700)) 3600 ), --past sleep
            Deadline (ParseEvent  ("dead3") 3 (utczero,UTCTime (fg 2014 12 12) (stdt 44000)) 3600 ), --same time as ordered event
            Deadline (ParseEvent ("dead_fail") 6 (utczero, UTCTime (fg 2014 12 12)  (stdt 33000)) 3600 ),
            Deadline (ParseEvent ("dead4") 2 (utczero,UTCTime (fg 2014 12 14) (stdt 72000)) 3600 ),
            Deadline (ParseEvent ("dead5") 4 (utczero,UTCTime (fg 2014 12 16) (stdt 54000)) 7200 ),
            Deadline (ParseEvent ("dead6") 5 (utczero, UTCTime (fg 2014 12 12)  (stdt 33000)) 3600 ),
            Deadline (ParseEvent ("dead7") 5 (utczero, UTCTime (fg 2014 12 12)  (stdt 22000)) 3600 ),
            Deadline (ParseEvent ("dead8") 4 (utczero,UTCTime (fg 2014 12 16) (stdt 54000)) 10800 )
            ]

priodates :: [Prioritized]
priodates = [Prioritized (ParseEvent ("prio") 3 (utczero, utczero) 3000 ),
            Prioritized (ParseEvent ("prio1") 5 (utczero, utczero) 5000 ),
            Prioritized (ParseEvent ("prio2") 4 (utczero, utczero) 3000 )
           ] 

tddates :: [Todo]
tddates =  [Todo (ParseEvent ("td") 3 (utczero, utczero) 3000 ),
           Todo (ParseEvent ("td1") 3 (utczero, utczero) 2500 ),
           Todo (ParseEvent ("td2") 5 (utczero, utczero) 5000 ),
           Todo (ParseEvent ("td3") 4 (utczero, utczero) 3000)
           ] 

--same as ordered but put in this list for convenience s
scheduleddates :: [Scheduled]
scheduleddates = [Scheduled (ParseEvent ("sch") 4 (UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)) 3000),
            Scheduled (ParseEvent ("sch1") 4 (UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)) 3000),
            Scheduled (ParseEvent ("sch2") 4 (UTCTime (fg 2014 12 12) (stdt 72100), UTCTime (fg 2014 12 12) (stdt 75700)) 3000), 
            Scheduled (ParseEvent ("sch3") 4 (UTCTime (fg 2014 12 12) (stdt 44200), UTCTime (fg 2014 12 12) (stdt 47200)) 3000) 
           ] 

vrruledates = (UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400))

datetime = (UTCTime (fg 2014 12 12) (stdt 36400), UTCTime (fg 2014 12 12) (stdt 39400))