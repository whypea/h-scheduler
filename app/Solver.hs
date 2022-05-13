{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE BlockArguments      #-}

module Solver where

import qualified Control.Monad.Trans.Class as Trans


import Events hiding (Todo)
import Control.Monad.Trans.State
import Control.Applicative (Const)
import Control.Lens
import Control.Monad

import Data.List (nub, nubBy, sortBy, groupBy)
-- import Data.IntMap
import Data.Char (ord)
import Data.Time
import Data.Text (Text)

newtype Scheduled = Scheduled {sEvent :: ParseEvent} deriving (Show)
newtype Ordered = Ordered {oEvent :: ParseEvent} deriving (Show)           --Set date, eg. meetings   
newtype Deadline = Deadline {dEvent :: ParseEvent} deriving (Show)            --Certain date to finish by, 
newtype Prioritized = Prioritized {pEvent :: ParseEvent} deriving (Show)    --timed tasks with a priority, flexible
newtype Todo  = Todo {tEvent :: ParseEvent} deriving (Show)                 --Any time, priority has lower pecedence than above

type CState a = State ([Scheduled], [Scheduled]) a --Conflicts/not conflicts

initCstate :: CState ()
initCstate = put ([],[])

wake :: DiffTime
bed :: DiffTime
(wake,bed) = (secondsToDiffTime 32400, secondsToDiffTime 75600)

getDT :: UTCTime -> DiffTime
getDT = utctDayTime

-- toParseEvent :: Vevent -> ParseEvent 
-- toParseEvent s@(Vevent {..}) = ParseEvent s (uprio ePrio) (udts eDTStart,udte eDTEnd) (fromRational . toRational $ (diffUTCTime (udte eDTEnd) (udts eDTStart)))
--  where uprio (Priority (Just a)) = a  
--        udts (DateStart a) = a
--        udte (DateStop (Just a)) = a

--TODO These 3 need "empty" dates, last needs an empty priority and all need to 
--     backprop into Vevent

--Can store deadline in pSET_2 , zero for unscheduled values
utczero :: UTCTime
utczero = UTCTime (fromGregorian 1858 11 17) (0) 

fillVeventD :: Deadline -> Deadline
fillVeventD d = undefined

--Compares time
timeCompare :: ParseEvent -> ParseEvent -> Bool
timeCompare p1 p2 = diffUTCTime ((pSET $ p1)^._1)  ((pSET $ p2)^._2) < 0 && diffUTCTime ((pSET $ p1)^._2) ((pSET $ p2)^._1) > 0  

deadpCompare :: Deadline -> Deadline -> Ordering
deadpCompare p1 p2 = compare (prio . dEvent $ p1) (prio . dEvent $ p2)

priopCompare :: Prioritized -> Prioritized -> Ordering
priopCompare p1 p2 = compare (prio . pEvent $ p1) (prio . pEvent $ p2)

todopCompare :: Todo -> Todo -> Ordering
todopCompare p1 p2 = compare (prio . tEvent $ p1) (prio . tEvent $ p2)

--Find an open timeslot by checking if stoptime and starttime are less than the duration
hasTimetest :: UTCTime -> UTCTime -> DiffTime -> Bool
hasTimetest stop start dur = comp == LT || comp == EQ
    where comp = compare (getDT stop - getDT start) dur

getsStop :: Scheduled -> UTCTime
getsStop x = (pSET . sEvent $ x)^._2

getsStart :: Scheduled -> UTCTime
getsStart x = (pSET . sEvent $ x)^._1

getdStart :: Deadline -> UTCTime 
getdStart x= (pSET . dEvent $ x)^._1

getdStop :: Deadline -> UTCTime 
getdStop x= (pSET . dEvent $ x)^._2

----SOLVERS
econstrSolve :: [Ordered] -> [Deadline] -> [Prioritized] {--> [Todo]-} -> ([Scheduled],[Scheduled])
econstrSolve ord dl pr = execState (pCheck pr $ dcCheck dl $ ocCheck2 ord) ([],[])

----ordSolve
typeOC :: Ordered -> Scheduled
typeOC (Ordered oevt) = Scheduled oevt

orderSolve :: [Ordered] -> [Scheduled]
orderSolve = fmap typeOC

--Finds amount of collisions in the dates
oCompare :: [Ordered] -> Int
oCompare ls = length ls - (length $ nubBy (\x y -> timeCompare (oEvent x) (oEvent y) ) ls) 

ocCheck ::[Ordered] -> CState () -> CState ()
ocCheck ord state = if oCompare ord == 0 then modify (\(l,r) -> (l,r ++ (orderSolve ord))) else modify (\(l,r) -> (l++ (orderSolve ord),r))

--These two might invalidate the functions above 
findoColGroups :: [Ordered] -> [[Ordered]]
findoColGroups = groupBy (\o1 o2 -> timeCompare (oEvent o1) (oEvent o2)) 

ocCheck2 ::[Ordered] -> CState ()
ocCheck2 ord = modify (\(l,r) -> (l ++ (orderSolve $ head colGroups), r ++ (orderSolve $ concat $ tail colGroups)))
    where colGroups = findoColGroups ord

--TODO list of times an event should repeat, for scheduled
-- testRRule :: VrRule -> [(UTCTime, UTCTime)]
-- testRRule vrr@(VrRule{..}) =  

-- ----dlSolve 

-- Sort after priority before assigning 
dcCheck ::[Deadline] -> CState () -> CState ()
dcCheck dl ostate = do let sorted = sortBy (deadpCompare) (dl)
                       modify (dlSolve sorted) 
                       return ()

--Add the event to either to state (left - not possible, right - assign time)
dlAdd :: ([Scheduled], [Scheduled]) -> Deadline -> Scheduled
dlAdd (l, r) d = if ht == (utczero, utczero) then (Scheduled (ParseEvent NoEvent (prio $ dEvent d) (pSET $ dEvent d) (dur $ dEvent d)) )
                 else (Scheduled (ParseEvent NoEvent (prio $ dEvent d) ht (dur $ dEvent d)) )
    where deadline      = getdStart d
          duration      = dur $ dEvent $ d
          before        = filter (\s -> deadline < (getsStop s) && afterwake s && beforesleep s ) r
          afterwake s   = duration + wake > getDT (getsStart s)
          beforesleep s = duration + bed < getDT (getsStop s)
          ht            = hasdTime r duration

--Assm. list sorted by date, hasTimetest finds a time, if it doesn't it passes. 
hasdTime :: [Scheduled] -> DiffTime -> (UTCTime,UTCTime) 
hasdTime [] dt = (utczero,utczero)
hasdTime [x] dt = if  (fromRational . toRational $ dt) + (utctDayTime (getsStop x))  < bed 
                  then (getsStop x , addUTCTime (fromRational . toRational $ dt) (getsStop x) )
                  else (getsStart x, getsStop x)
hasdTime (x:xs) dt = if hasTimetest (getsStop x) (getsStart x) dt 
                    then (getsStop x , addUTCTime (fromRational . toRational $ dt) (getsStop x) ) 
                    else hasdTime (xs) dt
--                        
dlSolve :: [Deadline] -> ([Scheduled],[Scheduled]) -> ([Scheduled],[Scheduled])
dlSolve dl (l, r) = foldl (\(a,b) x -> if (getsStart x == utczero) then (a++[x], b) else (a, b++[x])) (l,r) mapped
    where mapped = fmap (dlAdd (l, r)) dl

---------prioSolve

pCheck :: [Prioritized] -> CState () -> CState ()
pCheck prio pstate = do let sorted = sortBy priopCompare (prio)   --modify (++ prioSolve prio)
                        modify (pSolve sorted)
                        return ()

haspTime :: [Scheduled] -> DiffTime -> (UTCTime,UTCTime) 
haspTime [] dt = (utczero, utczero)
haspTime [x] dt = (utczero, utczero)
haspTime (x:xs) dt = if hasTimetest (getsStop x) (getsStart (head xs)) dt 
                    then (getsStop x , addUTCTime (fromRational . toRational $ dt) (getsStop x) ) 
                    else haspTime (xs) dt

pAdd :: ([Scheduled],[Scheduled]) -> Prioritized -> Scheduled
pAdd (l,r) p = if ht == (utczero, utczero) then (Scheduled ParseEvent{event = NoEvent, prio = (prio $ pEvent p), pSET = pSET $ pEvent p, dur =(dur $ pEvent p)} )
                    else (Scheduled ParseEvent{event = NoEvent, prio = (prio $ pEvent p), pSET = ht, dur =(dur $ pEvent p)} )
                    where duration      = dur $ pEvent $ p
                          before        = filter (\s -> afterwake s && beforesleep s ) r
                          afterwake s   = duration + wake > getDT (getsStart s)
                          beforesleep s = duration + bed < getDT (getsStop s)
                          ht            = haspTime r duration

--Checks first agrgument to see if it has been assigned, if not     
pSolve :: [Prioritized] -> ([Scheduled],[Scheduled]) -> ([Scheduled], [Scheduled])
pSolve pr (l, r) = foldl (\(a, b) x -> if getsStart x == utczero then (a++[x], b) else (a, b++[x])) (l, r) mapped
    where mapped = fmap (pAdd (l, r)) pr

-- -- tdSolve :: [Todo] -> CState () -> [Scheduled]
-- -- tdSolve todos = undefined

fg :: Integer -> Int -> Int -> Day
fg = fromGregorian

stdt = secondsToDiffTime 
utcdates = [(UTCTime (fg 2014 12 12) (stdt 200),UTCTime (fg 2014 12 12) (stdt 230)),
            (UTCTime (fg 2014 12 12) (stdt 190), UTCTime (fg 2014 12 12) (stdt 360)),
            (UTCTime (fg 2014 12 13) (stdt 220), UTCTime (fg 2014 12 13) (stdt 250))]

--Test for clashing times,
--Clashing for start, finish 
--too close to (wake,bed) = (secondsToDiffTime 32400, secondsToDiffTime 75600)


--clashing time works, but removes both dates
-- 
orddates :: [Ordered]
orddates = [Ordered (ParseEvent NoEvent 4 (UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)) 3000),
            Ordered (ParseEvent NoEvent 4 (UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)) 3000),
            Ordered (ParseEvent NoEvent 4 (UTCTime (fg 2014 12 12) (stdt 33600), UTCTime (fg 2014 12 12) (stdt 36400)) 3000),
            Ordered (ParseEvent NoEvent 4 (UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)) 3000)
           ] 

--sleep seems to work, but checks for the deadline instead of the greedy chosen time
dldates :: [Deadline]
dldates =  [Deadline ParseEvent{event = NoEvent, prio=4, pSET =(utczero, UTCTime (fg 2014 12 13) (stdt 230)), dur=3600},
            Deadline ParseEvent{event = NoEvent, prio=4 ,pSET =(utczero, UTCTime (fg 2014 12 13) (stdt 40400)), dur=3600},
            Deadline ParseEvent{event = NoEvent, prio=4 ,pSET =(utczero, UTCTime (fg 2014 12 13) (stdt 75700)), dur=3600},
            Deadline (ParseEvent  NoEvent 3 (UTCTime (fg 2014 12 12) (stdt 40400),UTCTime (fg 2014 12 12) (stdt 44000)) 3600)
            ]

priodates :: [Prioritized]
priodates = [Prioritized ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)), dur=3000},
            Prioritized ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)), dur=3000},
            Prioritized ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 40400), UTCTime (fg 2014 12 12) (stdt 44000)), dur=3000}
           ] 

schdates :: [Scheduled]
schdates = [Scheduled ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)), dur=3000},
            Scheduled ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)), dur=3000},
            Scheduled ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 40400), UTCTime (fg 2014 12 12) (stdt 44000)), dur=3000}
           ] 

