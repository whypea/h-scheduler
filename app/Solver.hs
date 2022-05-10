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
import Control.Monad.Reader (Reader)
import Control.Applicative (Const)
import Control.Lens
import Control.Monad

import Data.List (nub, nubBy, sortBy, groupBy)
-- import Data.IntMap
import Data.Char (ord)
import Data.Time
import Data.Text (Text)

newtype Scheduled = Scheduled {sEvent :: ParseEvent}
newtype Ordered = Ordered {oEvent :: ParseEvent}            --Set date, eg. meetings   
newtype Deadline = Deadline {dEvent :: ParseEvent}                --Certain date to finish by, 
newtype Prioritized = Prioritized {pEvent :: ParseEvent}    --timed tasks with a priority, flexible
newtype Todo  = Todo {tEvent :: ParseEvent}                 --Any time, priority has lower pecedence than above

type CState a = State ([Scheduled], [Scheduled]) a --Conflicts/not conflicts

wake :: DiffTime
bed :: DiffTime
(wake,bed) = (secondsToDiffTime 32400, secondsToDiffTime 75600)

getDT :: UTCTime -> DiffTime
getDT = utctDayTime

toParseEvent :: Vevent -> ParseEvent 
toParseEvent s@(Vevent {..}) = ParseEvent s (uprio ePrio) (udts eDTStart,udte eDTEnd) (fromRational . toRational $ (diffUTCTime (udte eDTEnd) (udts eDTStart)))
 where uprio (Priority (Just a)) = a  
       udts (DateStart a) = a
       udte (DateStop (Just a)) = a

--TODO These 3 need "empty" dates, last needs an empty priority and all need to 
--     backprop into Vevent

--Can store deadline in pSET_2 , zero for unscheduled values
utczero :: UTCTime
utczero = UTCTime (fromGregorian 1858 11 17) (0) 

fillVeventD :: Deadline -> Deadline
fillVeventD d = undefined

--Compares
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


----SOLVERS
constrSolve :: [Ordered] -> [Deadline] -> [Prioritized] {--> [Todo]-} -> CState ()
constrSolve ord dl pr = do let final = pCheck pr $ dcCheck dl $ ocCheck2 ord 
                           return final ()

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

--Finds the n collisions and puts them in a list
-- findoCollisions :: Int -> [Ordered] -> [Ordered] 
-- findoCollisions 0 ords = ords
-- findoCollisions n ords = [Ordered f1] ++ findoCollisions (n-1) (fmap Ordered f2)  
--     where sorted = sortBy (\p1 p2 -> compare (view _1 (pSET $ p1)) (view _1 (pSET $ p2))) (fmap oEvent ords)  
--           find (x:xs) = case timeCompare x (head xs) of
--                             True  -> (head xs, tail xs)
--                             False -> find xs
--           (f1,f2) = (find sorted) 

--These two might invalidate the functions above 
findoColGroups :: [Ordered] -> [[Ordered]]
findoColGroups = groupBy (\o1 o2 -> timeCompare (oEvent o1) (oEvent o2)) 

ocCheck2 ::[Ordered] -> CState ()
ocCheck2 ord = modify (\(l,r) -> (l ++ (orderSolve $ head colGroups), r ++ (orderSolve $ concat $ tail colGroups)))
    where colGroups = findoColGroups ord

--TODO list of times an event should repeat, for scheduled
testRRule :: VrRule -> [(UTCTime, UTCTime)]
testRRule vrr@(VrRule{..}) = undefined 

-- ----dlSolve 

-- Sort after priority before assigning 
dcCheck ::[Deadline] -> CState () -> CState ()
dcCheck dl ostate = do let sorted = sortBy (deadpCompare) (dl)
                       modify (dlSolve sorted) 
                       return ()

--Add the event to either to state (left - not possible, right - assign time)
dlAdd :: ([Scheduled], [Scheduled]) -> Deadline -> Scheduled
dlAdd (l, r) d = if ht == (utczero, utczero) then (Scheduled ParseEvent{event = NoEvent, prio = (prio $ dEvent d), pSET = pSET $ dEvent d, dur =(dur $ dEvent d)} )
                 else (Scheduled ParseEvent{event = NoEvent, prio = (prio $ dEvent d), pSET = ht, dur =(dur $ dEvent d)} )
    where deadline      = (view _2 (pSET $ dEvent $ d))
          duration      = dur $ dEvent $ d
          before        = filter (\s -> deadline < (view _2 (pSET $ sEvent $ s)) && afterwake s && beforesleep s ) r
          afterwake s   = duration + wake > getDT (view _1 (pSET $ sEvent $ s))
          beforesleep s = duration + bed < getDT (view _2 (pSET $ sEvent $ s))
          ht            = haspTime r duration

--Assm. list sorted by date, hasTimetest finds a time, if it doesn't it passes. 
hassTime :: [Scheduled] -> DiffTime -> (UTCTime,UTCTime) 
hassTime [] dt = (utczero,utczero)
hassTime [x] dt = if  (fromRational . toRational $ dt) + (utctDayTime ((pSET . sEvent $ x)^._2))  < bed 
                  then ((pSET . sEvent $ x)^._2 , addUTCTime (fromRational . toRational $ dt) ((pSET . sEvent $ x)^._2) )
                  else ((pSET . sEvent $ x)^._1, (pSET . sEvent $ x)^._2)
hassTime (x:xs) dt = if hasTimetest ((pSET . sEvent $ x)^._2) ( (pSET . sEvent $ (head xs))^._1) dt 
                    then ((pSET . sEvent $ x)^._2 , addUTCTime (fromRational . toRational $ dt) ((pSET . sEvent $ x)^._2) ) 
                    else hassTime (xs) dt
--                        
dlSolve :: [Deadline] -> ([Scheduled],[Scheduled]) -> ([Scheduled],[Scheduled])
dlSolve dl (l, r) = foldl (\(a,b) x -> if ((pSET . sEvent $ x)^._1) == utczero then (a++[x], b) else (a, b++[x])) (l,r) mapped
    where mapped = fmap (dlAdd (l, r)) dl

---------prioSolve

pCheck :: [Prioritized] -> CState () -> CState ()
pCheck prio pstate = do let sorted = sortBy priopCompare (prio)   --modify (++ prioSolve prio)
                        modify (pSolve sorted)
                        return ()

haspTime :: [Scheduled] -> DiffTime -> (UTCTime,UTCTime) 
haspTime [] dt = (utczero, utczero)
haspTime [x] dt = (utczero, utczero)
haspTime (x:xs) dt = if hasTimetest ((pSET . sEvent $ x)^._2) ( (pSET . sEvent $ (head xs))^._1) dt 
                    then ((pSET . sEvent $ x)^._2 , addUTCTime (fromRational . toRational $ dt) ((pSET . sEvent $ x)^._2) ) 
                    else haspTime (xs) dt

pAdd :: ([Scheduled],[Scheduled]) -> Prioritized -> Scheduled
pAdd (l,r) p = if ht == (utczero, utczero) then (Scheduled ParseEvent{event = NoEvent, prio = (prio $ pEvent p), pSET = pSET $ pEvent p, dur =(dur $ pEvent p)} )
                    else (Scheduled ParseEvent{event = NoEvent, prio = (prio $ pEvent p), pSET = ht, dur =(dur $ pEvent p)} )
                    where duration      = dur $ pEvent $ p
                          before        = filter (\s -> afterwake s && beforesleep s ) r
                          afterwake s   = duration + wake > getDT (view _1 (pSET $ sEvent $ s))
                          beforesleep s = duration + bed < getDT (view _2 (pSET $ sEvent $ s))
                          ht            = haspTime r duration
                          
pSolve :: [Prioritized] -> ([Scheduled],[Scheduled]) -> ([Scheduled], [Scheduled])
pSolve pr (l, r) = foldl (\(a, b) x -> if ((pSET . sEvent $ x)^._1) == utczero then (a++[x], b) else (a, b++[x])) (l, r) mapped
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
--Clashing for  
--too close to (wake,bed) = (secondsToDiffTime 32400, secondsToDiffTime 75600)
--
dldates =  [Deadline ParseEvent{event = NoEvent, prio=4, pSET =(UTCTime (fg 2014 12 12) (stdt 200), UTCTime (fg 2014 12 12) (stdt 230)), dur=3000},
            Deadline ParseEvent{event = NoEvent, prio=4 ,pSET =(UTCTime (fg 2014 12 12) (stdt 200), UTCTime (fg 2014 12 12) (stdt 230)), dur=300}
            ]
schdates = [Scheduled ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)), dur=3000},
            Scheduled ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)), dur=3000},
            Scheduled ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 40400), UTCTime (fg 2014 12 12) (stdt 44000)), dur=3000}
           ] 

priodates = [Prioritized ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)), dur=3000},
            Prioritized ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 33400), UTCTime (fg 2014 12 12) (stdt 36400)), dur=3000},
            Prioritized ParseEvent{event = NoEvent, prio=4, pSET=(UTCTime (fg 2014 12 12) (stdt 40400), UTCTime (fg 2014 12 12) (stdt 44000)), dur=3000}
           ] 

timeCompare2 :: (UTCTime,UTCTime) -> (UTCTime,UTCTime) -> Bool
timeCompare2 p1 p2 = diffUTCTime (p1^._1) (p2^._2) < 0 && diffUTCTime (p1^._2) (p2^._1) > 0  
