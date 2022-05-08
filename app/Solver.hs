{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE BlockArguments      #-}
module Solver where

import qualified Control.Monad.Trans.Class as Trans
import Data.Time
import Data.Text (Text)
import Control.Monad.Trans.State
import Events hiding (Todo)
import Control.Monad.Reader (Reader)
import Control.Applicative (Const)
import Data.List (nub, nubBy, sortBy, groupBy)
-- import Data.IntMap
import Data.Char (ord)
import Control.Lens
import Control.Monad

newtype Scheduled = Scheduled {sEvent :: Pevent}
newtype Ordered = Ordered {oEvent :: Pevent}            --Set date, eg. meetings   
newtype Deadline = DL {dEvent :: Pevent}                --Certain date to finish by, 
newtype Prioritized = Prioritized {pEvent :: Pevent}    --timed tasks with a priority, flexible (maybe with )
newtype Todo  = Todo {tEvent :: Pevent}                 --Any time, priority has lower pecedence than above

type CState a = State ([Scheduled], [Scheduled]) a --Conflicts/not conflicts

wake :: DiffTime
bed :: DiffTime
(wake,bed) = (secondsToDiffTime 32400, secondsToDiffTime 75600)

getDT :: UTCTime -> DiffTime
getDT = utctDayTime
--nominalDiffTime -> 
toPevent :: Vevent -> Pevent 
toPevent s@(Vevent {..}) = Pevent s (uprio ePrio) (udts eDTStart,udte eDTEnd) (fromRational . toRational $ (diffUTCTime (udte eDTEnd) (udts eDTStart)))
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
timeCompare :: Pevent -> Pevent -> Bool
timeCompare p1 p2 = diffUTCTime ((pSET $ p1)^._1)  ((pSET $ p2)^._2) < 0 && diffUTCTime ((pSET $ p1)^._2) ((pSET $ p2)^._1) > 0  

deadpCompare :: Deadline -> Deadline -> Ordering
deadpCompare p1 p2 = compare (prio . dEvent $ p1) (prio . dEvent $ p2)

priopCompare :: Prioritized -> Prioritized -> Ordering
priopCompare p1 p2 = compare (prio . pEvent $ p1) (prio . pEvent $ p2)

todopCompare :: Todo -> Todo -> Ordering
todopCompare p1 p2 = compare (prio . tEvent $ p1) (prio . tEvent $ p2)

--readers 

constrSolve :: [Ordered] -> [Deadline] -> [Prioritized] -> [Todo] -> Either [Scheduled] [Scheduled]
constrSolve = undefined

----ordSolve
typeOC :: Ordered -> Scheduled
typeOC (Ordered oevt) = Scheduled oevt

orderSolve :: [Ordered] -> [Scheduled]
orderSolve = fmap typeOC

--Finds amount of collisions in the dates
oCompare :: [Ordered] -> Int
oCompare ls = length ls - (length $ nubBy (\x y -> timeCompare (oEvent x) (oEvent y) ) ls) 

ocCheck ::[Ordered] -> CState ()-> CState ()
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

-- ----dlSolve 
typeDC :: Deadline -> Scheduled
typeDC (DL devt) = Scheduled devt

-- Sort after
dcCheck ::[Deadline] -> CState () -> CState ()
dcCheck dl ostate = do let sorted = sortBy (deadpCompare) (dl)
                       modify (dlSolve sorted) 
                       return ()

--Add the event to either to state (left - not possible, right - assign time)

dlAdd :: ([Scheduled], [Scheduled]) -> Deadline -> Scheduled
dlAdd (l, r) d = if ht == (utczero, utczero) then (Scheduled Pevent{event = NoEvent, prio = (prio $ dEvent d), pSET = pSET $ dEvent d, dur =(dur $ dEvent d)} )
                 else (Scheduled Pevent{event = NoEvent, prio = (prio $ dEvent d), pSET = ht, dur =(dur $ dEvent d)} )
    where deadline = (view _2 (pSET $ dEvent $ d))
          duration = dur $ dEvent $ d
          before   = filter (\s -> deadline < (view _2 (pSET $ sEvent $ s)) && afterwake s && beforesleep s ) r
          afterwake s= duration + wake > getDT (view _1 (pSET $ sEvent $ s))
          beforesleep s= duration + bed < getDT (view _2 (pSET $ sEvent $ s))
          ht = hasTime r duration

--Find an open timeslot by checking if 
hasTimetest :: UTCTime -> UTCTime -> DiffTime -> Bool
hasTimetest stop start dur = comp == LT || comp == EQ
    where comp = compare (getDT stop - getDT start) dur

hasTime :: [Scheduled] -> DiffTime -> (UTCTime,UTCTime) 
hasTime [] dt = (utczero, utczero)
hasTime [x] dt = (utczero, utczero)
hasTime (x:xs) dt = if hasTimetest ((pSET . sEvent $ x)^._2) ( (pSET . sEvent $ (head xs))^._1) dt 
                    then ((pSET . sEvent $ x)^._2 , addUTCTime (fromRational . toRational $ dt) ((pSET . sEvent $ x)^._2) ) 
                    else hasTime (xs) dt
                        
dlSolve :: [Deadline] -> ([Scheduled],[Scheduled]) -> ([Scheduled],[Scheduled])
dlSolve dl (l, r) = foldl (\(a,b) x -> if ((pSET . sEvent $ x)^._1) == utczero then (a++[x], b) else (a, b++[x])) (l,r) mapped
    where mapped = fmap (dlAdd (l, r)) dl

-- --------prioSolve
-- typePC :: Priority -> Scheduled
-- typePC prios = undefined --Scheduled pDesc (Just pTime) (Just pprio)

-- prioSolve :: [Priority] -> [Scheduled]
-- prioSolve = do fmap typePC

-- pcCheck :: [Priority] -> CState
-- pcCheck prio = undefined   --modify (++ prioSolve prio)

-- -- unorderedSolve :: [Todo] -> CState () -> [Scheduled]
-- -- unorderedSolve todos = undefined

-- --Not quite tests
-- curryCalendar :: ((Integer, Int), Int) -> Day
-- curryCalendar = uncurry.uncurry $fromGregorian

fg = fromGregorian
stdt = secondsToDiffTime 
utcdates = [(UTCTime (fg 2014 12 12) (stdt 200),UTCTime (fg 2014 12 12) (stdt 230)),
            (UTCTime (fg 2014 12 12) (stdt 190), UTCTime (fg 2014 12 12) (stdt 360)),
            (UTCTime (fg 2014 12 13) (stdt 220), UTCTime (fg 2014 12 13) (stdt 250))]


timeCompare2 :: (UTCTime,UTCTime) -> (UTCTime,UTCTime) -> Bool
timeCompare2 p1 p2 = diffUTCTime (p1^._1) (p2^._2) < 0 && diffUTCTime (p1^._2) (p2^._1) > 0  
