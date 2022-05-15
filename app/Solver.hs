{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE BlockArguments      #-}

module Solver where

import qualified Control.Monad.Trans.Class as Trans


import Events hiding (Todo)

import Common

import Control.Monad.Trans.State
import Control.Applicative (Const)
import Control.Lens
import Control.Monad

import Data.List (nub, nubBy, sortBy, groupBy)
-- import Data.IntMap
import Data.Char (ord)
import Data.Text (Text)
import Data.Time
import Events (.Rule(rInterval))

type CState a = State ([Scheduled], [Scheduled]) a --Conflicts/not conflicts

initCstate :: CState ()
initCstate = put ([],[])

liftTime = fromRational . toRational

----SOLVERS
----MAIN SOLVER
econstrSolve :: [Ordered] -> [Deadline] -> [Prioritized] {--> [Todo]-} -> ([Scheduled],[Scheduled]) 
econstrSolve ord dl pr = execState (pCheck pr $ dcCheck dl $ ocCheck2 ord) ([],[])

----ordSolve

--Finds amount of collisions in the dates
oCompare :: [Ordered] -> Int
oCompare ls = length ls - (length $ nubBy (\x y -> timeCompare (oEvent x) (oEvent y) ) ls) 

-- ocCheck ::[Ordered] -> CState () -> CState ()
-- ocCheck oord state = if oCompare ord == 0 then modify (\(l,r) -> (l,r ++ (orderSolve ord))) else modify (\(l,r) -> (l++ (orderSolve ord),r))

--These two might invalidate the functions above 
findoColGroups :: [Ordered] -> [[Ordered]]
findoColGroups = groupBy (\o1 o2 -> timeCompare (oEvent o1) (oEvent o2)) 

--TODO: Current functions will (should) always assign times (with the exception of clashing ordered and overdue deadline)
--      as it assigns them greedily on any available timeslots, without any higher bound.   
ocCheck2 ::[Ordered] -> CState ()
ocCheck2 ord = modify (\(l,r) -> (l ++ (fmap retype (head colGroups)), r ++ (fmap retype (concat $ tail colGroups))))
    where colGroups = findoColGroups ord
          retype (Ordered oevt) =  Scheduled oevt

--TODO list of times an event should repeat, for scheduled
--TODO 
--VrRule gives the repeat, 
testRRule' :: VrRule -> (UTCTime, UTCTime) -> [(UTCTime, UTCTime)]
testRRule' vrr start  = zip (utclist $ fst start) (utclist $ snd start)  
    where freqAdd date = freqAdder (rFreq vrr) date (rInterval vrr)
          utclist date = [ (UTCTime x y) | x <- [(utctDay date), (utctDay $ freqAdd date ) .. (utctDay (untiler $ rUntil vrr))], 
                                           y <- [utctDayTime date, utctDayTime $ freqAdd date.. secondsToDiffTime 75600]]
          untiler (Until (Just a)) = a
          
freqAdder :: Vrfreq -> UTCTime -> Interval -> UTCTime
freqAdder (HOURLY) s (Interval (Just n))   = s{utctDayTime = (utctDayTime s + (secondsToDiffTime $ 3600*n))} 
freqAdder (DAILY)  s (Interval (Just n))   = UTCTime (addDays n (utctDay s)) (utctDayTime s)
freqAdder (WEEKLY) s (Interval (Just n))   = UTCTime (addDays (7*n) (utctDay s)) (utctDayTime s)
freqAdder (MONTHLY) s (Interval (Just n))  = UTCTime (addGregorianMonthsClip n (utctDay s)) (utctDayTime s)
freqAdder (YEARLY)  s (Interval (Just n))  = UTCTime  (addGregorianYearsClip n (utctDay s)) (utctDayTime s)

-- Sort after priority before assigning 
dcCheck ::[Deadline] -> CState () -> CState ()
dcCheck dl ostate = do let sorted = sortBy (deadpCompare) (dl)
                       modify (dlSolve sorted) 
                       return ()

--Should always find a spot for it, since any events before the deadline are filtered out
--Using utczero as empty list
hasdTime :: [Scheduled] -> DiffTime -> (UTCTime,UTCTime) 
hasdTime [] dt = (utczero,utczero)
hasdTime [x] dt = dhelper dt x
hasdTime (x:xs) dt = if hasTimetest (getsStop x) (getsStart (head xs)) dt then dhelper dt x else hasdTime (xs) dt

dhelper :: DiffTime -> Scheduled -> (UTCTime, UTCTime)
dhelper dt x = if bed < (utctDayTime $ getsStop x)+dt --if it's past bedtime, add the event in the morning
              then (addUTCTime (liftTime (bed - wake)) (UTCTime (utctDay $ getsStop x) (bed)), addUTCTime (liftTime (bed - wake + dt)) (UTCTime (utctDay $ getsStop x) (bed)) )
              else (getsStop x, addUTCTime (liftTime dt) (getsStop x) )

--Add the event to either to state (left - not possible, right - assign time)
dlAdd :: ([Scheduled], [Scheduled]) -> Deadline -> Scheduled
dlAdd (l, r) d = if ht == (utczero, utczero) then (Scheduled (ParseEvent NoEvent (prio $ dEvent d) (pSET $ dEvent d) (dur $ dEvent d)) ) --
                 else (Scheduled (ParseEvent NoEvent (prio $ dEvent d) ht (dur $ dEvent d)) )
    where deadline      = getdStop d --deadline stored in snd
          duration      = dur $ dEvent $ d 
          before        = filter (\s -> deadline > (getsStop s)) r -- filters events deadline > end of other event
        --   afterwake s   = duration + wake > getDT (getsStart s)
        --   beforesleep s = duration + bed < getDT (getsStop s)
          ht            = hasdTime r duration

--Assm. list sorted by date, hasTimetest finds a time, if it doesn't it passes. 
dlSolve :: [Deadline] -> ([Scheduled],[Scheduled]) -> ([Scheduled],[Scheduled])
dlSolve dl (l, r) = foldl (\(a,b) x -> condition (a,b) x) (l,r) dl
    where condition (a,b) x= if (getsStart (dlAdd (a,b) x) == utczero) then (a++[(dlAdd (a,b) x)], b) else (a, b++[(dlAdd (a,b) x)])

---------prioSolve

pCheck :: [Prioritized] -> CState () -> CState ()
pCheck prio pstate = do let sorted = sortBy priopCompare (prio)   --modify (++ prioSolve prio)
                        modify (pSolve sorted)
                        return ()

haspTime :: [Scheduled] -> DiffTime -> (UTCTime,UTCTime) 
haspTime [] dt = (utczero,utczero)
haspTime [x] dt = dhelper dt x
haspTime (x:xs) dt = if hasTimetest (getsStop x) (getsStart (head xs)) dt then dhelper dt x else hasdTime (xs) dt

pAdd :: ([Scheduled],[Scheduled]) -> Prioritized -> Scheduled
pAdd (l,r) p = if ht == (utczero, utczero) then (Scheduled (ParseEvent NoEvent (prio $ pEvent p) (pSET $ pEvent p) (dur $ pEvent p)) )
                    else (Scheduled (ParseEvent NoEvent (prio $ pEvent p) ht (dur $ pEvent p)) )
                    where duration      = dur $ pEvent $ p
                          before        = filter (\s -> afterwake s && beforesleep s ) r
                        --   afterwake s   = duration + wake > getDT (getsStart s)
                        --   beforesleep s = duration + bed < getDT (getsStop s)
                          ht            = haspTime r duration

--Checks first agrgument to see if it has been assigned, if not     
pSolve :: [Prioritized] -> ([Scheduled],[Scheduled]) -> ([Scheduled], [Scheduled])
pSolve dl (l, r) = foldl (\(a,b) x -> if (getsStart (pAdd (a,b) x) == utczero) then (a++[(pAdd (a,b) x)], b) else (a, b++[(pAdd (a,b) x)])) (l,r) dl

    -- where mapped = fmap (pAdd (l, r)) pr

---- tdSolve :: [Todo] -> CState () -> [Scheduled]
---- tdSolve todos = undefined

--Test for clashing times,
--Clashing for start, finish 
--too close to (wake,bed) = (secondsToDiffTime 32400, secondsToDiffTime 75600)

