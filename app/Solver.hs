{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE BlockArguments      #-}

module Solver where

import qualified Control.Monad.Trans.Class as Trans


import Events

import Common

import Control.Monad.Trans.State
import Control.Applicative (Const)
import Control.Lens
import Control.Monad

import Data.List (nub, nubBy, sortBy, groupBy, sort)
-- import Data.IntMap
import Data.Char (ord)
-- import Data.Text
import Data.Time

type CState a = State ([Scheduled], [Scheduled]) a --Conflicts/not conflicts
data RecurR = Recur [(UTCTime, UTCTime)]

initCstate :: CState ()
initCstate = put ([],[])

liftTime = fromRational . toRational

--TODO list of times an event should repeat, for scheduled
--TODO put this into the main solver
--VrRule gives the repeat, 
testRRule' :: VrRule -> (UTCTime, UTCTime) -> [(UTCTime, UTCTime)]
testRRule' vrr start  = zip (utclist $ fst start) (utclist $ snd start)  
    where freqAdd date = freqAdder (rFreq vrr) date (rInterval vrr)
          utclist date = [ (UTCTime x y) | x <- datelist date, 
                                           y <- timelist date]
          untiler (Until (Just a)) = a
          datelist date = if (rFreq vrr) == HOURLY 
                          then [(utctDay date).. (utctDay (untiler $ rUntil vrr))] 
                          else [(utctDay date), (utctDay $ freqAdd date ) .. (utctDay (untiler $ rUntil vrr))] 
          timelist date = if not $ (rFreq vrr) == HOURLY 
                          then [utctDayTime date] 
                          else [utctDayTime date, utctDayTime $ freqAdd date.. secondsToDiffTime 75600]

freqAdder :: Vrfreq -> UTCTime -> Interval -> UTCTime
freqAdder (HOURLY) s (Interval (Just n))   = s{utctDayTime = (utctDayTime s + (secondsToDiffTime $ 3600*n))} 
freqAdder (DAILY)  s (Interval (Just n))   = UTCTime (addDays n (utctDay s)) (utctDayTime s)
freqAdder (WEEKLY) s (Interval (Just n))   = UTCTime (addDays (7*n) (utctDay s)) (utctDayTime s)
freqAdder (MONTHLY) s (Interval (Just n))  = UTCTime (addGregorianMonthsClip n (utctDay s)) (utctDayTime s)
freqAdder (YEARLY)  s (Interval (Just n))  = UTCTime  (addGregorianYearsClip n (utctDay s)) (utctDayTime s)

--TODO Could be rewritten to return the date
freqChecker :: (UTCTime, UTCTime) -> [(UTCTime, UTCTime)] -> Bool
freqChecker t rlist = all (== True) $ map (timeoverlap t) rlist

----SOLVERS
----MAIN SOLVER
econstrSolve :: [Ordered] -> [Deadline] -> [Prioritized] -> [Todo] -> ([Scheduled],[Scheduled]) 
econstrSolve ord dl pr td= execState (tdCheck td $ pCheck pr $ dcCheck dl $ ocCheck ord) ([],[])

----ordSolve

--Finds amount of collisions in the dates
oCompare :: [Ordered] -> Int
oCompare ls = length ls - (length $ nubBy (\x y -> timeCompare (oEvent x) (oEvent y) ) ls) 

ordGroups :: [Ordered] -> ([Ordered], [Ordered])
ordGroups ordered = foldl (\(l, r) x -> if all (timeCompare $ oEvent x) (fmap oEvent r)  == True then (l,r++[x]) else (l++[x], r) ) ([], [head ordered]) (tail ordered)
    -- where sord = sortBy (ordered)

--TODO: Current functions will (should) always assign times (with the exception of clashing ordered and overdue deadline)
--      as it assigns them greedily on any available timeslots, without any higher bound. 
ocCheck ::[Ordered] -> CState ()
ocCheck ord = put (fmap retype (fst $ colGroups), fmap retype (snd $ colGroups) )
    where colGroups = ordGroups ord
          retype (Ordered oevt) =  Scheduled oevt

eocCheck :: [Ordered] -> ([Scheduled], [Scheduled])
eocCheck ord = execState (ocCheck ord) ([],[])

-- Sort after priority before assigning 
dcCheck ::[Deadline] -> CState () -> CState ()
dcCheck dl ostate = put (solved) 
 where sorted = sortBy deadpCompare dl   
       solved = dlSolve sorted (execState ostate ([],[]))

edcCheck :: [Ordered]  -> [Deadline] -> ([Scheduled], [Scheduled])
edcCheck ord dl = execState (dcCheck dl $ ocCheck ord) ([],[])

--Should always find a spot for it, since any events before the deadline are filtered out
--Using utczero as empty list
hasdTime :: [Scheduled] -> DiffTime -> (UTCTime,UTCTime)
hasdTime [] dt = (utczero,utczero)
hasdTime (x:[]) dt = dhelper dt x
hasdTime (x:xs) dt = if hasTimetest (getsStop x) (getsStart (head xs)) dt then dhelper dt x else hasdTime (xs) dt

dhelper :: DiffTime -> Scheduled -> (UTCTime, UTCTime)
dhelper dt x 
    | bed < (utctDayTime $ getsStop x) + dt   = (addUTCTime (liftTime (bed - wake)) (UTCTime (utctDay $ getsStop x) (bed)), addUTCTime (liftTime (bed - wake + dt)) (UTCTime (utctDay $ getsStop x) (bed)) )
    | otherwise                               = (getsStop x, addUTCTime (liftTime dt) (getsStop x) )

--Add the event to either to state (left - not possible, right - assign time)
dlAdd :: ([Scheduled], [Scheduled]) -> Deadline -> Scheduled
dlAdd (l, r) d = if ht == (utczero, utczero) then (Scheduled (ParseEvent (desc $ dEvent d) (prio $ dEvent d) (pSET $ dEvent d) (dur $ dEvent d)) ) 
                 else (Scheduled (ParseEvent (desc $ dEvent d) (prio $ dEvent d) ht (dur $ dEvent d) ) )
    where deadline      = getdStop d --deadline stored in snd
          duration      = dur $ dEvent $ d 
          before        = filter (\s -> deadline > (getsStop s)) (sortBy (schDateCompare) r) -- filters events deadline > end of other event
          ht            = hasdTime before duration
          
--Assm. list sorted by date, hasTimetest finds a time, if it doesn't it passes. 
dlSolve :: [Deadline] -> ([Scheduled],[Scheduled]) -> ([Scheduled],[Scheduled])
dlSolve dl (l, r) = foldl (\(a,b) x -> condition (a,b) x) (l,r) dl
    where condition (a,b) x = if (getsStart (adder (a,b) x) == utczero) then (a++[(adder (a,b) x)], b) else (a, b++[(adder (a,b) x)])
          adder (d,f) y = dlAdd (d,f) y 

dlSolvet :: [Deadline]
    -> ([Scheduled], [Scheduled]) -> [([Scheduled], [Scheduled])]
dlSolvet dl (l, r) = scanl (\(a,b) x -> condition (a,b) x) (l,r) dl
    where condition (a,b) x= if (getsStart (dlAdd (a,b) x) == utczero) then (a++[(dlAdd (a,b) x)], b) else (a, b++[(dlAdd (a,b) x)])

---------prioSolve

pCheck :: [Prioritized] -> CState () -> CState ()
pCheck prio dlstate = put (solved)    --modify (++ prioSolve prio)
    where sorted = sortBy priopCompare (prio)
          solved = pSolve sorted (execState dlstate ([],[])) 

haspTime :: [Scheduled] -> DiffTime -> (UTCTime,UTCTime) 
haspTime [] dt = (utczero,utczero)
haspTime [x] dt = dhelper dt x
haspTime (x:xs) dt = if hasTimetest (getsStop x) (getsStart (head xs)) dt then dhelper dt x else haspTime (xs) dt

phelper :: DiffTime -> Scheduled -> (UTCTime, UTCTime)
phelper dt x = if bed < (utctDayTime $ getsStop x)+dt --if it's past bedtime, add the event in the morning
               then (addUTCTime (liftTime (bed - wake)) (UTCTime (utctDay $ getsStop x) (bed)), addUTCTime (liftTime (bed - wake + dt)) (UTCTime (utctDay $ getsStop x) (bed)) )
               else (getsStop x, addUTCTime (liftTime dt) (getsStop x) ) 

pAdd :: ([Scheduled],[Scheduled]) -> Prioritized -> Scheduled
pAdd (l,r) p = (Scheduled (ParseEvent (desc $ pEvent p) (prio $ pEvent p) giventime (dur $ pEvent p)) )
                    where duration      = dur $ pEvent $ p
                          giventime     = haspTime r duration

--Just adds everything in turn    
pSolve :: [Prioritized] -> ([Scheduled],[Scheduled]) -> ([Scheduled], [Scheduled])
pSolve pr (w, s) = foldl (\(l, r) x -> (l, r ++ [pAdd (l, r) x])) (w, s) pr  

----todoSolve
tdCheck :: [Todo] -> CState () -> CState ()
tdCheck td pstate = put (solved) 
    where sorted = sortBy todopCompare (td)
          solved = tdSolve sorted (execState pstate ([],[]))

hastdTime :: [Scheduled] -> DiffTime -> (UTCTime,UTCTime) 
hastdTime [] dt = (utczero,utczero)
hastdTime [x] dt = thelper dt x
hastdTime (x:xs) dt = if hasTimetest (getsStop x) (getsStart (head xs)) dt then thelper dt x else hastdTime (xs) dt

thelper :: DiffTime -> Scheduled -> (UTCTime, UTCTime)
thelper dt x = if bed < (utctDayTime $ getsStop x)+dt --if it's past bedtime, add the event in the morning
              then (addUTCTime (liftTime (bed - wake)) (UTCTime (utctDay $ getsStop x) (bed)), addUTCTime (liftTime (bed - wake + dt)) (UTCTime (utctDay $ getsStop x) (bed)) )
              else (getsStop x, addUTCTime (liftTime dt) (getsStop x) ) 

tdAdd :: ([Scheduled], [Scheduled]) -> Todo -> Scheduled
tdAdd (l, r) td =  (Scheduled (ParseEvent (desc $ tEvent td) (prio $ tEvent td) giventime (dur $ tEvent td)) )
                    where duration      = dur $ tEvent $ td
                          giventime     = hastdTime r duration 

tdSolve :: [Todo] -> ([Scheduled], [Scheduled]) ->  ([Scheduled], [Scheduled])
tdSolve todos (w, s) = foldl (\(l, r) x -> (l, r ++ [tdAdd (l, r) x])) (w, s) todos  

--check this
stateToSols :: ([Scheduled], [Scheduled]) ->  ([String], [String])
stateToSols = over both (fmap show)

