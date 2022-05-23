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
import InputParsers
import Control.Monad.Trans.State ( execState, put, State )
import Control.Monad.Trans.Reader
import Control.Applicative (Const)
import Control.Lens
import Control.Monad

import Data.List (nub, nubBy, sortBy, groupBy, sort)
-- import Data.IntMap
import Data.Char (ord)
-- import Data.Text
import Data.Time
import GHC (XAbsBinds)

type CState a = State ([WithRule Scheduled], [WithRule Scheduled]) a --Conflicts/not conflicts

----MAIN SOLVER
econstrSolve :: Opts -> [WithRule Ordered] -> [Deadline] -> [Prioritized] -> [Todo] -> ([WithRule Scheduled],[WithRule Scheduled]) 
econstrSolve opts ord dl pr td = execState (tdCheck opts td $ pCheck opts pr $ dcCheck opts dl $ ocCheck opts ord) ([],[])

schToVevent :: Scheduled -> Vevent 
schToVevent sch = Vevent (DT (getsStart sch)) (UID " ") PUBLIC (DateStart $ getsStart sch) (DateStop $ Just (getsStop sch)) 
                   (Duration Nothing) (Desc $ Just (desc .sEvent $ sch)) (Summary $ Just (desc .sEvent $ sch)) (Priority (Just (prio . sEvent $ sch))) (EvtSequence Nothing) (Just TRANSPARENT) Nothing

stateToEventR :: ([WithRule Scheduled], [WithRule Scheduled]) -> [Vevent]
stateToEventR (l, r) = fmap (schToVevent .event)  r

stateToEventL :: ([WithRule Scheduled], [WithRule Scheduled]) -> [Vevent]
stateToEventL (l, r) = fmap (schToVevent . event) l

eventsToCalendar :: [Vevent] -> Vcalendar
eventsToCalendar evts = Vcalendar (Prod "") (Version "2") GREGORIAN (TZ (TimeZone 0 False "UTC")) evts 

solveToCalendarMake :: Opts -> [WithRule Ordered] -> [Deadline] -> [Prioritized] -> [Todo] -> Vcalendar 
solveToCalendarMake opts ord dl pr td = eventsToCalendar . stateToEventR $ econstrSolve opts ord dl pr td
























liftTime :: DiffTime -> NominalDiffTime
liftTime = fromRational . toRational

schDateCompare :: Scheduled -> Scheduled -> Ordering
schDateCompare p1 p2 = compare ((pSET . sEvent $ p1)^._2) ((pSET . sEvent $ p2)^._2)

deadpCompare :: Deadline -> Deadline -> Ordering
deadpCompare p1 p2 = compare (prio . dEvent $ p1) (prio . dEvent $ p2)

priopCompare :: Prioritized -> Prioritized -> Ordering
priopCompare p1 p2 = compare (prio . pEvent $ p1) (prio . pEvent $ p2)

todopCompare :: Todo -> Todo -> Ordering
todopCompare p1 p2 = compare (prio . tEvent $ p1) (prio . tEvent $ p2)

--Find an open timeslot by checking if stoptime_last - starttime_first are less than the duration
hasTimetest :: UTCTime -> UTCTime -> DiffTime -> Bool
hasTimetest stop start dur = comp == LT || comp == EQ  --EQ makes it open
    where comp = compare dur (getDT stop - getDT start)

withinDay :: Opts -> DiffTime -> Bool 
withinDay opts x  = x > wakeO opts && x < bedO opts

--Compares time 
timeCompare :: Opts -> ParseEvent -> ParseEvent -> Bool
timeCompare opts p1 p2 =  not (dateoverlap p1 p2) --not overlapping
                    && withinDay opts (utctDayTime ((pSET $ p1)^._1)) && withinDay opts (utctDayTime ((pSET $ p2)^._1)) --first event inside of day
                    && withinDay opts (utctDayTime ((pSET $ p1)^._2)) && withinDay opts (utctDayTime ((pSET $ p2)^._2)) --second event inside of day

dateoverlap :: ParseEvent -> ParseEvent -> Bool
dateoverlap p1 p2 = (diffUTCTime ((pSET $ p1)^._1) ((pSET $ p2)^._2) < 0 && diffUTCTime ((pSET $ p1)^._2) ((pSET $ p2)^._1) > 0 )

timeoverlap:: (UTCTime, UTCTime) -> (UTCTime, UTCTime) -> Bool
timeoverlap (t1, t2) (c1, c2) = diffUTCTime t1 c2 < 0 && diffUTCTime t2 c1 > 0 

schDateCompareW :: WithRule Scheduled -> WithRule Scheduled -> Ordering
schDateCompareW p1 p2 = compare ((pSET . sEvent .event $ p1)^._2) ((pSET . sEvent . event $ p2)^._2)

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
freqAdder HOURLY s (Interval (Just n))   = s{utctDayTime = (utctDayTime s + (secondsToDiffTime $ 3600*n))} 
freqAdder DAILY  s (Interval (Just n))   = UTCTime (addDays n (utctDay s)) (utctDayTime s)
freqAdder WEEKLY s (Interval (Just n))   = UTCTime (addDays (7*n) (utctDay s)) (utctDayTime s)
freqAdder MONTHLY s (Interval (Just n))  = UTCTime (addGregorianMonthsClip n (utctDay s)) (utctDayTime s)
freqAdder YEARLY  s (Interval (Just n))  = UTCTime  (addGregorianYearsClip n (utctDay s)) (utctDayTime s)

freqChecker :: (UTCTime, UTCTime) -> [(UTCTime, UTCTime)] -> Bool
freqChecker t rlist = all (== True) $ map (timeoverlap t) rlist

----SOLVERS
----ordSolve

ordGroups :: Opts -> [WithRule Ordered] -> ([WithRule Ordered], [WithRule Ordered])
ordGroups opts ordered = foldl (\(l, r) x -> if all (==True) (fmap (trule x) r) 
                      && all (timeCompare opts $ oEvent $ event x) (fmap (oEvent . event) r )  == True 
                     then (l,r++[x]) 
                     else (l++[x], r) ) ([], [head ordered]) (tail ordered)
    where trule x r= case rule r of 
                     Just s -> freqChecker (pSET. oEvent . event $ x) $ testRRule' s (pSET . oEvent . event $ r)
                     Nothing ->  True
    -- checking x against recur: True if no rule, false if any dates overlap

-- Current functions will (should) always assign times (with the exception of clashing ordered and overdue deadline)
--      as it assigns them greedily on any available timeslots, without any higher bound. 
ocCheck ::Opts -> [WithRule Ordered] -> CState ()
ocCheck opts ord = put (fmap retype (fst $ colGroups), fmap retype (snd $ colGroups) )
    where colGroups = ordGroups opts ord
          retype (WithRule (Ordered oevt) rule) = WithRule (Scheduled oevt) rule

eocCheck :: Opts -> [WithRule Ordered] -> ([WithRule Scheduled], [WithRule Scheduled])
eocCheck opts ord = execState (ocCheck opts ord) ([],[])

-- Sort after priority before assigning 
dcCheck :: Opts -> [Deadline] -> CState () -> CState ()
dcCheck opts dl ostate = put (solved) 
 where sorted = sortBy deadpCompare dl   
       solved = dlSolve opts sorted (execState ostate ([],[]))

--Should always find a spot for it, since any events before the deadline are filtered out
--Using utczero as empty datetime
hasdTimeRule ::Opts -> [WithRule Scheduled] -> DiffTime -> (UTCTime,UTCTime)
hasdTimeRule opts [] dt = (utczero,utczero)
hasdTimeRule opts (x:[]) dt = dhelper opts dt x
hasdTimeRule opts (x:xs) dt = if hasTimetest (getsStop . event $ x) (getsStart (event $ head xs)) dt && trule  
                     then dhelper opts dt x 
                     else hasdTimeRule opts (xs) dt
    where trule = case rule x of 
                     Just s -> freqChecker (pSET. sEvent . event $ x) $ testRRule' s (pSET . sEvent . event $ x)
                     Nothing ->  True 

dhelper :: Opts -> DiffTime -> WithRule Scheduled -> (UTCTime, UTCTime)
dhelper opts dt x 
    | bedh < (utctDayTime . getsStop .event $ x) + dt   = (addUTCTime (liftTime (bedh- wakeh)) (UTCTime (utctDay . getsStop . event $ x) (bedh)), addUTCTime (liftTime (bedh- wakeh + dt)) 
                                                          (UTCTime (utctDay . getsStop . event $ x) (bedh)) )
    | otherwise                               = (getsStop . event $ x, addUTCTime (liftTime dt) (getsStop . event $ x) )
    where bedh = bedO opts
          wakeh = bedO opts 
--Add the event to either to state (left - not possible, right - assign time)
dlAdd :: Opts -> ([WithRule Scheduled], [WithRule Scheduled]) -> Deadline -> WithRule Scheduled
dlAdd opt (l, r) d = if ht == (utczero, utczero) then passevent 
                 else addevent
    where deadline      = getdStop d --deadline stored in snd
          duration      = dur $ dEvent $ d 
          before        = filter (\s -> deadline > ((getsStop . event) s)) (sortBy (schDateCompareW) r) -- filters events deadline > end of other event
          ht            = hasdTimeRule opt before duration
          passevent     = (WithRule (Scheduled (ParseEvent (desc $ dEvent d) (prio $ dEvent d) (pSET $ dEvent d) (dur $ dEvent d))) Nothing )
          addevent      = (WithRule (Scheduled (ParseEvent (desc $ dEvent d) (prio $ dEvent d) ht (dur $ dEvent d))) Nothing )
          
--Assm. list sorted by date, hasTimetest finds a time, if it doesn't it passes. 
dlSolve :: Opts -> [Deadline] -> ([WithRule Scheduled],[WithRule Scheduled]) -> ([WithRule Scheduled],[WithRule Scheduled])
dlSolve opt dl (l, r) = foldl (\(a,b) x -> condition (a,b) x) (l,r) dl
    where condition (a,b) x = if ((getsStart . event $ adder (a,b) x) == utczero) 
                              then (a ++ [(adder (a,b) x)], b) 
                              else (a, b ++ [(adder (a,b) x)])
          adder (d,f) y = dlAdd opt (d,f) (y)

dlSolvet :: Opts -> [Deadline]
    -> ([WithRule Scheduled], [WithRule Scheduled]) -> [([WithRule Scheduled], [WithRule Scheduled])]
dlSolvet opts dl (l, r) = scanl (\(a,b) x -> condition (a,b) x) (l,r) dl
    where condition (a,b) x= if ((getsStart . event $ (dlAdd opts (a,b) x)) == utczero) then (a++[(dlAdd opts (a,b) x)], b) else (a, b++[(dlAdd opts (a,b) x)])

---------prioSolve

pCheck :: Opts ->[Prioritized] -> CState () -> CState ()
pCheck opts prio dlstate = put (solved)    --modify (++ prioSolve prio)
    where sorted = sortBy priopCompare (prio)
          solved = pSolve opts sorted (execState dlstate ([],[])) 

haspTime :: Opts ->[WithRule Scheduled] -> DiffTime -> (UTCTime,UTCTime) 
haspTime opts [] dt = (utczero,utczero)
haspTime opts [x] dt = phelper opts dt x
haspTime opts (x:xs) dt = if hasTimetest (getsStop . event $ x) (getsStart (event $ head xs)) dt then phelper opts dt x else haspTime opts (xs) dt

phelper :: Opts -> DiffTime -> WithRule Scheduled -> (UTCTime, UTCTime)
phelper opts dt x = if bedh < (utctDayTime . getsStop . event $ x)+dt --if it's past bedtime, add the event in the morning
                    then (addUTCTime (liftTime (bedh - wakeh)) (UTCTime (utctDay . getsStop . event $  x) (bedO opts)), addUTCTime (liftTime (bedh - wakeh + dt)) (UTCTime (utctDay . getsStop . event $ x) (bedh)) )
                    else (getsStop . event $ x, addUTCTime (liftTime dt) (getsStop .event $ x) ) 
    where bedh = bedO opts
          wakeh = wakeO opts

pAdd :: Opts -> ([WithRule Scheduled],[WithRule Scheduled]) -> Prioritized -> WithRule Scheduled
pAdd opts (l,r) p = addevent
                    where duration      = dur $ pEvent $ p
                          giventime     = haspTime opts r duration
                          addevent      = (WithRule (Scheduled (ParseEvent (desc $ pEvent p) (prio $ pEvent p) giventime (dur $ pEvent p))) Nothing )

--Just adds everything in turn    
pSolve :: Opts ->[Prioritized] -> ([WithRule Scheduled],[WithRule Scheduled]) -> ([WithRule Scheduled], [WithRule Scheduled])
pSolve opts pr (w, s) = foldl (\(l, r) x -> (l, r ++ [pAdd opts (l, r) x])) (w, s) pr  

----todoSolve
tdCheck :: Opts -> [Todo] -> CState () -> CState ()
tdCheck opts td pstate = put (solved) 
    where sorted = sortBy todopCompare (td)
          solved = tdSolve opts sorted (execState pstate ([],[]))

hastdTime :: Opts -> [WithRule Scheduled] -> DiffTime -> (UTCTime,UTCTime) 
hastdTime opts [] dt = (utczero,utczero)
hastdTime opts [x] dt = thelper opts dt x
hastdTime opts (x:xs) dt = if hasTimetest (getsStop . event $  x) (getsStart (event $ head xs)) dt then thelper opts dt x else hastdTime opts (xs) dt

thelper :: Opts -> DiffTime -> WithRule Scheduled -> (UTCTime, UTCTime)
thelper opt dt x = if bedh < (utctDayTime . getsStop . event $ x)+dt --if it's past bedtime, add the event in the morning
                   then (addUTCTime (liftTime (bedh - wakeh)) (UTCTime (utctDay . getsStop . event $  x) (bedh)), addUTCTime (liftTime (bedh- wakeh + dt)) (UTCTime (utctDay . getsStop . event $  x) bedh) )
                   else (getsStop . event $ x, addUTCTime (liftTime dt) (getsStop . event $  x) ) 
    where bedh = bedO opt
          wakeh = wakeO opt
          
tdAdd :: Opts -> ([WithRule Scheduled], [WithRule Scheduled]) -> Todo -> WithRule Scheduled
tdAdd opts (l, r) td = (WithRule (Scheduled (ParseEvent (desc $ tEvent td) (prio $ tEvent td) giventime (dur $ tEvent td))) Nothing )
                    where duration      = dur $ tEvent $ td
                          giventime     = hastdTime opts r duration 

tdSolve :: Opts -> [Todo] -> ([WithRule Scheduled], [WithRule Scheduled]) ->  ([WithRule Scheduled], [WithRule Scheduled])
tdSolve opts todos (w, s) = foldl (\(l, r) x -> (l, r ++ [tdAdd opts (l, r) x])) (w, s) todos  

stateToSols :: ([WithRule Scheduled], [WithRule Scheduled]) ->  ([String], [String])
stateToSols = over both (fmap $ show . event )

--TODO Pevent -> Solver types -> Vevent -> Print