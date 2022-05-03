{-# LANGUAGE OverloadedStrings #-}
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
import Events
import Control.Monad.Reader (Reader)
import Control.Applicative (Const)
import Data.List (nub, nubBy)
import Data.IntMap
import Data.Char (ord)
import Control.Lens
--UTCTime: yyyy-mm-ddThh:mm:ss
--possibility to split up
newtype Constraint = Constraint {cEvent :: Pevent}
newtype Ordered = Ordered {oEvent :: Pevent}  --Set date, eg. meetings   
newtype Deadline = DL {dEvent :: Pevent}       --Certain date to finish by,
newtype Priority = Priority {pEvent :: Pevent} -- timed tasks with a priority, flexible
newtype Todo  = Todo {tEvent :: Pevent}     --Any time, priority has lower pecedence than above
--               |Free 

type CState = State (Either [Constraint] [Constraint]) 

--
timeCompare :: Pevent -> Pevent -> Bool
timeCompare p1 p2 = diffUTCTime ((pSET $ p1)^._1)  ((pSET $ p2)^._2) > 0|| diffUTCTime ((pSET $ p1)^._1) ((pSET $ p2)^._2) > 0  

-- ->  solve -> check and add to state -> Either for errors?  

--TODO: implement a binary search with timeCompare

--constrSolve :: [Ordered] -> [Constraint]
-- typeOC :: Constraint -> Constraint
-- typeOC (Ordered oevt) = Constraint oevt

-- orderSolve :: [Constraint] -> [Constraint]
-- orderSolve = fmap typeOC

-- ocCheck ::[Constraint] -> CState ()  -> CState ()
-- ocCheck ord state = if timeCompare (pslot . oEvent) then modify (Right :ord) else modify (Left :ord)

-- oCompare :: [Ordered] -> Bool
-- oCompare ls = length $ nubBy (timeCompare (pSET . oEvent)) ls == length ls


-- ----dlSolve 
-- typeDC :: Constraint -> Constraint
-- typeDC (DL devt) = Constraint devt

-- dlSolve :: [Constraint] -> [Constraint]
-- dlSolve = fmap typeDC

-- dcCheck ::[Constraint] -> CState
-- dcCheck dl = modify (++ dlSolve dl)

-- --------prioSolve
-- typePC :: Constraint -> Constraint
-- typePC prios = undefined --Constraint pDesc (Just pTime) (Just pprio)

-- prioSolve :: [Constraint] -> [Constraint]
-- prioSolve = do fmap typePC

-- pcCheck :: [Constraint] -> CState
-- pcCheck prio = undefined--modify (++ prioSolve prio)

-- -- unorderedSolve :: [Todo] -> [Constraint] -> CState
-- -- unorderedSolve todos = undefined

-- --Not quite tests
-- curryCalendar :: ((Integer, Int), Int) -> Day
-- curryCalendar = uncurry.uncurry $fromGregorian

