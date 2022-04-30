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


--UTCTime: yyyy-mm-ddThh:mm:ss
--possibility to split up
data Constraint = Constraint {desc :: Text, time :: Maybe UTCTime , prio :: Maybe Integer }

--Set date, eg. meetings
data Ordered = Ordered {oDesc :: Text, oTime :: UTCTime}

--Certain date to finish by,   
data Deadline = DL {dDesc :: Text, dTime :: UTCTime}

-- timed tasks with a priority, flexible
data Priority = Priority {pDesc :: Text, pTime :: UTCTime, pprio :: Integer}

--Free time 
data Todo = Todo {tDesc :: Text, tTime :: UTCTime, tprio:: Integer}

type CState = State [Constraint] ()
--type ->  solve -> check and add to state  -> 

--constrSolve 
typeOC :: Ordered -> Constraint
typeOC (Ordered oDesc oTime) = Constraint oDesc (Just oTime) Nothing

orderSolve :: [Ordered] -> [Constraint]
orderSolve = fmap typeOC 

ocCheck ::[Ordered] -> CState -> CState 
ocCheck ord state = if (oChecker ord ord) then (modify (++ orderSolve ord)) else []

oChecker :: [Ordered] -> Bool
oChecker os = any $ filter (\(o,c) -> oTime == oTime ) $zip(os,os)
 
----dlSolve 
typeDC :: Deadline -> Constraint
typeDC (DL dDesc dTime) = Constraint dDesc (Just dTime) Nothing

dlSolve :: [Deadline] -> [Constraint] 
dlSolve = fmap typeDC


dcCheck ::[Deadline] -> CState  
dcCheck dl = modify (++ dlSolve dl)


--------prioSolve
prioSolve :: [Priority] -> [Constraint]
prioSolve = do fmap typePC

typePC :: Priority -> Constraint
typePC (Priority pDesc pTime pprio) = Constraint pDesc (Just pTime) (Just pprio)

pcCheck :: [Priority] -> CState  
pcCheck prio = modify (++ prioSolve prio)
 
-- unorderedSolve :: [Todo] -> [Constraint] -> CState
-- unorderedSolve todos = undefined

--Not quite tests
curryCalendar :: ((Integer, Int), Int) -> Day
curryCalendar = uncurry.uncurry $fromGregorian 

