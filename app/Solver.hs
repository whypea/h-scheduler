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


--UTCTime: yyyy-mm-ddThh:mm:ss
--possibility to split up
data Constraint = Constraint {desc :: Text, time :: Maybe UTCTime , prio :: Maybe Integer }
                 |Ordered {oDesc :: Text, oTime :: UTCTime}
                 |DL {dDesc :: Text, dTime :: UTCTime}
                 |Priority {pDesc :: Text, pTime :: UTCTime, pprio :: Integer}
                 |Todo {tDesc :: Text, tTime :: UTCTime, tprio:: Integer}
--Set date, eg. meetings
-- data Ordered = Ordered {oDesc :: Text, oTime :: UTCTime}

-- --Certain date to finish by,   
-- data Deadline = DL {dDesc :: Text, dTime :: UTCTime}

-- -- timed tasks with a priority, flexible
-- data Priority = Priority {pDesc :: Text, pTime :: UTCTime, pprio :: Integer}

-- --Free time 
-- data Todo = Todo {tDesc :: Text, tTime :: UTCTime, tprio:: Integer}

type CState = State  [Constraint] Constraint

type CReader = Reader [Constraint]
--type in constraint ->  solve -> check and add to state -> Either for errors?  

--constrSolve :: [Constraint] -> 
typeOC :: Constraint -> Constraint
typeOC (Ordered oDesc oTime) = Constraint oDesc (Just oTime) Nothing

orderSolve :: [Constraint] -> [Constraint]
orderSolve = fmap typeOC

ocCheck ::[Constraint] -> CState -> CState
ocCheck ord state = if oChecker ord then modify (++ orderSolve ord) else modify (++ [])


oChecker :: [Constraint] -> Bool
oChecker s = length s == length (nubBy oTest s)
    where oTest a b = oTime a == oTime b
 

--oChecker os = not (any (\(o1,o2) -> oTime o1 == oTime o2 ) (zip os os))

----dlSolve 
typeDC :: Constraint -> Constraint
typeDC (DL dDesc dTime) = Constraint dDesc (Just dTime) Nothing

dlSolve :: [Constraint] -> [Constraint]
dlSolve = fmap typeDC

dcCheck ::[Constraint] -> CState
dcCheck dl = modify (++ dlSolve dl)


--------prioSolve
prioSolve :: [Constraint] -> [Constraint]
prioSolve = do fmap typePC

typePC :: Constraint -> Constraint
typePC (Priority pDesc pTime pprio) = Constraint pDesc (Just pTime) (Just pprio)

pcCheck :: [Constraint] -> CState
pcCheck prio = modify (++ prioSolve prio)

-- unorderedSolve :: [Todo] -> [Constraint] -> CState
-- unorderedSolve todos = undefined

--Not quite tests
curryCalendar :: ((Integer, Int), Int) -> Day
curryCalendar = uncurry.uncurry $fromGregorian

