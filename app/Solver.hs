{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE BlockArguments      #-}
module Solver where

import qualified Control.Monad.Trans.Class as Trans
import Data.Time ( UTCTime )
import Data.Text (Text)
import Control.Monad.Trans.State
import Events


--UTCTime: yyyy-mm-ddThh:mm:ss
data Constraint = Constraint {desc :: Text, time :: Maybe UTCTime , prio :: Maybe Integer }

data Constraints o d p t = C [o] [d] [p] [t]

data Ordered = Ordered {oDesc :: Text, oTime :: UTCTime}

data Deadline = DL {dDesc :: Text, dTime :: UTCTime}

data Priority = Priority {pDesc :: Text, pTime :: UTCTime, pprio :: Integer}

data Todo = Todo {tDesc :: Text, tTime :: UTCTime, tprio:: Integer}

type CState = State [Constraint] [Constraint]

--constrSolve

--

--orderSolve for a given time, 
--how to use State to collate all the answers?
--  
-- orderSolve ::  [Ordered] -> CState  
-- orderSolve ords = do 
--                     let o = ords
--                     until (lst == []) state fillS
--                     return 

orderSolve :: [Ordered] -> [Constraint]
orderSolve = fmap fillOC 

fillOC :: Ordered -> Constraint
fillOC (Ordered oDesc oTime) = Constraint oDesc (Just oTime) Nothing


----dlSolve 
dlSolve :: [Deadline] -> [Constraint] 
dlSolve dl = undefined

fillDC :: Deadline -> Constraint
fillDC (DL dDesc dTime) = Constraint dDesc (Just dTime) Nothing

-- -- ----prioSolve
prioSolve :: [Priority] -> [Constraint] -> CState
prioSolve = undefined
-- -- ----unorderedSolve

fillPC :: Priority -> Constraint
fillPC (Priority pDesc pTime pprio) = Constraint pDesc (Just pTime) (Just pprio)

-- unorderedSolve :: [Todo] -> [Constraint] -> CState
-- unorderedSolve todos = undefined