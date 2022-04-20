{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Solver where

import qualified Control.Monad.Trans.Class as Trans
import Data.Time ( UTCTime )
import Data.Text (Text)
import Control.Monad.Trans.State
import Events


--UTCTime: yyyy-mm-ddThh:mm:ss
data Constraint = Constraint {time :: Maybe UTCTime, desc :: Text, prio :: Maybe Int }

data Constraints o d p t = C [o] [d] [p] [t]

data Ordered = Ordered {oDesc :: Text, oTime :: UTCTime} 

data Deadline = DL {dDesc :: Text, dTime :: UTCTime}

data Priority = Priority {pDesc :: Text, pTime :: UTCTime}

data Todo = Todo {tDesc :: Text, tTime :: UTCTime}

type CState = State [Constraint] [Constraint]

--constrSolve

--

----orderSolve before a given time, use State
orderSolve ::  [Ordered] -> CState  
orderSolve ords = do 
                    lst <- ords
                    until (lst == []) state fillS 
                where fillS = undefined 

----dlSolve 
dlSolve :: [Deadline] -> [Constraint] -> CState
dlSolve dl = undefined

-- ----prioSolve
prioSolve :: [Priority] -> [Constraint] -> CState
prioSolve = undefined
-- ----unorderedSolve

unorderedSolve :: [Todo] -> [Constraint] -> CState
unorderedSolve todos = undefined