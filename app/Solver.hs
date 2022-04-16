module Solver where

import qualified Control.Monad.Trans.Class as Trans
import Data.Time ( UTCTime )



--UTCTime: yyyy-mm-ddThh:mm:ss
data Constraint = Ordered [UTCTime] | Deadline [UTCTime] | Prio [Priority] | Unordered [Todo]

data Priority = Priority String UTCTime Int

data Todo = Todo String Int

--constrSolve

----orderSolve before a given time

----dlSolve 

----prioSolve

----unorderedSolve