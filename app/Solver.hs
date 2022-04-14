module Solver where

import qualified Control.Monad.Trans.Class as Trans
import Data.Time ( UTCTime )



--UTCTime: yyyy-mm-ddThh:mm:ss
data Constraint = Ordered [UTCTime] | Deadline [UTCTime] | Prio [Priority] | Unordered [UTCTime]

data Priority = Priority UTCTime Int

--constrSolve

----orderSolve

----dlSolve

----prioSolve

----unorderedSolve