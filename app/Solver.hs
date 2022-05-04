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
import Data.List (nub, nubBy, sortBy, groupBy)
import Data.IntMap
import Data.Char (ord)
import Control.Lens
import Control.Monad.Except (ExceptT)
--UTCTime: yyyy-mm-ddThh:mm:ss
--possibility to split up
newtype Scheduled = Scheduled {sEvent :: Pevent}
newtype Ordered = Ordered {oEvent :: Pevent}  --Set date, eg. meetings   
newtype Deadline = DL {dEvent :: Pevent}       --Certain date to finish by, 
newtype Priority = Priority {pEvent :: Pevent} -- timed tasks with a priority, flexible (maybe with )
newtype Todo  = Todo {tEvent :: Pevent}     --Any time, priority has lower pecedence than above
--               |Free 

type CState a= State ([Scheduled], [Scheduled]) a --Conflicts/not conflicts
 --
timeCompare :: Pevent -> Pevent -> Bool
timeCompare p1 p2 = diffUTCTime ((pSET $ p1)^._1)  ((pSET $ p2)^._2) < 0 && diffUTCTime ((pSET $ p1)^._2) ((pSET $ p2)^._1) > 0  

-- ->  solve -> check and add to state -> Either for errors?   
--TODO: implement a binary search with timeCompare

constrSolve :: [Ordered] -> [Scheduled]
constrSolve = undefined

----ordSolve
typeOC :: Ordered -> Scheduled
typeOC (Ordered oevt) = Scheduled oevt

orderSolve :: [Ordered] -> [Scheduled]
orderSolve = fmap typeOC

--Finds amount of collisions in the dates
oCompare :: [Ordered] -> Int
oCompare ls = length ls -(length $ nubBy (\x y -> timeCompare (oEvent x) (oEvent y) ) ls) 

ocCheck ::[Ordered] -> CState ()-> CState ()
ocCheck ord state = if oCompare ord == 0 then modify (\(l,r) -> (l,r ++ (orderSolve ord))) else modify (\(l,r) -> (l++ (orderSolve ord),r))

--Finds the n collisions and puts them in a list
findoCollisions :: Int -> [Ordered] -> [Ordered] 
findoCollisions 0 ords = ords
findoCollisions n ords = [Ordered f1] ++ findoCollisions (n-1) (fmap Ordered f2)  
    where sorted = sortBy (\p1 p2 -> compare (view _1 (pSET $ p1)) (view _1 (pSET $ p2))) (fmap oEvent ords)  
          find (x:xs) = case timeCompare x (head xs) of
                            True  -> (head xs, tail xs)
                            False -> find xs
          (f1,f2) = (find sorted) 

findoColGroups :: [Ordered] -> [[Ordered]]
findoColGroups = groupBy (\o1 o2 -> timeCompare (oEvent o1) (oEvent o2)) 

ocCheck2 ::[Ordered] -> CState ()
ocCheck2 ord = modify (\(l,r) -> (l ++ (orderSolve $ head colGroups),r ++ (orderSolve $ concat $ tail colGroups)))
    where colGroups = findoColGroups ord

-- ----dlSolve 
-- typeDC :: Deadlline -> Scheduled
-- typeDC (DL devt) = Scheduled devt

-- dlSolve :: [Deadline] -> [Scheduled]
-- dlSolve = fmap typeDC

-- dcCheck ::[Deadline] -> CState
-- dcCheck dl = modify (++ dlSolve dl)

-- --------prioSolve
-- typePC :: Priority -> Scheduled
-- typePC prios = undefined --Scheduled pDesc (Just pTime) (Just pprio)

-- prioSolve :: [Priority] -> [Scheduled]
-- prioSolve = do fmap typePC

-- pcCheck :: [Priority] -> CState
-- pcCheck prio = undefined   --modify (++ prioSolve prio)

-- -- unorderedSolve :: [Todo] -> [Scheduled] -> CState
-- -- unorderedSolve todos = undefined

-- --Not quite tests
-- curryCalendar :: ((Integer, Int), Int) -> Day
-- curryCalendar = uncurry.uncurry $fromGregorian
fg = fromGregorian
stdt = secondsToDiffTime 
utcdates = [(UTCTime (fg 2014 12 12) (stdt 200),UTCTime (fg 2014 12 12) (stdt 230)),
            (UTCTime (fg 2014 12 12) (stdt 190), UTCTime (fg 2014 12 12) (stdt 360)),
            (UTCTime (fg 2014 12 13) (stdt 220), UTCTime (fg 2014 12 13) (stdt 250))]

timeCompare2 :: (UTCTime,UTCTime) -> (UTCTime,UTCTime) -> Bool
timeCompare2 p1 p2 = diffUTCTime (p1^._1)  (p2^._2) < 0 && diffUTCTime (p1^._2) (p2^._1) > 0  
