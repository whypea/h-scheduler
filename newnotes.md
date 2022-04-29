
input
    either a new user-generated ics
    or a file to be modified
    options
        asking for more options if they aren't filled out?
        defaults for options?
        be verbose?

input is parsed (newFile ) into Data.Time (Day/time), Timezone
    what do I do with a parser/megaparsec/optparse? X
        megaparsec works better
Day/time will be distributed by the solver
Each Type will be evaluated by a function in order

State as a way of checking? 

Install containers
Types cli -> Vevent -> solver (potentially no date) -> Vevent, rrule -> 
How to get between cli and megaparsec
    I guess just call a function?
Solver: 
Add to the list of values,
Check the same dates/times
Check for recurrence 
   (So you need recurrance rule and a parser for it)
   how would you encode recurrence ?
    The ByDay/ByMonth, then check if that mod whatever is on. 
    Count: check in the obvious way first? 
 Due windows
Add to list
Continue

Error messages in megaparsec
    For each of the different parsers, how would that be done?



icalendar file printing options
    - multiple events in one call
    - repeating rule
    - UID generator
    -
-- Define bounds for time scheduling (sleep)
    -- As defaults/present at startup
-- Preference/"balance" constraints
    --Priority for schedule
-- (X) Sleep time, other free time
    -- unlikely to be of use
-- ordered list -> disordered list -> rest? (free time)
    -return 
    -Ordered list -> Deterministic state shop
    -Prioritized list -> Job shop
    -Unordred list -> Project
( ) cli print an entire calendar/change one


--shorthands (with try)
    -- Mon(day)    -> closest monday X
    -- 0103        -> closest 1st of march
    --"in a year"  -> from today
    -- 3am (mon)   -> closest (monday)
    -- 5 times     -> frequency  
    -- asap?       -> check other events, category
    -- every       -> byMonth/byYear
    -- x times     -> 


-- set defaults 
-- options and printer
----datetime format
----multiple events 
----recurrance X
----type of events



--utkast
--leap = (\x -> (x `mod` 4 == 0) /= (x `mod` 100 /= 0 && x `mod` 400 == 0))
--satisfy (\x -> x > 0 && x < (monthLength (isLeapYear year) month)) day
--satisfy (\x -> (x > 2000 && x <= 2022) || (x <= 22)) year 
--satisfy (\x -> (x <= 12)) month
--data Event a = Calendar a | Event a| Todo a
--data Datetime = Dt Integer Integer Integer deriving (Show, Eq, Ord)
<!-- type TParser = Parsec Void String
type ShParser = Parsec Void Text
type MyStack a = StateT Day TParser a -->
<!-- pBegin :: IO Text
pBegin = pure "BEGIN:"

pEND :: IO Text
pEND = pure "END:"

pSemiC :: Text -> Text
pSemiC = intersperse ';' -->

-- getInterval :: MParser Interval 
-- getInterval = do a <- takedigitChar
--                  return (Interval (Just (read a :: Integer)))

