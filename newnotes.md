"employee" a different category of task?
    Routines vs 
"preference" as a way to encode deadlines?
    sooner/more urgent


"machines" as locations/categories, with extra time given for travel?

input
    either a new user-generated ics
    or a file to be modifie
    options
        asking for more options if they aren't filled out?
        defaults for options?
        be verbose?

input is parsed (newFile ) into Data.Time (Day/time), Timezone
    what do I do with a parser/megaparsec/optparse?
Day/time will be distributed by the solver, 
    SOLVER IS THE MOST DIFFICULT PART OF THIS



icalendar file printing options
    - multiple events in one call
    - repeating rule
    - 
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

-- https://datatracker.ietf.org/doc/html/rfc5545#section-3.3
--Make a parser for this
--Content lines are delimited by a line break, CRLF sequence
--File must include (in contentlines) 
--recurrance rrule
--"contentline   = name *(";" param ) ":" value CRLF" 
    -- name = iana-token / x-name
    -- iana-token    = 1*(ALPHA / DIGIT / "-")

cli should print an entire calendar/change one


--shorthands (with try)
    -- Mon(day)    -> closest monday
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
----recurrance
----type of events
