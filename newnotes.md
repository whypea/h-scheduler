"employee" a different category of task?
    Routines vs 
"preference" as a way to encode deadlines?
    sooner/more urgent


"machines" as locations/categories, with extra time given for travel?

input
    either a new user-generated ics
    or a file to be modified
input is parsed (newFile ) into Data.Time (Day/time), Timezone
    what do I do with a parser/megaparsec/optparse?
Day/time will be distributed 


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
    -Ordered list -> Deterministic state shop
    -Prioritized list -> 
    -Unordred list
-- every

-- https://datatracker.ietf.org/doc/html/rfc5545#section-3.3
--Make a parser for this
--Content lines are delimited by a line break, CRLF sequence
--File must include (in contentlines) 
--recurrance rrule
--"contentline   = name *(";" param ) ":" value CRLF" 
    -- name = iana-token / x-name
    -- iana-token    = 1*(ALPHA / DIGIT / "-")

cli should print the 


--shorthands (with try)
    -- Mon(day)    -> closest monday
    -- 0103        -> closest 1st of march
    --"in a year"  -> from today
    -- 3am (mon)   -> closest (monday)
    -- 5 times     -> frequency  
    -- asap?       -> check other events, category


--TODO:
-- set defaults
-- options
----datetime format
----multiple events
----recurrance
----type of events
