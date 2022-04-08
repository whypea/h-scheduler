
module Events where
import Text.Megaparsec 
import Text.Megaparsec.String

data Event a = Calendar a | Event a| Todo a  

data Date d m s = D d m s   
 
data Time h m s = T {h :: Int, m :: Int, s :: Int}

type Parser = Parsec 
-- https://datatracker.ietf.org/doc/html/rfc5545#section-3.3
--Make a parser for this
--Content lines are delimited by a line break, CRLF sequence
--File must include (in contentlines) 
--recurrance rrule
--"contentline   = name *(";" param ) ":" value CRLF" 
    -- name = iana-token / x-name
    -- iana-token    = 1*(ALPHA / DIGIT / "-")
    -- 
 p