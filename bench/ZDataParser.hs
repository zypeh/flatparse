module ZDataParser (runSexp, runLongws, runNumcsv) where

import qualified Z.Data.ASCII  as C
import qualified Z.Data.Parser as P

import Control.Monad (when)

ws = P.skipWhile (\w -> w == C.SPACE || w == C.NEWLINE)
-- open = P.char8 '(' >> ws
-- close = P.char8 ')' >> ws
ident = P.skipWhile (\w -> C.isUpper w || C.isLower w) >> ws
sexp = do
    w <- P.peek
    if w == C.PAREN_LEFT
        then P.skipWord8 >> sexp
        else ident

src = sexp >> P.endOfInput
runSexp = P.parseChunk sexp

longw = P.bytes "thisisalongkeyword"
longws = P.peek >>= \w -> when (w == C.LETTER_t) (longw >> ws)
runLongws = P.parseChunk longws

numeral = P.skipWhile C.isDigit >> ws
-- comma = P.char8 ',' >> ws
numcsv = do
    numeral
    w <- P.peek
    when (w == C.COMMA) $ P.skipWord8 >> ws >> numcsv

runNumcsv = P.parseChunk numcsv