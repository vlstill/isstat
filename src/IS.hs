module IS where

import Util
import Data.Generics.Text

import Control.Arrow
import Control.Applicative
import Data.Char
import Data.List
import Data.Ord
import Data.Data ( Data )
import Data.Typeable ( Typeable )

data Zk = A | B | C | D | E | F | Missed
        deriving ( Show, Read, Enum )

class Result a where
    passed :: a -> Bool
    parseRes :: String -> a
    printRes :: a -> String

instance Result Zk where
    passed F = False
    passed Missed = False
    passed _ = True

    parseRes "-" = Missed
    parseRes x   = read x

    printRes Missed = "-"
    printRes x      = show x

instance Result a => Result [a] where
    passed [] = False
    passed xs = passed (last xs)

    parseRes = map (parseRes . (:[]))
    printRes = concatMap printRes

type Uco = Int
type Points = Double

data Repeat = All | Norepeat | Repeat
            deriving ( Show, Read )

extract :: Repeat -> ([String] -> a) -> String -> [a]
extract r f = lines >>> map separate >>> filterRep r >>> map f

filterRep :: Repeat -> [[String]] -> [[String]]
filterRep r = case r of
    All    -> id
    Norepeat   -> filter (not . opak)
    Repeat -> filter opak
  where
    opak :: [String] -> Bool
    opak = (!! 4) >>> ("opak" `isInfixOf`)

parseMarks :: Repeat -> String -> [ (Uco, [Zk]) ]
parseMarks r = extract r (head &&& (!! 8)  >>> read *** parseRes)

passedStudents :: Repeat -> String -> [Uco]
passedStudents r = parseMarks r >>> map (second passed) >>> filter snd >>> map fst

parsePts :: String -> Points
parsePts = dropWhile (/= '*') >>> separateBy '*' >>> filter (any isDigit) >>>
           map (takeWhile ((||) <$> isDigit <*> (== '.')) >>> fix >>> read) >>> sum
  where fix ('.':xs) = "0." ++ xs
        fix x = x

parseNb :: Repeat -> String -> [ (Uco, Points) ]
parseNb r = extract r (head &&& (!! 7) >>> read *** parsePts) >>> sortBy (comparing snd)
