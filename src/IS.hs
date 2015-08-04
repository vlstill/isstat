{-# LANGUAGE DeriveDataTypeable #-}

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
        deriving ( Show, Read, Enum, Data, Typeable )

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

extract :: ([String] -> a) -> String -> [a]
extract f = lines >>> map (separate >>> f)

parseMarks :: String -> [ (Uco, [Zk]) ]
parseMarks = extract (head &&& (!! 8) >>> read *** parseRes)

passedStudents :: String -> [Uco]
passedStudents = parseMarks >>> map (second passed) >>> filter snd >>> map fst

parsePts :: String -> Points
parsePts = dropWhile (/= '*') >>> separateBy '*' >>> filter (any isDigit) >>>
           map (takeWhile ((||) <$> isDigit <*> (== '.')) >>> read) >>> sum

parseNb :: String -> [ (Uco, Points) ]
parseNb = extract (head &&& (!! 7) >>> read *** parsePts) >>> sortBy (comparing snd)
