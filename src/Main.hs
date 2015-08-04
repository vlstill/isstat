module Main ( main ) where

import IS
import Stat
import Data.Char
import System.Environment
import Text.Printf
import Text.Read
import Control.Applicative

main = do
    args <- getArgs
    case args of
        [zn, bl] -> process passedByPtsAcc "all" zn bl
        [rep, opt, zn, bl] -> case opt of
                            "ptacc" -> process passedByPtsAcc rep zn bl
                            "pt"    -> process passedByPts rep zn bl
                            _ -> usage
        [rep, gr, n, zn, bl] -> case (gr, readMaybe n) of
                            ("gracc", Just n) -> process (passedByPtsGrAcc n) rep zn bl
                            ("gr", Just n)    -> process (passedByPtsGr n) rep zn bl
                            _ -> usage
        _ -> usage
  where
    usage = error "usage: [all | norepeat | repeat] [ pt | ptacc | gr <N> | gracc <N> ] <marks> <notebook>"
    process f (r:rs) zn bl = putStrLn =<< table <$> (f <$> (passedStudents rep <$> readFile zn) <*> (parseNb rep <$> readFile bl))
      where
        rep = read (toUpper r : rs)

table :: [ (Points, Int, Int, Double) ] -> String
table = unlines . map line
  where
    line (pts, tot, succ, pro) = printf ">= %4.1f  %d/%d (%0.1f %%)" pts succ tot (pro * 100)
