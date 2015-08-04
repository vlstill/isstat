module Main ( main ) where

import IS
import Stat
import System.Environment
import Text.Printf
import Text.Read

main = do
    args <- getArgs
    case args of
        [zn, bl] -> process passedByPtsAcc zn bl
        [opt, zn, bl] -> case opt of
                            "ptacc" -> process passedByPtsAcc zn bl
                            "pt"    -> process passedByPts zn bl
                            _ -> usage
        [gr, n, zn, bl] -> case (gr, readMaybe n) of
                            ("gracc", Just n) -> process (passedByPtsGrAcc n) zn bl
                            ("gr", Just n)    -> process (passedByPtsGr n) zn bl
                            _ -> usage
        _ -> usage
  where
    usage = error "usage: [ pt | ptacc | gr <N> | gracc <N> ] <marks> <notebook>"
    process f zn bl = putStrLn =<< table <$> (f <$> (passedStudents <$> readFile zn) <*> (parseNb <$> readFile bl))

table :: [ (Points, Int, Int, Double) ] -> String
table = unlines . map line
  where
    line (pts, tot, succ, pro) = concat [show pts, "\t", show succ, "/", show tot, " (", printf "%0.1f" (pro * 100), " %)"]
