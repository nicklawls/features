module Main where
import           Control.Monad      (forM_)
import qualified Data.Matrix        as M
import           Data.Text          (unpack)
import qualified Data.Text.IO       as T (readFile)
import qualified Data.Vector        as V
import           System.Environment (getArgs)


-- massage the big string into a good dataset
parse :: String -> M.Matrix Float
parse = M.fromLists . map (map read . words) . lines

main :: IO ()
main = do
    [file] <- getArgs -- will crash if more than one file
    raw <- T.readFile file
    let dataset = parse (unpack raw)
    print dataset


-- quick dataset access in repl
small34 :: IO String
small34 = Prelude.readFile "data/cs_170_small34.txt"

large34 :: IO String
large34 = Prelude.readFile "data/cs_170_large34.txt"

small80 :: IO String
small80 = Prelude.readFile "data/cs_170_small80.txt"

large80 :: IO String
large80 = Prelude.readFile "data/cs_170_large34.txt"
