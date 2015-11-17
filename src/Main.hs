module Main where
import           Control.Monad      (forM_)
import qualified Data.Matrix        as M
import           Data.Text          (unpack)
import qualified Data.Text.IO       as T (readFile)
import qualified Data.Vector        as V
import           System.Environment (getArgs)
import System.IO (hFlush, stdout)


-- massage the big string into a good dataset
parse :: String -> M.Matrix Float
parse =
    M.fromLists . map (map read . words) . lines


splitData :: M.Matrix Float -> (V.Vector Float, M.Matrix Float)
splitData m =
    ( M.getCol 1 m
    , M.submatrix 1 (M.nrows m) 2 (M.ncols m) m
    )

-- TODO if length args == 2, skip over prompt, else query user

prompt :: IO (String, String) -- The filepath and the algorithm number
prompt = do
    args <- getArgs
    putStrLn "Welcome to Nick Lawler's Feature Selection Algorithm"
    case args of
        [filename, algorithm] -> do
            putStrLn "Filename and Algorithm provided at command line"
            return (filename,algorithm)
        _ -> do
            putStr "Type in the name of the file to test : "
            hFlush stdout
            filename <- getLine
            putStrLn ""
            putStrLn "Type the number of the algorithm you want to run."
            putStrLn ""
            putStrLn "        1) Forward Selection"
            putStrLn "        2) Backward Elimination"
            putStrLn "        3) Bertie’s Special Algorithm."
            putStr "                                    "
            hFlush stdout
            algorithm <- getLine
            return (filename,algorithm)

normalize :: M.Matrix Float -> M.Matrix Float
normalize = id -- TODO Implement

main :: IO ()
main = do
    (filename, algorithm) <- prompt
    raw <- T.readFile filename
    let (labels, rawFeatures) = splitData (parse (unpack raw))
    putStrLn $
          "This dataset has "
          ++ show (M.ncols rawFeatures)
          ++ " features (not including the class attribute) with "
          ++ show (M.nrows rawFeatures)
          ++ " instances."
    putStr "Please wait while I normalize the data... "
    hFlush stdout
    let features = M.forceMatrix (normalize rawFeatures) -- might have to use seq
    putStrLn "Not Implemented!"
    let algo = case algorithm of
                    "1" -> forwardSelection
                    "2" -> backwardElimination
                    "3" -> error "Extra algorithm not implemented"
                    _ -> error "Choose a proper algorithm!"
    algo labels features

forwardSelection :: V.Vector Float -> M.Matrix Float -> IO ()
forwardSelection = undefined

backwardElimination :: V.Vector Float -> M.Matrix Float -> IO ()
backwardElimination = undefined

-- quick dataset access in repl
small34 :: IO String
small34 = Prelude.readFile "data/cs_170_small34.txt"

large34 :: IO String
large34 = Prelude.readFile "data/cs_170_large34.txt"

small80 :: IO String
small80 = Prelude.readFile "data/cs_170_small80.txt"

large80 :: IO String
large80 = Prelude.readFile "data/cs_170_large34.txt"
