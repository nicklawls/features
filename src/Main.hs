module Main where
import           Control.Monad      (forM_)
import qualified Data.Matrix        as M
import           Data.Text          (unpack)
import qualified Data.Text.IO       as T (readFile)
import qualified Data.Vector        as V
import           System.Environment (getArgs)
import           System.IO          (hFlush, stdout)
import Data.List ((\\))

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
            putStrLn "Filename and Algorithm provided at command line\n"
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
            putStrLn "        3) Nick’s Special Algorithm."
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
    let (labels, rawDataset) = splitData (parse (unpack raw))
    putStrLn $
          "This dataset has "
          ++ show (M.ncols rawDataset)
          ++ " features (not including the class attribute) with "
          ++ show (M.nrows rawDataset)
          ++ " instances.\n"
    putStr "Please wait while I normalize the data... "
    hFlush stdout
    let dataset = M.forceMatrix (normalize rawDataset) -- might have to use seq
    putStrLn "Not Implemented!\n"
    let algo = case algorithm of
                    "1" -> forwardSelection
                    "2" -> backwardElimination
                    "3" -> error "Extra algorithm not implemented"
                    _ -> error "Choose a proper algorithm!"
    putStrLn $ "Running nearest neighbor with all "
            ++ show (M.ncols dataset)
            ++ " features, using “leaving-one-out” evaluation, I get an accuracy of "
            ++ show (leaveOneOutCV [1.. M.ncols dataset] labels dataset)
            ++ "%\n"
    putStrLn "Beginning Search\n"
    (accuracy, featureSet) <- algo `seq` algo labels dataset
    putStrLn $ "Finished Search!! The best feature subset is "
            ++ show featureSet
            ++ ", which has an accuracy of "
            ++ show accuracy
            ++ "%"

-- take the labels and the dataset, produce and optimal set of features
-- labels and features stay in scope all the way down, haskell really is the best imperative language!
forwardSelection :: V.Vector Float -> M.Matrix Float -> IO (Float,[Int])
forwardSelection labels dataset = do
    forM_ [1.. M.ncols dataset] $ \i -> do
        putStrLn $ "On the " ++ show i ++ "th level of the search tree"
        forM_ [1.. M.ncols dataset] $ \k -> do
            putStrLn $ "--Considering adding the " ++ show k ++ " feature"
    return (0,[1,4,5])

-- [(The search depth i, max accuracy at that depth, set of features that got you that accuracy)]
-- [(The kth feature, accuracy, set of features that got you that accuracy)]
-- separate the computaion out from the pretty printing?

-- TODO Change return type to ((Float,[Int]),(Float,[Int])) to keep track of the best result for each level
-- Might have to canibalize this for parts to put into the imperative version above
step :: (Float, [Int]) -- the previous best accuracy and feature set so far
     -> V.Vector Float -- the labels
     -> M.Matrix Float -- the Data
     -> (Float, [Int]) -- the new best accuracy and feature set
step (bestAcc,bestFeatures) labels dataset =
    let featuresToConsider = [1..M.ncols dataset] \\ bestFeatures
        candidateFeatureSets = map (flip (:) bestFeatures) featuresToConsider
    in max (bestAcc,bestFeatures) $
        maximum $ map (\features -> (leaveOneOutCV features labels dataset, features)) candidateFeatureSets

leaveOneOutCV :: [Int] -> V.Vector Float -> M.Matrix Float -> Float
leaveOneOutCV _ _ _ = 0

backwardElimination :: V.Vector Float -> M.Matrix Float -> IO (Float,[Int])
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
