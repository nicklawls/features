module Main where
import           Control.Monad       (forM_, when)
import           Control.Monad.State
import           Data.List           ((\\))
import qualified Data.Matrix         as M
import           Data.Text           (unpack)
import qualified Data.Text.IO        as T (readFile)
import qualified Data.Vector         as V
import           System.Environment  (getArgs)
import           System.IO           (hFlush, stdout)

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
    (_, (_,_,accuracy,featureSet)) <- runStateT (algo labels dataset) (50,[],90,[11,11])
    putStrLn $ "Finished Search!! The best feature subset is "
            ++ show featureSet
            ++ ", which has an accuracy of "
            ++ show accuracy
            ++ "%"

-- TODO push all the state stuff into the body of forwardSelection
forwardSelection :: V.Vector Float -> M.Matrix Float -> StateT (Float, [Int], Float, [Int]) IO ()
forwardSelection labels dataset =
    forM_ [1.. M.ncols dataset] $ \i -> do
        (lastBestAcc, lastBestFeatures, bestAcc, bestFeatures) <- get
        let featuresToAdd = [1.. M.ncols dataset] \\ lastBestFeatures
        accSets <- forM featuresToAdd $ \k -> do
                        let candidateSet = lastBestFeatures ++ [k]
                            accuracy = leaveOneOutCV candidateSet labels dataset
                        liftIO . putStrLn $ "       Using feature(s) "
                                         ++ show candidateSet
                                         ++ " accuracy is "
                                         ++ show accuracy ++ "%"
                        return (accuracy, candidateSet)
        let (newAcc, newFeatures) = maximum accSets
        liftIO $ putStrLn ""
        when (newAcc < lastBestAcc) $ -- possible corner case when equal
            liftIO $ putStrLn "(Warning, Accuracy has decreased! Continuing search in case of local maxima)"
        liftIO . putStrLn $ "Feature set " ++ show newFeatures ++ " was best, accuracy " ++ show newAcc ++ "%\n"
        put (newAcc, newFeatures, max newAcc bestAcc, max newFeatures bestFeatures)


leaveOneOutCV :: [Int] -> V.Vector Float -> M.Matrix Float -> Float
leaveOneOutCV _ _ _ = 50

backwardElimination :: V.Vector Float -> M.Matrix Float -> StateT (Float,[Int],Float,[Int]) IO ()
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
