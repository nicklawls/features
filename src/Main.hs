module Main where
import           Control.Monad.State
import           Data.List           (sortBy, (\\))
import           Data.Matrix         ((<->), (<|>))
import qualified Data.Matrix         as M
import           Data.Monoid         ((<>))
import           Data.Ord            (comparing)
import           Data.Random         (sampleState, shuffleNofM)
import           Data.Text           (Text, unpack)
import qualified Data.Text.IO        as T (readFile)
import qualified Data.Vector         as V
import           Statistics.Sample   (mean, stdDev)
import           System.Environment  (getArgs)
import           System.IO           (hFlush, stdout)
import           System.Random       (mkStdGen)


{- massage the big string into a good dataset
-}

parse :: String -> M.Matrix Double
parse =
    M.fromLists . map (map read . words) . lines


{- Split the initial matrix into a vector of labels and a matrix of features
-}

splitData :: M.Matrix Double -> (V.Vector Double, M.Matrix Double)
splitData m =
    ( M.getCol 1 m
    , M.submatrix 1 (M.nrows m) 2 (M.ncols m) m
    )


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

{- Z normalize the matrix one column at a time
-}

normalize :: M.Matrix Double -> M.Matrix Double
normalize =
    let normalizeCol col = V.map (\x -> (x - mean col) / stdDev col ) col
    in V.foldl1' (<|>) . V.map (M.colVector . normalizeCol) . toCols


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
    let dataset = M.forceMatrix (normalize rawDataset)
    putStrLn "Done!"
    let algo = case algorithm of
                    "1" -> forwardSelection
                    "2" -> backwardElimination
                    "3" -> specialAlgorithm
                    _ -> error "Choose a proper algorithm!"
    putStrLn $ "Running nearest neighbor with all "
            ++ show (M.ncols dataset)
            ++ " features, using “leaving-one-out” evaluation, I get an accuracy of "
            ++ show (leaveOneOutCV [1.. M.ncols dataset] labels dataset)
            ++ "%\n"
    putStrLn "Beginning Search\n"
    (accuracy,featureSet) <- algo labels dataset
    putStrLn $ "Finished Search!! The best feature subset is "
            ++ show featureSet
            ++ ", which has an accuracy of "
            ++ show accuracy
            ++ "%"

{- The full cross validation pipeline

   After computing the nearest neighbor for each validation run, pair them with
   the true labels, count the number of times you computed the correct one and
   return the proportion of correct classifications
-}

leaveOneOutCV :: [Int]           -- features to consider
              -> V.Vector Double -- labels
              -> M.Matrix Double -- dataset
              -> Double          -- the percentage of computed labels that matched the true lable
leaveOneOutCV features labels dataset =
    let correctlyLabled = V.length
                        $ V.filter (== True)
                        $ V.zipWith (==) labels
                        $ allNearestNeighbors labels (getCols (V.fromList features) dataset)
    in 100 * (fromIntegral correctlyLabled / fromIntegral (M.nrows dataset))


{- Picks out the columns of 'matrix' specified by their 1 based indices in the array cols
-}

getCols :: V.Vector Int -> M.Matrix a -> M.Matrix a
getCols cols matrix =
    if   V.null cols then M.fromLists [[]]
    else V.foldl1' (<|>) $ V.map (M.colVector . (`M.getCol` matrix)) cols

{- Apply nearest neighbor to all (true label, example, training set) permutations
-}

allNearestNeighbors :: V.Vector Double -> M.Matrix Double -> V.Vector Double
allNearestNeighbors labels dataset =
    V.zipWith3 nearestNeighbor (leaveOneOutVec labels) (toRows dataset) (leaveOneOut dataset)


{- Returns a vector containing all possible removals of a single element from
   the argument vector
-}

leaveOneOutVec :: V.Vector a -> V.Vector (V.Vector a)
leaveOneOutVec vec =
    V.map (removeElement vec) (V.enumFromN 0 (V.length vec))


{- Returns a vector equivalent to the argument matrix except that 0-based
   element i has been removed
-}

removeElement :: V.Vector a -> Int -> V.Vector a
removeElement vec i =
    let (top, bottom) = V.splitAt i vec
    in  top <> V.slice 1 (V.length bottom - 1) bottom


{- Returns a vector containing all possible removals of a single row from the
   argument matrix
-}

leaveOneOut :: M.Matrix a -> V.Vector (M.Matrix a)
leaveOneOut matrix =
    V.map (removeRow matrix) (V.enumFromN 1 (M.nrows matrix))


{- Returns a copy of the argument matrix missing the 1-indexed row provided as
   the second argumetn
-}

removeRow :: M.Matrix a -> Int -> M.Matrix a
removeRow matrix rowNum
    | rowNum == 1              = rowsBelow
    | rowNum == M.nrows matrix = rowsAbove
    | otherwise                = rowsAbove <-> rowsBelow
    where rowsAbove = M.submatrix 1 (rowNum - 1) 1 (M.ncols matrix) matrix
          rowsBelow = M.submatrix (rowNum+1) (M.nrows matrix) 1 (M.ncols matrix) matrix


{- The core nearest neighbor algorithm.

   After turning the dataset into a vector of rows, it calculates the euclidian
   distance between each row and the example, pairs that distance with the
   provided label, then extracts the label of the minimum-scoring example

-}
nearestNeighbor :: V.Vector Double -- the labels
                -> V.Vector Double -- the example to be classified
                -> M.Matrix Double -- the dataset
                -> Double          -- the computed label
nearestNeighbor labels example =
      fst
    . V.minimumBy (comparing snd)
    . V.zip labels
    . V.map (euclidianDistance example)
    . toRows

{- Calculate the euclidian distance between two numeric vectors
-}

euclidianDistance :: Num a => V.Vector a -> V.Vector a -> a
euclidianDistance v1 v2 =
      V.sum (V.map (^(2 :: Int)) (V.zipWith (-) v1 v2))


{- Represent a matrix as a vector of row vectors
-}

toRows :: M.Matrix a -> V.Vector (V.Vector a)
toRows matrix =
    V.map (`M.getRow` matrix) (V.enumFromN 1 (M.nrows matrix))


{- Represent a matrix as a vector of Column vectors
-}

toCols :: M.Matrix a -> V.Vector (V.Vector a)
toCols = toRows . M.transpose


{- It turns out that the vast majority of the code for the forward and backward
   searches is essentially the same. We can reuse the main "featureSearch" section
   by abstracting out a function to choose which features are examined in the
   inner loop, another function to determine whether we add or remove a feature
   on each iteration, and the inital set of features.
-}

featureSearch :: ([Int] -> [Int] -> [Int]) -- how to choose which features to consider
              -> ([Int] -> [Int] -> [Int]) -- how to add the next feature
              -> [Int]                     -- starting features
              -> V.Vector Double
              -> M.Matrix Double
              -> IO (Double, [Int])
featureSearch choose add initial labels dataset = do
    (_,_,acc,features) <- execStateT computeFeatures (50,initial,50,initial)
    return (acc,features)
    where
        -- Haskell forces us to be clear about our intention to use 4 mutable variables
        -- even then, its only a functional simulation of mutable state
        computeFeatures :: StateT (Double, [Int], Double, [Int]) IO ()
        computeFeatures =
            forM_ [1.. M.ncols dataset] $ \_ -> do
                (lastBestAcc, lastBestFeatures, bestAcc, bestFeatures) <- get
                let featuresToConsider = [1.. M.ncols dataset] `choose` lastBestFeatures
                accSets <- forM featuresToConsider $ \k -> do
                            let candidateSet = lastBestFeatures `add` [k]
                                (accuracy,set) = case candidateSet of
                                                [] -> (leaveOneOutCV [1..M.ncols dataset] labels dataset,[1..M.ncols dataset])
                                                _  -> (leaveOneOutCV candidateSet labels dataset,candidateSet)
                            putStrLnM ("       Using feature(s) " ++ show set ++ " accuracy is " ++ show accuracy ++ "%")
                            return (accuracy, set)
                let (newAcc, newFeatures) = maximum accSets -- maximum looks at left entry of tuple
                putStrLnM  ""
                when (newAcc < lastBestAcc) $ -- possible corner case when equal
                    putStrLnM "(Warning, Accuracy has decreased! Continuing search in case of local maxima)"
                putStrLnM ("Feature set " ++ show newFeatures ++ " was best, accuracy " ++ show newAcc ++ "%\n")
                put (newAcc, newFeatures, max newAcc bestAcc, if newAcc > bestAcc then newFeatures else bestFeatures)


-- to choose the features considered, we remove the ones we've already looked at
forwardSelection :: V.Vector Double -> M.Matrix Double -> IO (Double, [Int])
forwardSelection = featureSearch (\\) (++) []

-- we're only interested in the features that are left after the last iteration
-- so we use `seq`, which always returns its second argument.
backwardElimination :: V.Vector Double -> M.Matrix Double -> IO (Double,[Int])
backwardElimination labels dataset = featureSearch seq (\\) [1..M.ncols dataset] labels dataset

{- in each inner loop, try out the top 3 scoring feature subsets on the full dataset.
   pick the one with the highest score and maintain that score going forward
-}
specialAlgorithm :: V.Vector Double -> M.Matrix Double -> IO (Double,[Int])
specialAlgorithm labels dataset = do
    putStrLn "Taking a random sample of the original dataset"
    let sampleSize = M.nrows dataset `div` 10
    (_,_,acc,features) <- execStateT  (uncurry computeFeatures (randomSample sampleSize labels dataset)) (50,[],50,[])
    return (acc,features)
    where
        computeFeatures :: V.Vector Double -> M.Matrix Double -> StateT (Double, [Int], Double, [Int]) IO ()
        computeFeatures labels' dataset' =
            forM_ [1.. M.ncols dataset'] $ \_ -> do
                (lastBestAcc, lastBestFeatures, bestAcc, bestFeatures) <- get
                let featuresToConsider = [1.. M.ncols dataset'] \\ lastBestFeatures
                accSets <- forM featuresToConsider $ \k -> do
                            let candidateSet = lastBestFeatures ++ [k]
                                (accuracy,set) = case candidateSet of
                                                [] -> (leaveOneOutCV [1..M.ncols dataset'] labels' dataset',[1..M.ncols dataset'])
                                                _  -> (leaveOneOutCV candidateSet labels' dataset',candidateSet)
                            putStrLnM ("       Using feature(s) " ++ show set ++ " accuracy is " ++ show accuracy ++ "%")
                            return (accuracy, set)
                let accSets' = topNOnFullDataset 3 labels dataset accSets
                    (newAcc, newFeatures) = maximum accSets' -- maximum looks at left entry of tuple
                putStrLnM  ""
                when (newAcc < lastBestAcc) $ -- possible corner case when equal
                    putStrLnM "(Warning, Accuracy has decreased! Continuing search in case of local maxima)"
                putStrLnM ("Feature set " ++ show newFeatures ++ " was best, full dataset accuracy " ++ show newAcc ++ "%\n")
                put (newAcc, newFeatures, max newAcc bestAcc, if newAcc > bestAcc then newFeatures else bestFeatures)

{- Compute the full-dataset validation score of the features subsets that scored
   the best on the reduced-size dataset
-}

topNOnFullDataset :: Int -> V.Vector Double -> M.Matrix Double -> [(Double,[Int])] -> [(Double,[Int])]
topNOnFullDataset n labels dataset =
    let scoreWithFeatures labels' dataset' (_,features) = (leaveOneOutCV features labels' dataset', features)
    in map (scoreWithFeatures labels dataset) . take n . sortBy (flip compare)


{- Return a random n-length subset of the supplied label vector and dataset matrix
-}

randomSample :: Int -> V.Vector Double -> M.Matrix Double -> (V.Vector Double, M.Matrix Double)
randomSample n labels dataset =
    let (labels',dataset') = unzip . fst $ sampleState (shuffleNofM n (M.nrows dataset) (zip (V.toList labels) (M.toLists dataset))) (mkStdGen 366)
    in (V.fromList labels', M.fromLists dataset')




-- quick dataset access in repl
small34 :: IO Text
small34 = T.readFile "data/cs_170_small34.txt"


large34 :: IO Text
large34 = T.readFile "data/cs_170_large34.txt"


small80 :: IO Text
small80 = T.readFile "data/cs_170_small80.txt"


large80 :: IO Text
large80 = T.readFile "data/cs_170_large34.txt"


-- easy printing in IO Monads
putStrLnM :: MonadIO m => String -> m ()
putStrLnM = liftIO . putStrLn
