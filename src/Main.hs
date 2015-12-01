module Main where
import           Control.Monad.State
import           Data.List           (sortBy, (\\))
import           Data.Matrix         ((<->), (<|>))
import qualified Data.Matrix         as M
import           Data.Monoid         ((<>))
import           Data.Text           (Text, unpack)
import qualified Data.Text.IO        as T (readFile)
import qualified Data.Vector         as V
import           System.Environment  (getArgs)
import           System.IO           (hFlush, stdout)
import           Statistics.Sample   (mean, stdDev)
import           Data.Ord            (comparing)
import           Safe                (headMay)

-- massage the big string into a good dataset
parse :: String -> M.Matrix Double
parse =
    M.fromLists . map (map read . words) . lines


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
    -- TODO put a "Done!" in the trace, or ya know, actually figure this out
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
    (_, (_,_,accuracy,featureSet)) <- runStateT (algo labels dataset) (50,[],50,[])
    putStrLn $ "Finished Search!! The best feature subset is "
            ++ show featureSet
            ++ ", which has an accuracy of "
            ++ show accuracy
            ++ "%"


leaveOneOutCV :: [Int]          -- features to consider
              -> V.Vector Double -- labels
              -> M.Matrix Double -- dataset
              -> Double          -- the percentage of computed labels that matched the true lable
leaveOneOutCV features labels dataset =
    let correctlyLabled = V.length
                        $ V.filter (== True)
                        $ V.zipWith (==) labels
                        $ score labels (getCols (V.fromList features) dataset)
    in 100 * (fromIntegral correctlyLabled / fromIntegral (M.nrows dataset))



-- picks out the columns of 'matrix' specified by their 1 based indices in the array cols
getCols :: V.Vector Int -> M.Matrix a -> M.Matrix a
getCols cols matrix =
    if   V.null cols then M.fromLists [[]]
    else V.foldl1' (<|>) $ V.map (M.colVector . (`M.getCol` matrix)) cols


-- TODO Intead of passing around a vector of matricies with the rows removed,
-- iterate over a vector of vectors of indices to look at
score :: V.Vector Double -> M.Matrix Double -> V.Vector Double
score labels dataset =
    V.zipWith3 nearestNeighbor (leaveOneOutVec labels) (toRows dataset) (leaveOneOut dataset)


leaveOneOutVec :: V.Vector a -> V.Vector (V.Vector a)
leaveOneOutVec vec =
    V.map (removeElement vec) (V.enumFromTo 0 (V.length vec - 1))


-- 0 based indexing
removeElement :: V.Vector a -> Int -> V.Vector a
removeElement vec i =
    let (top, bottom) = V.splitAt i vec
    in  top <> V.slice 1 (V.length bottom - 1) bottom


leaveOneOut :: M.Matrix a -> V.Vector (M.Matrix a)
leaveOneOut matrix =
    V.map (removeRow matrix) (V.enumFromTo 1 (M.nrows matrix))


-- rowNum refers to the 1 based indexing of rows
removeRow :: M.Matrix a -> Int -> M.Matrix a
removeRow matrix rowNum
    | rowNum == 1              = rowsBelow
    | rowNum == M.nrows matrix = rowsAbove
    | otherwise                = rowsAbove <-> rowsBelow
    where rowsAbove = M.submatrix 1 (rowNum - 1) 1 (M.ncols matrix) matrix
          rowsBelow = M.submatrix (rowNum+1) (M.nrows matrix) 1 (M.ncols matrix) matrix





nearestNeighbor :: V.Vector Double -- the labels
                -> V.Vector Double -- the example to be classified
                -> M.Matrix Double -- the dataset
                -> Double          -- the computed label
nearestNeighbor labels example dataset =
    maybe (error "catastrophic failure") fst $
          headMay
        $ sortBy (comparing snd)
        $ V.toList
        $ V.zip labels
        $ V.map (euclidianDistance example) (toRows dataset)


euclidianDistance :: Num a => V.Vector a -> V.Vector a -> a
euclidianDistance v1 v2 =
      V.sum (V.map (^(2 :: Int)) (V.zipWith (-) v1 v2))



toRows :: M.Matrix a -> V.Vector (V.Vector a)
toRows matrix =
    V.map (`M.getRow` matrix) (V.enumFromTo 1 (M.nrows matrix))


toCols :: M.Matrix a -> V.Vector (V.Vector a)
toCols = toRows . M.transpose






forwardSelection :: V.Vector Double -> M.Matrix Double -> StateT (Double, [Int], Double, [Int]) IO ()
forwardSelection labels dataset =
    forM_ [1.. M.ncols dataset] $ \_ -> do
        (lastBestAcc, lastBestFeatures, bestAcc, bestFeatures) <- get
        let featuresToAdd = [1.. M.ncols dataset] \\ lastBestFeatures -- (\\) is set difference
        accSets <- forM featuresToAdd $ \k -> do
                    let candidateSet = lastBestFeatures ++ [k]
                        accuracy = leaveOneOutCV candidateSet labels dataset
                    putStrLnM ("       Using feature(s) " ++ show candidateSet ++ " accuracy is " ++ show accuracy ++ "%")
                    return (accuracy, candidateSet)
        let (newAcc, newFeatures) = maximum accSets -- maximum looks at left entry of tuple
        putStrLnM  ""
        when (newAcc < lastBestAcc) $ -- possible corner case when equal
            putStrLnM "(Warning, Accuracy has decreased! Continuing search in case of local maxima)"
        putStrLnM ("Feature set " ++ show newFeatures ++ " was best, accuracy " ++ show newAcc ++ "%\n")
        put (newAcc, newFeatures, max newAcc bestAcc, if newAcc > bestAcc then newFeatures else bestFeatures)

-- if I remember correctly, there was some ambiguity about which results to compare when you're continuing the search after a decrease




backwardElimination :: V.Vector Double -> M.Matrix Double -> StateT (Double,[Int],Double,[Int]) IO ()
backwardElimination = undefined

-- quick dataset access in repl
small34 :: IO Text
small34 = T.readFile "data/cs_170_small34.txt"


large34 :: IO Text
large34 = T.readFile "data/cs_170_large34.txt"


small80 :: IO Text
small80 = T.readFile "data/cs_170_small80.txt"


large80 :: IO Text
large80 = T.readFile "data/cs_170_large34.txt"


putStrLnM :: MonadIO m => String -> m ()
putStrLnM = liftIO . putStrLn
