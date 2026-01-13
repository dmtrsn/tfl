module Main where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, array, (!))
import Data.Array.ST (STArray, newArray, readArray, writeArray, getBounds, getElems)
import Data.Bits ((.&.), shiftL, countTrailingZeros)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

alphabetOk :: String -> Bool
alphabetOk = all (`elem` ("ab" :: String))

isPowerOfTwo :: Int -> Bool
isPowerOfTwo x = x > 0 && (x .&. (x - 1)) == 0

relTT :: Int -> Int -> Int -> Bool
relTT kMin v1 v2
  | v2 <= 0          = False
  | v1 `mod` v2 /= 0 = False
  | otherwise =
      let q = v1 `div` v2
      in isPowerOfTwo q && countTrailingZeros q >= kMin

freeze2D :: STArray s (Int,Int) a -> ST s (Array (Int,Int) a)
freeze2D st = do
  bnds <- getBounds st
  elems <- getElems st
  let ((i0,j0),(i1,j1)) = bnds
      idxs = [ (i,j) | i <- [i0..i1], j <- [j0..j1] ]
  pure $ array bnds (zip idxs elems)

inLanguageNaive :: String -> Int -> Bool
inLanguageNaive word kMin
  | null word = False
  | not (alphabetOk word) = False
  | otherwise =
      let (dpS, _dpT) = buildTablesNaive word kMin
      in not . Set.null $ dpS ! (0, length word)

buildTablesNaive :: String -> Int -> (Array (Int,Int) (Set Int), Array (Int,Int) (Set Int))
buildTablesNaive word kMin = runST $ do
  let n = length word
      bnds = ((0,0),(n,n))

  sArr <- newArray bnds Set.empty
  tArr <- newArray bnds Set.empty

  forM_ [0..n-1] $ \i -> do
    case word !! i of
      'b' -> writeArray sArr (i,i+1) (Set.singleton 2)
      'a' -> writeArray tArr (i,i+1) (Set.singleton 1)
      _   -> pure ()

  forM_ [2..n] $ \len -> do
    forM_ [0..n-len] $ \i -> do
      let j = i + len
      sSet <- calcSNaive word kMin sArr tArr i j
      tSet <- calcTNaive word      sArr tArr i j
      writeArray sArr (i,j) sSet
      writeArray tArr (i,j) tSet

  sFrozen <- freeze2D sArr
  tFrozen <- freeze2D tArr
  pure (sFrozen, tFrozen)

calcSNaive
  :: String -> Int
  -> STArray s (Int,Int) (Set Int)
  -> STArray s (Int,Int) (Set Int)
  -> Int -> Int
  -> ST s (Set Int)
calcSNaive word kMin sArr tArr i j = do
  let len = j - i
      cI  = word !! i
      cJ  = word !! (j-1)

  s1 <- if len >= 3 && cI == 'a' && cJ == 'a'
        then do inner <- readArray sArr (i+1, j-1)
                pure $ Set.map (*2) inner
        else pure Set.empty

  vals <- fmap concat $ mapM (\m -> do
                                left  <- readArray tArr (i,m)
                                right <- readArray tArr (m,j)
                                pure [ v1
                                     | v1 <- Set.toList left
                                     , v2 <- Set.toList right
                                     , relTT kMin v1 v2
                                     ]
                            ) [i+1 .. j-1]
  let s2 = Set.fromList vals

  pure (s1 `Set.union` s2)

calcTNaive
  :: String
  -> STArray s (Int,Int) (Set Int)
  -> STArray s (Int,Int) (Set Int)
  -> Int -> Int
  -> ST s (Set Int)
calcTNaive word sArr tArr i j = do
  let len = j - i
      cI  = word !! i
      cJ  = word !! (j-1)

  t1 <- if len >= 3 && cI == 'b' && cJ == 'b'
        then do inner <- readArray tArr (i+1, j-1)
                pure $ Set.map (+3) inner
        else pure Set.empty

  vals <- fmap concat $ mapM (\m -> do
                                left  <- readArray sArr (i,m)
                                right <- readArray sArr (m,j)
                                pure [ v1*v2 + 1
                                     | v1 <- Set.toList left
                                     , v2 <- Set.toList right
                                     ]
                            ) [i+1 .. j-1]
  let t2 = Set.fromList vals

  pure (t1 `Set.union` t2)

type VSet = Map Int (Set Int)

emptyV :: VSet
emptyV = Map.empty

vAdd :: Int -> Int -> VSet -> VSet
vAdd odd expn = Map.insertWith Set.union odd (Set.singleton expn)

vPairs :: VSet -> [(Int,Int)]
vPairs v = [ (odd,e) | (odd,es) <- Map.toList v, e <- Set.toList es ]

vOddPairs :: VSet -> [(Int,Int)]
vOddPairs v = [ (odd,0) | (odd,es) <- Map.toList v, Set.member 0 es ]

vNonOddPairs :: VSet -> [(Int,Int)]
vNonOddPairs v = [ (odd,e) | (odd,es) <- Map.toList v, e <- Set.toList es, e /= 0 ]

vMinExp :: VSet -> Map Int Int
vMinExp = Map.map Set.findMin

mergeV :: VSet -> VSet -> VSet
mergeV = Map.unionWith Set.union

canon :: Int -> (Int,Int)
canon x =
  let tz = countTrailingZeros x
      odd = x `div` (2 ^ tz)
  in (odd, tz)

fastAccept :: String -> Int -> Maybe Bool
fastAccept word kMin
  | null word = Just False
  | not (alphabetOk word) = Just False
  | otherwise =
      let n = length word
      in case n of
           1 -> Just (word == "b")
           2 -> if kMin == 0 then Just (word == "aa") else Just False
           _ ->
             if kMin == 0 && all (=='a') word
               then Just (n /= 1 && n /= 3)
               else
                 if countChar 'b' word == 1
                   then
                     let p = indexOf 'b' word
                         l = p
                         r = n - p - 1
                     in if word == replicate l 'a' ++ "b" ++ replicate r 'a' && l == r
                          then Just True
                          else checkFamily2 word kMin
                   else checkFamily2 word kMin
  where
    countChar c = length . filter (==c)

    indexOf c = go 0
      where
        go _ [] = 0
        go i (x:xs) | x==c      = i
                    | otherwise = go (i+1) xs

    spanCount c xs = let (a,b) = span (==c) xs in (length a, b)

    checkFamily2 w kMin' =
      let (r, rest1) = spanCount 'a' w
          (m, rest2) = spanCount 'b' rest1
      in case rest2 of
           ('a':rest3) ->
             let (m2, rest4) = spanCount 'b' rest3
             in if m>0 && m2==m && rest4 == replicate (r+1) 'a'
                  then
                    let val = 1 + 3*m
                    in if isPowerOfTwo val && countTrailingZeros val >= kMin'
                         then Just True
                         else Nothing
                  else Nothing
           _ -> Nothing

inLanguageFast :: String -> Int -> Bool -> Bool
inLanguageFast word kMin enableFast
  | null word = False
  | not (alphabetOk word) = False
  | enableFast =
      case fastAccept word kMin of
        Just ans -> ans
        Nothing  -> slow
  | otherwise = slow
  where
    slow =
      let (dpS, _dpT) = buildTablesFast word kMin
      in not (Map.null (dpS ! (0, length word)))

buildTablesFast :: String -> Int -> (Array (Int,Int) VSet, Array (Int,Int) VSet)
buildTablesFast word kMin = runST $ do
  let n = length word
      bnds = ((0,0),(n,n))

  sArr <- newArray bnds emptyV
  tArr <- newArray bnds emptyV

  forM_ [0..n-1] $ \i -> do
    case word !! i of
      'b' -> writeArray sArr (i,i+1) (vAdd 1 1 emptyV)
      'a' -> writeArray tArr (i,i+1) (vAdd 1 0 emptyV)
      _   -> pure ()

  forM_ [2..n] $ \len -> do
    forM_ [0..n-len] $ \i -> do
      let j  = i + len
          cI = word !! i
          cJ = word !! (j-1)

      sSet <- do
        s1 <- if len >= 3 && cI=='a' && cJ=='a'
              then do inner <- readArray sArr (i+1, j-1)
                      pure $ foldr (\(odd,e) acc -> vAdd odd (e+1) acc) emptyV (vPairs inner)
              else pure emptyV

        s2 <- foldlM (\acc m -> do
                        left  <- readArray tArr (i,m)
                        right <- readArray tArr (m,j)
                        let rMin = vMinExp right
                            acc' = foldr
                              (\(odd, expsL) acc2 ->
                                  case Map.lookup odd rMin of
                                    Nothing   -> acc2
                                    Just mexp ->
                                      let threshold = mexp + kMin
                                          okExps = filter (>= threshold) (Set.toList expsL)
                                      in foldr (\eL a3 -> vAdd odd eL a3) acc2 okExps
                              )
                              acc
                              (Map.toList left)
                        pure acc'
                    ) emptyV [i+1 .. j-1]

        pure (Map.unionWith Set.union s1 s2)

      writeArray sArr (i,j) sSet

      tSet <- do
        t1 <- if len >= 3 && cI=='b' && cJ=='b'
              then do inner <- readArray tArr (i+1, j-1)
                      pure $ foldr addBTB emptyV (vPairs inner)
              else pure emptyV

        t2 <- foldlM (\acc m -> do
                        left  <- readArray sArr (i,m)
                        right <- readArray sArr (m,j)

                        let lOdd = vOddPairs left
                            lNon = vNonOddPairs left
                            rOdd = vOddPairs right
                            rNon = vNonOddPairs right
                            rAll = vPairs right

                            acc1 =
                              foldr (\(o1,e1) a1 ->
                                      foldr (\(o2,e2) a2 ->
                                              let prod = (o1*o2) `shiftL` (e1+e2)
                                              in vAdd (prod+1) 0 a2
                                            ) a1 rAll
                                    ) acc lNon

                            acc2 =
                              foldr (\(o1,_) a1 ->
                                      foldr (\(o2,e2) a2 ->
                                              let prod = (o1*o2) `shiftL` e2
                                              in vAdd (prod+1) 0 a2
                                            ) a1 rNon
                                    ) acc1 lOdd

                            acc3 =
                              foldr (\(o1,_) a1 ->
                                      foldr (\(o2,_) a2 ->
                                              let val = o1*o2 + 1
                                                  (od,ex) = canon val
                                              in vAdd od ex a2
                                            ) a1 rOdd
                                    ) acc2 lOdd

                        pure acc3
                    ) emptyV [i+1 .. j-1]

        pure (mergeV t1 t2)

      writeArray tArr (i,j) tSet

  sFrozen <- freeze2D sArr
  tFrozen <- freeze2D tArr
  pure (sFrozen, tFrozen)
  where
    addBTB (odd, expn) acc
      | expn > 0 =
          let val = (odd `shiftL` expn) + 3
          in vAdd val 0 acc
      | otherwise =
          let val = odd + 3
              (od,ex) = canon val
          in vAdd od ex acc

foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM _ acc []     = pure acc
foldlM f acc (x:xs) = do
  acc' <- f acc x
  foldlM f acc' xs

wordsOfLen :: Int -> [String]
wordsOfLen 0 = [""]
wordsOfLen n =
  [ c : w
  | w <- wordsOfLen (n-1)
  , c <- "ab"
  ]

allWordsUpTo :: Int -> [String]
allWordsUpTo n = filter (not . null) $ concatMap wordsOfLen [0..n]

testParsers :: Int -> Int -> Bool -> IO ()
testParsers maxLen kMin enableFast = do
  let ws = allWordsUpTo maxLen

      ref w = inLanguageNaive w kMin
      opt w = inLanguageFast  w kMin enableFast

      goods = [w | w <- ws, ref w]
      bads  = [w | w <- ws, not (ref w)]
      mism  = [ (w, ref w, opt w) | w <- ws, ref w /= opt w ]

  putStrLn "Слова, принадлежащие языку:"
  mapM_ print (take 20 goods)
  putStrLn ""

  putStrLn "Слова, не принадлежащие языку:"
  mapM_ print (take 20 bads)
  putStrLn ""

  putStrLn $ "Всего слов: " ++ show (length ws)
  putStrLn $ "Принадлежащих: " ++ show (length goods)
  putStrLn $ "Не принадлежащих: " ++ show (length bads)

  if null mism
    then putStrLn "Парсеры эквивалентно отработали на исходном наборе слов."
    else do
      putStrLn "Парсеры отработали не эквивалентно на исходном наборе слов. Примеры:"
      forM_ (take 20 mism) $ \(w,r,o) ->
        putStrLn $ "  " ++ show w ++ " : naive=" ++ show r ++ ", fast=" ++ show o

main :: IO ()
main = do
  let maxLen = 12
      kMin = 0
      enableFast = True

  testParsers maxLen kMin enableFast

