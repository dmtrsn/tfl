module Main where

import System.Random (randomRIO)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (isPrefixOf)
import Control.Monad (replicateM)

type StateId = Int
type Trans   = M.Map (StateId, Char) (S.Set StateId)

blocks :: [String]
blocks = ["abc", "bac", "cba", "bca", "acb"]

randInt :: (Int, Int) -> IO Int
randInt range = randomRIO range

randChoice :: [a] -> IO a
randChoice xs = do
  i <- randInt (0, length xs - 1)
  return (xs !! i)

randChoiceChar :: String -> IO Char
randChoiceChar s = randChoice s

generateWord :: Int -> IO String
generateWord maxBlocks = do
  k <- randInt (0, maxBlocks)

  prefixBlocks <- replicateM k (randChoice blocks)
  let prefix = concat prefixBlocks

  c2 <- randChoiceChar "abc"
  c3 <- randChoiceChar "ab"
  c4 <- randChoiceChar "bc"
  c5 <- randChoiceChar "ac"

  let tailStr = "aa" ++ [c2, c3, c4, c5]
  return (prefix ++ tailStr)

randomString :: Int -> IO String
randomString maxLen = do
  n <- randInt (0, maxLen)
  replicateM n (randChoiceChar "abc")

dfaTrans :: M.Map (StateId, Char) StateId
dfaTrans = M.fromList
  [ ((0,'a'),1), ((0,'b'),4), ((0,'c'),6)
  , ((1,'b'),2), ((1,'a'),7), ((1,'c'),3)
  , ((2,'c'),0)
  , ((3,'b'),0)
  , ((4,'c'),5), ((4,'a'),2)
  , ((5,'a'),0)
  , ((6,'b'),5)
  , ((7,'a'),8), ((7,'b'),8), ((7,'c'),8)
  , ((8,'a'),9), ((8,'b'),9)
  , ((9,'b'),10), ((9,'c'),10)
  , ((10,'a'),11), ((10,'c'),11)
  ]

dfaStart :: StateId
dfaStart = 0

dfaFinal :: S.Set StateId
dfaFinal = S.fromList [11]

dfaAccept :: String -> Bool
dfaAccept word = go dfaStart word
  where
    go st [] = st `S.member` dfaFinal
    go st (c:cs) =
      case M.lookup (st, c) dfaTrans of
        Nothing  -> False
        Just st' -> go st' cs

addNfaEdge :: Trans -> StateId -> String -> StateId -> Trans
addNfaEdge trans u symbols v =
  foldl addOne trans symbols
  where
    addOne m ch =
      M.insertWith S.union (u, ch) (S.singleton v) m

nfaRun :: StateId -> S.Set StateId -> String -> Trans -> Bool
nfaRun start finals word trans = go (S.singleton start) word
  where
    go current [] =
      not (S.null (S.intersection current finals))

    go current (c:cs) =
      let nextStates =
            S.unions [ M.findWithDefault S.empty (st, c) trans
                     | st <- S.toList current
                     ]
      in if S.null nextStates
         then False
         else go nextStates cs

nfaTrans :: Trans
nfaTrans =
  let t0  = M.empty
      t1  = addNfaEdge t0  0 "a"   1
      t2  = addNfaEdge t1  0 "a"   7
      t3  = addNfaEdge t2  1 "b"   2
      t4  = addNfaEdge t3  2 "c"   0
      t5  = addNfaEdge t4  1 "c"   3
      t6  = addNfaEdge t5  3 "b"   0
      t7  = addNfaEdge t6  0 "b"   4
      t8  = addNfaEdge t7  4 "c"   5
      t9  = addNfaEdge t8  5 "a"   0
      t10 = addNfaEdge t9  0 "c"   6
      t11 = addNfaEdge t10 6 "b"   5
      t12 = addNfaEdge t11 4 "a"   2

      t13 = addNfaEdge t12 7 "a"   8
      t14 = addNfaEdge t13 8 "abc" 9
      t15 = addNfaEdge t14 9 "ab"  10
      t16 = addNfaEdge t15 10 "bc" 11
      t17 = addNfaEdge t16 11 "ac" 12
  in t17

nfaStart :: StateId
nfaStart = 0

nfaFinal :: S.Set StateId
nfaFinal = S.fromList [12]

nfaAccept :: String -> Bool
nfaAccept word = nfaRun nfaStart nfaFinal word nfaTrans

pkaBranch1Trans :: Trans
pkaBranch1Trans =
  let t0 = M.empty
      t1  = addNfaEdge t0  1 "a" 2
      t2  = addNfaEdge t1  1 "b" 5
      t3  = addNfaEdge t2  1 "c" 7
      t4  = addNfaEdge t3  2 "a" 8
      t5  = addNfaEdge t4  2 "b" 3
      t6  = addNfaEdge t5  2 "c" 4
      t7  = addNfaEdge t6  3 "c" 1
      t8  = addNfaEdge t7  4 "b" 1
      t9  = addNfaEdge t8  5 "c" 6
      t10 = addNfaEdge t9  5 "a" 3
      t11 = addNfaEdge t10 7 "b" 6
      t12 = addNfaEdge t11 6 "a" 1
      t13 = addNfaEdge t12 8 "abc" 8
  in t13

pkaBranch2Trans :: Trans
pkaBranch2Trans =
  let t0  = M.empty
      t1  = addNfaEdge t0  9 "abc" 9
      t2  = addNfaEdge t1  9 "a" 10

      t3  = addNfaEdge t2  10 "a" 11

      t4  = addNfaEdge t3  11 "abc" 12

      t5  = addNfaEdge t4  12 "ab" 13

      t6  = addNfaEdge t5  13 "bc" 14

      t7  = addNfaEdge t6  14 "ac" 15
  in t7

afaAccept :: String -> Bool
afaAccept word =
  let branch1Ok = nfaRun 1 (S.fromList [8])  word pkaBranch1Trans
      branch2Ok = nfaRun 9 (S.fromList [15]) word pkaBranch2Trans
  in branch1Ok && branch2Ok

regexAccept :: String -> Bool
regexAccept word =
  let rest = dropBlocks word
  in length rest == 6
     && take 2 rest == "aa"
     && rest !! 2 `elem` "abc"
     && rest !! 3 `elem` "ab"
     && rest !! 4 `elem` "bc"
     && rest !! 5 `elem` "ac"
  where
    dropBlocks s =
      case takeBlock s of
        Just s' -> dropBlocks s'
        Nothing -> s

    takeBlock s =
      case filter (`isPrefixOf` s) blocks of
        []    -> Nothing
        (b:_) -> Just (drop (length b) s)

main :: IO ()
main = do
  ok1 <- testGenerated 1000
  putStrLn "---------------------"
  ok2 <- testRandom 1000
  putStrLn "---------------------"
  putStrLn ("All accepted: " ++ show (ok1 && not(ok2)))

testGenerated :: Int -> IO Bool
testGenerated n = go n True
  where
    go 0 acc = return acc
    go k acc = do
      w <- generateWord 5

      let dfaA = dfaAccept w
      let nfaA = nfaAccept w
      let afaA = afaAccept w

      putStrLn ("Generated word: " ++ w)
      putStrLn ("DFA accepted: " ++ show dfaA)
      putStrLn ("NFA accepted: " ++ show nfaA)
      putStrLn ("AFA accepted: " ++ show afaA)

      go (k-1) (acc && dfaA && nfaA && afaA)

testRandom :: Int -> IO Bool
testRandom n = go n True
  where
    go 0 acc = return acc
    go k acc = do
      w <- randomString 20

      let reA  = regexAccept w
      let dfaA = dfaAccept w
      let nfaA = nfaAccept w
      let afaA = afaAccept w

      let chainedBad = not (dfaA == nfaA && nfaA == afaA && dfaA == reA)

      putStrLn ("Generated word: " ++ w)
      putStrLn ("Regex accepted: " ++ show reA)
      putStrLn ("DFA accepted: " ++ show dfaA)
      putStrLn ("NFA accepted: " ++ show nfaA)
      putStrLn ("AFA accepted: " ++ show afaA)

      go (k-1) (acc && not chainedBad)
