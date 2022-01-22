{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Applicative
import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.State (State, evalState, get, gets, put)
import Data.Array.IArray (Array, (!), (//))
import qualified Data.Array.IArray as A
import Data.Bits
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.Function
import Data.Int
import Data.List hiding (head, tail)
import Data.List.NonEmpty (NonEmpty, head, tail)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Debug.Trace (traceShow, traceShowId)
import Prelude hiding (head, tail)
import qualified Prelude

main :: IO ()
main = C.interact $ runScanner input >>> solve >>> output

type Input = (Int, Int, Array Int Int)

type Output = Int

input = do
  (n, d) <- two int
  as <- lotsOf int
  pure (n, d, A.listArray (1, n) as)

output = showB

plus a b = (a + b) `mod` 998244353

solve :: Input -> Output
solve (n, d, as) = foldl1' plus $ A.elems $ foldl' update dp_init [1 .. n]
  where
    bnds = (0, 1 `shiftL` (d * 2 + 1) - 1)

    -- states
    -- defined as a lazy Array for memoization
    s_base :: Array Int Int
    s_base =
      A.listArray
        (0, n)
        [ let f = \k -> fromEnum (A.inRange (1, n) (i + k) && as ! (i + k) > 0) `shiftL` (d + k)
           in sum $ map f [- d .. d]
          | i <- [0 .. n]
        ]
    s_init = s_base ! 0

    -- dp[i][s]: i番目までおいてsの状態（固定されたiを含む）になったときの組み合わせの数
    dp_init :: Array Int Int
    dp_init = A.listArray bnds (repeat 0) // [(s_init, 1)]

    update dp_prev i = A.listArray bnds $ map compute (A.range bnds)
      where
        prev = flip shiftL 1 . flip clearBit (d * 2)
        -- edge cases
        compute s =
          if
              -- including unavailable bits
              | any (testBit s . (+ d)) . filter (\k -> not $ A.inRange (1, n) (i + k)) $ [- d .. d] -> 0
              -- not including s_base
              | (s `xor` s_base ! i) .&. s_base ! i > 0 -> 0
              -- already put, or putting at i + d
              | i `elem` as || (s `xor` s_base ! i) `testBit` (d * 2) -> (dp_prev ! prev s) `plus` (dp_prev ! (prev s + 1))
              | otherwise -> compute' s
        -- cons cases
        compute' s =
          let s_added = s `xor` s_base ! i
              p k = testBit s_added (d + k)
              f k =
                let prev_s = prev $ clearBit s (d + k)
                 in (dp_prev ! prev_s) `plus` (dp_prev ! (prev_s + 1))
           in (sum . map f . filter p) [- d .. d - 1]

-- Debugging

traceShowDp dp = traceShow (filter (\(_, v) -> v > 0) . A.assocs $ dp)

solveDumber (n, d) = filter p $ permutations [1 .. n]
  where
    p perm = all (\(i, v) -> abs (i - v) <= d) (A.assocs arr)
      where
        arr = A.listArray (1, n) perm :: Array Int Int

--------------------------------------------------------------------------------
-- Template: convenience utilities
--------------------------------------------------------------------------------
groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . on (==)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (hs, ts) = splitAt k xs in hs : chunksOf k ts

adjacents :: NonEmpty a -> [(a, a)]
adjacents = adjacentsWith (,)

adjacentsWith :: (a -> a -> b) -> NonEmpty a -> [b]
adjacentsWith f xs = zipWith f (NE.toList xs) (tail xs)

count :: Eq a => a -> [a] -> Int
count = countBy . (==)

countBy :: (a -> Bool) -> [a] -> Int
countBy p = length . filter p

showB :: Show a => a -> C.ByteString
showB = C.pack . show

--------------------------------------------------------------------------------
-- Template: Scanner functions. Many of these are deliberately unsafe.
--------------------------------------------------------------------------------
type Scanner = State [C.ByteString]

runScanner :: Scanner a -> C.ByteString -> a
runScanner = runScannerWith C.words

runScannerWith :: (C.ByteString -> [C.ByteString]) -> Scanner a -> C.ByteString -> a
runScannerWith t s = evalState s . t

peek :: Scanner C.ByteString
peek = gets Prelude.head

bstr :: Scanner C.ByteString
bstr = get >>= \case [] -> pure ""; s : ss -> put ss >> pure s

str :: Scanner String
str = C.unpack <$> bstr

readStr :: Read a => Scanner a
readStr = read <$> str

int :: Scanner Int
int = fst . fromJust . C.readInt <$> bstr

integer :: Scanner Integer
integer = readStr

int64 :: Scanner Int64
int64 = readStr

double :: Scanner Double
double = readStr

decimal :: Int -> Scanner Int
decimal p = round . ((10 ^ p) *) <$> double

lotsOf :: Scanner a -> Scanner [a]
lotsOf s = get >>= \case [] -> pure []; _ -> liftA2 (:) s (lotsOf s)

till :: (C.ByteString -> Bool) -> Scanner a -> Scanner [a]
till p s = do
  t <- peek
  if p t
    then pure []
    else (:) <$> s <*> till p s

pair :: Scanner a -> Scanner b -> Scanner (a, b)
pair = liftA2 (,)

two :: Scanner a -> Scanner (a, a)
two s = pair s s

three :: Scanner a -> Scanner (a, a, a)
three s = liftA3 (,,) s s s

four :: Scanner a -> Scanner (a, a, a, a)
four s = liftM4 (,,,) s s s s
