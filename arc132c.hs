{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Arrow                  ( (>>>) )
import           Control.Monad
import           Control.Monad.State
import           Data.Bits
import qualified Data.ByteString.Char8         as C
import           Data.Char
import           Data.Function
import           Data.Int
import qualified Data.Array                    as A
import           Data.List
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import           Data.Vector.Unboxed            ( (!) )
import qualified Data.Vector.Unboxed           as VU
import           Debug.Trace                    ( traceShow
                                                , traceShowId
                                                )
import           Data.Ix                        ( Ix(inRange) )

main :: IO ()
main = C.interact $ runScanner input >>> solve >>> output

type Input = (Int, Int, VU.Vector Int)

type Output = Int

type States = Int

-- change to 0-based indexing
input = do
    (n, d) <- two int
    as     <- lotsOf int
    pure (n, d, VU.fromList . map (subtract 1) $ as)

output = showB

plus a b = (a + b) `mod` 998244353

solve :: Input -> Output
solve (n, d, as) = VU.foldl1' plus $ foldl' update dp_init [0 .. n - 1]
  where
    n_states = 1 `shiftL` (d * 2 + 1)

    -- states
    get_s_base i = sum $ map (f i) [-d .. d]
      where
        f i k = fromEnum (inRange (0, n - 1) (i + k) && as ! (i + k) >= 0) `shiftL` (d + k)
    -- defined as a Vector for memoization
    s_base :: VU.Vector States
    s_base = VU.generate n get_s_base
    s_init = get_s_base (-1)

    -- dp[i][s]: i番目までおいてsの状態（固定されたiを含む）になったときの組み合わせの数
    dp_init :: VU.Vector Int
    dp_init = VU.generate n_states $ \s -> if s == s_init then 1 else 0

    update dp_prev i = VU.generate n_states compute
      where
        prev = flip shiftL 1 . flip clearBit (d * 2)
        -- edge cases
        compute s = if
            | any (testBit s . (+ d))
                . filter (\k -> not $ inRange (0, n - 1) (i + k))
                $ [-d .. d]
            -> 0
            | (s `xor` s_base ! i) .&. s_base ! i > 0
            -> 0
            | i `VU.elem` as || (s `xor` s_base ! i) `testBit` (d * 2)
            -> (dp_prev ! prev s) `plus` (dp_prev ! (prev s + 1))
            | otherwise
            -> compute' s
        -- cons cases
        compute' s = (sum . map f . filter p) [-d .. d - 1]
          where
            s_added = s `xor` s_base ! i
            p k = testBit s_added (d + k)
            f k =
                let prev_s = prev $ clearBit s (d + k)
                in  (dp_prev ! prev_s) `plus` (dp_prev ! (prev_s + 1))

-- Debugging
traceShowDp dp = traceShow (filter (\(_, v) -> v > 0) . zip [0 ..] . VU.toList $ dp) dp

solveDumber (n, d) = filter p $ permutations [1 .. n]
  where
    p perm = all (\(i, v) -> abs (i - v) <= d) (A.assocs arr)
        where arr = A.listArray (1, n) perm :: A.Array Int Int

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
adjacentsWith f xs = zipWith f (NE.toList xs) (NE.tail xs)

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
bstr = get >>= \case
    []     -> pure ""
    s : ss -> put ss >> pure s

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
lotsOf s = get >>= \case
    [] -> pure []
    _  -> liftA2 (:) s (lotsOf s)

till :: (C.ByteString -> Bool) -> Scanner a -> Scanner [a]
till p s = do
    t <- peek
    if p t then pure [] else (:) <$> s <*> till p s

pair :: Scanner a -> Scanner b -> Scanner (a, b)
pair = liftA2 (,)

two :: Scanner a -> Scanner (a, a)
two s = pair s s

three :: Scanner a -> Scanner (a, a, a)
three s = liftA3 (,,) s s s

four :: Scanner a -> Scanner (a, a, a, a)
four s = liftM4 (,,,) s s s s
