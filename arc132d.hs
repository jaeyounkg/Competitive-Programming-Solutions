{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Applicative
import           Control.Arrow                  ( (>>>) )
import           Control.Monad
import           Control.Monad.State
import           Data.Bits
import qualified Data.ByteString.Char8         as C
import           Data.Char
import           Data.Function
import           Data.Int
import           Data.List
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import qualified Data.Vector.Unboxed           as VU
import           Debug.Trace                    ( traceShowId )

main :: IO ()
main = C.interact $ runScanner input >>> solve >>> output

type Input = (Int, VU.Vector (Int, Int))

type Output = Int

input = do
    (n, m) <- two int
    (s, t) <- two $ map digitToInt <$> str
    let e = if n < m then 0 else 1 -- not necessary, but reduces computation
        v = VU.fromList
            $ zipWith (\a b -> (min a b, max a b)) (elemIndices e s) (elemIndices e t)
    pure $ traceShowId (n + m, v)

output = showB

solve :: Input -> Output
solve (l, v) = 1
-- solve (l, v) = reduc $ foldl' update

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
