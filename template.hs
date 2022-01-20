{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.State (State, evalState, get, gets, put)
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.Function
import Data.Int
import Data.List hiding (head, tail)
import Data.List.NonEmpty (NonEmpty, head, tail)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Prelude hiding (head, tail)
import qualified Prelude

main :: IO ()
main = C.interact $ runScanner input >>> solve >>> output

-- main = C.interact $ runScanner (numberOf input) >>> map (solve >>> output) >>> C.unlines

type Input = ()

type Output = ()

input :: Scanner Input
input = undefined

output :: Output -> C.ByteString
output = undefined

solve :: Input -> Output
solve = undefined

--------------------------------------------------------------------------------
-- Template: convenience utilities.
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

--------------------------------------------------------------------------------
-- Template: Scanner functions. Most of these are deliberately unsafe.
--------------------------------------------------------------------------------
type Scanner = State [C.ByteString]

runScanner :: Scanner a -> C.ByteString -> a
runScanner = runScannerWith C.words

runScannerWith :: (C.ByteString -> [C.ByteString]) -> Scanner a -> C.ByteString -> a
runScannerWith t s = evalState s . t

peek :: Scanner C.ByteString
peek = gets Prelude.head

bstr :: Scanner C.ByteString
bstr = get >>= \case [] -> return ""; s : ss -> put ss >> return s

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

numberOf :: Scanner a -> Scanner [a]
numberOf s = int >>= flip replicateM s

many :: Scanner a -> Scanner [a]
many s = get >>= \case [] -> return []; _ -> liftA2 (:) s (many s)

till :: (C.ByteString -> Bool) -> Scanner a -> Scanner [a]
till p s = do
  t <- peek
  if p t
    then return []
    else (:) <$> s <*> till p s

two, three, four :: Scanner a -> Scanner [a]
[two, three, four] = map replicateM [2 .. 4]

pair :: Scanner a -> Scanner b -> Scanner (a, b)
pair = liftA2 (,)

showB :: Show a => a -> C.ByteString
showB = C.pack . show
