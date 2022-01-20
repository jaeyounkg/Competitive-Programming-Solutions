-- https://atcoder.jp/contests/arc132/tasks/arc132_a
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.State
  ( State,
    evalState,
    get,
    gets,
    put,
  )
import Data.Array.IArray
import qualified Data.ByteString.Char8 as C
import Data.Function
import Data.Int
import Data.List
import Data.Maybe

main :: IO ()
main = C.interact $ runScanner input >>> solve >>> output

data Cell = B | W

type Query = (Int, Int)

type Input = (Int, Array Int Int, Array Int Int, [Query])

type Output = [Cell]

input = do
  n <- int
  rs <- replicateM n int
  cs <- replicateM n int
  q <- int
  qs <- replicateM q (pair int int)
  pure (n, listArray (1, n) rs, listArray (1, n) cs, qs)

output = C.concat . map (\case B -> "#"; W -> ".")

solve :: Input -> Output
solve (n, rs, cs, qs) = map solveQuery qs
  where
    solveQuery = \(r, c) -> if rs ! r + cs ! c > n then B else W

----------------------------------- Template -----------------------------------
type Scanner = State [C.ByteString]

runScanner :: Scanner a -> C.ByteString -> a
runScanner = runScannerWith C.words

runScannerWith :: (C.ByteString -> [C.ByteString]) -> Scanner a -> C.ByteString -> a
runScannerWith t s = evalState s . t

bstr :: Scanner C.ByteString
bstr = get >>= \case s : ss -> put ss >> pure s

int :: Scanner Int
int = fst . fromJust . C.readInt <$> bstr

pair :: Scanner a -> Scanner b -> Scanner (a, b)
pair = liftA2 (,)
