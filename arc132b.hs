{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.State (State, evalState, get, gets, put)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Function
import Data.Int
import Data.List (groupBy)
import Data.List.NonEmpty (NonEmpty, head, tail)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Prelude hiding (fromJust, head, tail)
import qualified Prelude

main :: IO ()
main = C.interact $ runScanner input >>> solve >>> output

-- main = C.interact $ runScanner (numberOf input) >>> map (solve >>> output) >>> C.unlines

type Input = [Int]

type Output = Int

input :: Scanner Input
input = replicateM 3 int

output :: Output -> C.ByteString
output = showB

solve :: Input -> Output
solve [n, p1, p2] = if p1 == 1 && p2 == 2 then 0 else min (n - p1 + 1) (p1 + 1)

--------------------------------------------------------------------------------
-- Template: convenience utilities
--------------------------------------------------------------------------------
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

bstr :: Scanner C.ByteString
bstr = get >>= \case [] -> return ""; s : ss -> put ss >> return s

int :: Scanner Int
int = fst . fromJust . C.readInt <$> bstr
