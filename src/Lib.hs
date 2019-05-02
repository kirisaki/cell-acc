module Lib
    ( entry
    ) where

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

entry :: IO ()
entry = print . CPU.run $ dotp (use $ fromList (Z:.10) [0..] ) (use $ fromList (Z:.10) [1,3..] )
