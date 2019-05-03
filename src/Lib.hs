module Lib
    ( entry
    ) where

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Graphics.Gloss
import Graphics.Gloss.Accelerate.Data.Picture
import Data.Word

window :: Display
window = FullScreen

dotp :: Acc (A.Vector Float) -> Acc (A.Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

board :: Matrix Word32
board = fromList (Z:.600:.256) (fmap Prelude.fromIntegral [0xff000000..])

entry :: IO ()
entry = do
  display window white $ bitmapOfArray board True
--entry = display window white (text "Hello, world!!!")
--entry = print . CPU.run $ dotp (use $ fromList (Z:.100000000) [0..] ) (use $ fromList (Z:.100000000) [1,3..] )
