module Lib
    ( entry
    ) where

import Prelude as P
import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import Graphics.Gloss.Accelerate.Data.Picture
import Data.Word
import Data.Array.Accelerate.System.Random.MWC

import GHC.IO.Unsafe

window :: Display
window = FullScreen


type Model = Matrix Bool
type View = Matrix Word32

initialModel :: IO Model
initialModel = randomArray uniform (Z:.1024:.1024)

modelToBitmap :: Acc Model -> Acc View
modelToBitmap = A.map (\b -> b ? (0xff000000, 0xffffffff))

step :: Acc Model -> Acc Model
step = stencil rule wrap
  where
    rule :: Stencil3x3 Bool -> Exp Bool
    rule ((x11, x12, x13)
         ,(x21, x22, x23)
         ,(x31, x32, x33)
         ) =
      let
        n =   (cond x11 (constant 1) (constant 0))
          A.+ (cond x12 (constant 1) (constant 0))
          A.+ (cond x13 (constant 1) (constant 0))
          A.+ (cond x21 (constant 1) (constant 0))
          A.+ (cond x23 (constant 1) (constant 0))
          A.+ (cond x31 (constant 1) (constant 0))
          A.+ (cond x32 (constant 1) (constant 0))
          A.+ (cond x33 (constant 1) (constant 0))
      in
        cond x22
        (n A.== (2 :: Exp Int) A.|| n A.== (3 :: Exp Int))
        (n A.== (3 :: Exp Int))


entry :: IO ()
entry = do
  i <- initialModel
  {-let i = fromList (Z:.4:.4)
          [ False, False, False, False
          , False, True , False, False
          , False, True , False, False
          , False, True , False, False
          ]-}
  let view = CPU.run $ modelToBitmap (use i)
  simulate window white 1 i (scale 0.5 0.5 . flip bitmapOfArray False . CPU.run . modelToBitmap . use) $
    \_ _ m ->
      CPU.run $ step (use m)
--entry = display window white (text "Hello, world!!!")
--entry = print . CPU.run $ dotp (use $ fromList (Z:.100000000) [0..] ) (use $ fromList (Z:.100000000) [1,3..] )
