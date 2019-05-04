module Main where

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Graphics.Gloss
import Graphics.Gloss.Accelerate.Data.Picture
import Data.Word
import Data.Array.Accelerate.System.Random.MWC

type Model = Matrix Bool
type View = Matrix Word32

modelToBitmap :: Acc Model -> Acc View
modelToBitmap = A.map (\b -> b ? (0xff000000, 0xffffffff))

step :: Acc Model -> Acc Model
step = stencil rule clamp
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

main :: IO ()
main = do
  i <- randomArray uniform (Z:.1024:.1024)
  let view = CPU.run $ modelToBitmap (use i)
  simulate FullScreen white 30 i (flip bitmapOfArray False . CPU.run . modelToBitmap . use) $
    \_ _ m ->
      CPU.run $ step (use m)



