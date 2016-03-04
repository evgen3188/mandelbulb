module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

data Complex = C Float Float deriving (Show, Eq)

instance Num Complex where
  fromInteger n = C (fromIntegral n) 0.0
  (C x y) * (C z t) = C (z*x - y*t) (y*z + x*t)
  (C x y) + (C z t) = C (x + z) (y + t)
  abs (C x y) = C (sqrt (x*x + y*y)) 0.0
  signum (C x y) = C (signum x) 0.0

complex :: Float -> Float -> Complex
complex = C

real :: Complex -> Float
real (C x y) = x

im :: Complex -> Float
im (C x y) = y

magnitude :: Complex -> Float
magnitude = real.abs

main :: IO ()
main = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "Mandelbrot Set with Haskell and OpenGL"
  displayCallback $= display
  mainLoop

display = do
  clear [ColorBuffer]
  loadIdentity
  preservingMatrix drawMandelbrot
  swapBuffers

drawMandelbrot =
  renderPrimitive Points $ mapM_ drawColoredPoint allPoints
  where
    drawColoredPoint (x, y, c) = do
      color c
      vertex $ Vertex3 x y 0

width  = 320 :: GLfloat
height = 320 :: GLfloat

allPoints :: [(GLfloat, GLfloat, Color3 GLfloat)]
allPoints = [ (x/width, y/height, colorFromValue $ mandel x y) |
                x <- [-width..width],
                y <- [-height..height]]

colorFromValue n =
  let
    t :: Int -> GLfloat
    t i = 0.5 + 0.5*cos( fromIntegral i / 10 )
  in
    Color3 (t n) (t (n+5)) (t (n+10))

mandel x y =
  let r = 2.0 * x / width
      i = 2.0 * y / height
  in
      f (complex r i) 0 64

f :: Complex -> Complex -> Int -> Int
f c z 0 = 0
f c z n = if magnitude z > 2
          then n
          else f c ((z*z) + c) (n-1)

