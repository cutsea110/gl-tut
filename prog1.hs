module Main where

import Prelude hiding (init)
import Graphics.UI.GLUT

display :: IO ()
display = do
  clear [ColorBuffer]
  flush

resize :: Size -> IO ()
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  loadIdentity

keymouse :: KeyboardMouseCallback
keymouse key st mod pos = do
  print key
  print st
  print mod
  print pos

mouse :: MotionCallback
mouse p@(Position x y) = print p

init :: IO ()
init = clearColor $= Color4 1.0 1.0 1.0 1.0

main :: IO ()
main = do
  initialWindowPosition $= Position 100 100
  initialWindowSize $= Size 320 240
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode]
  createWindow progName
  displayCallback $= display
  reshapeCallback $= Just resize
  keyboardMouseCallback $= Just keymouse
  motionCallback $= Just mouse
  init
  mainLoop
