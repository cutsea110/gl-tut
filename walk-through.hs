module Main where

import Prelude hiding (init)
import Graphics.UI.GLUT

scene :: IO ()
scene = undefined

display :: DisplayCallback
display = undefined

resize :: ReshapeCallback
resize = undefined

keyboard :: KeyboardMouseCallback
keyboard (Char '\ESC') Down _ _ = exit
keyboard (Char 'q') Down _ _ = exit
keyboard _ _ _ _ = return ()

init :: IO ()
init = do
  clearColor $= Color4 1.0 1.0 1.0 0.0
  depthFunc $= Just Lequal
  cullFace $= Just Front
  lighting $= Enabled
  light (Light 0) $= Enabled

main :: IO ()
main = do
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, WithDepthBuffer]
  createWindow progName
  displayCallback $= display
  reshapeCallback $= Just resize
  keyboardMouseCallback $= Just keyboard
  init
  mainLoop
