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
keyboard = undefined

init :: IO ()
init = undefined

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
