module Main where

import Prelude hiding (init)
import Graphics.UI.GLUT

display :: IO ()
display = do
  clear [ColorBuffer]
  renderPrimitive LineLoop $ mapM_ vertex vs
  flush
  where
    vs :: [Vertex2 GLfloat]
    vs = [ Vertex2 (-0.9) (-0.9)
         , Vertex2 0.9 (-0.9)
         , Vertex2 0.9 0.9
         , Vertex2 (-0.9) 0.9]


init :: IO ()
init = clearColor $= Color4 0.0 0.0 1.0 1.0

main :: IO ()
main = do
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode]
  createWindow progName
  displayCallback $= display
  init
  mainLoop
