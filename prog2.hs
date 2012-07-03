module Main where

import Prelude hiding (init)
import Graphics.UI.GLUT

display :: IO ()
display = do
  clear [ColorBuffer]
  renderPrimitive Polygon $ mapM_ draw $ zip colors vs
  flush
  where
    draw (c,v) = color c >> vertex v
    colors :: [Color3 GLdouble]
    colors = [ Color3 1.0 0.0 0.0
             , Color3 0.0 1.0 0.0
             , Color3 0.0 0.0 1.0
             , Color3 1.0 1.0 0.0
             ]
    vs :: [Vertex2 GLfloat]
    vs = [ Vertex2 (-0.9) (-0.9)
         , Vertex2 0.9 (-0.9)
         , Vertex2 0.9 0.9
         , Vertex2 (-0.9) 0.9]


init :: IO ()
init = clearColor $= Color4 1.0 1.0 1.0 1.0

main :: IO ()
main = do
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode]
  createWindow progName
  displayCallback $= display
  init
  mainLoop