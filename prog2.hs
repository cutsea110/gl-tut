module Main where

import Prelude hiding (init)
import Graphics.UI.GLUT
import Control.Monad
import Data.Array
import Data.IORef

cubeVertex :: Array Int (Vertex3 GLdouble)
cubeVertex = listArray (0, 7) vs
  where
    vs =
      [ Vertex3 0.0 0.0 0.0
      , Vertex3 1.0 0.0 0.0
      , Vertex3 1.0 1.0 0.0
      , Vertex3 0.0 1.0 0.0
      , Vertex3 0.0 0.0 1.0
      , Vertex3 1.0 0.0 1.0
      , Vertex3 1.0 1.0 1.0
      , Vertex3 0.0 1.0 1.0
      ]

cubeFace :: Array Int (Int,Int,Int,Int)
cubeFace = listArray (0, 5) fs
  where 
    fs =
      [(0, 1, 2, 3)
      ,(1, 5, 6, 2)
      ,(5, 4, 7, 6)
      ,(4, 0, 3, 7)
      ,(4, 5, 1, 0)
      ,(3, 2, 6, 7)
      ]

cubeNormal :: Array Int (Normal3 GLdouble)
cubeNormal = listArray (0, 5) ns
  where
    ns =
      [ Normal3 0.0 0.0 (-1.0)
      , Normal3 1.0 0.0 0.0
      , Normal3 0.0 0.0 1.0
      , Normal3 (-1.0) 0.0 0.0
      , Normal3 0.0 (-1.0) 0.0
      , Normal3 0.0 1.0 0.0
      ]

lightPosition :: Array Int (Vertex4 GLfloat)
lightPosition = listArray (0, 1) ls
  where
    ls = [ Vertex4 0.0 3.0 5.0 1.0
         , Vertex4 5.0 3.0 0.0 1.0
         ]

green :: Color4 GLfloat
green = Color4 0.0 1.0 0.0 1.0

red :: Color4 GLfloat
red = Color4 0.8 0.2 0.2 1.0

idle :: Maybe Window -> IO ()
idle = postRedisplay

cube :: IO ()
cube = do
  renderPrimitive Quads $ do
    forM_ [0..5] renderQuad
  where
    renderQuad :: Int -> IO ()
    renderQuad i = do
      normal $ cubeNormal!i
      vertex $ cubeVertex!x'
      vertex $ cubeVertex!y'
      vertex $ cubeVertex!z'
      vertex $ cubeVertex!w'
      where
        (x', y', z', w') = cubeFace!i
  

display :: IORef GLdouble -> IO ()
display r = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  lookAt
    (Vertex3 3.0 4.0 5.0)
    (Vertex3 0.0 0.0 0.0)
    (Vector3 0.0 1.0 0.0)
  position (Light 0) $= lightPosition!0
  position (Light 1) $= lightPosition!1
  rot <- readIORef r
  modifyIORef r ((% 360.0).(+1.0))
  preservingMatrix $ do
    rotate rot $ Vector3 0.0 1.0 0.0
    materialAmbientAndDiffuse FrontAndBack $= red
    color $ Color3 0.0 0.0 (0.0::GLdouble)
    cube
  swapBuffers
  where
    (%) :: GLdouble -> GLdouble -> GLdouble
    x % y = if x > y then x - y else x

resize :: ReshapeCallback
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 30.0 (w'/h') 1.0 100.0
  matrixMode $= Modelview 0
  where
    (w', h') = (fromIntegral w, fromIntegral h)

keymouse :: KeyboardMouseCallback
keymouse (MouseButton LeftButton) Down _ _ = do 
  idleCallback $= Just (idle Nothing)
keymouse (MouseButton LeftButton) Up _ _ = do
  idleCallback $= Nothing
keymouse (MouseButton RightButton) Down _ _ = do
  postRedisplay Nothing
keymouse (Char 'q') _ _ _ = exit
keymouse (Char 'Q') _ _ _ = exit
keymouse (Char '\ESC') _ _ _ = exit
keymouse _ _ _ _ = return ()

init :: IO ()
init = do
  clearColor $= Color4 1.0 1.0 1.0 1.0
  depthFunc $= Just Lequal
  cullFace $= Just Front
  lighting $= Enabled
  light (Light 0) $= Enabled
  light (Light 1) $= Enabled
  diffuse (Light 1) $= green
  specular (Light 1) $= green

main :: IO ()
main = do
  r <- newIORef 0.0
  
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  createWindow progName
  displayCallback $= display r
  reshapeCallback $= Just resize
  keyboardMouseCallback $= Just keymouse
  init
  mainLoop
