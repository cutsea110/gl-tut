module Main where

import Prelude hiding (init)
import Graphics.UI.GLUT
import Control.Monad
import Data.Array

cubeVertex :: Array Int (GLdouble, GLdouble, GLdouble)
cubeVertex = listArray (0, 7) vs
  where
    vs =
      [(0.0, 0.0, 0.0)
      ,(1.0, 0.0, 0.0)
      ,(1.0, 1.0, 0.0)
      ,(0.0, 1.0, 0.0)
      ,(0.0, 0.0, 1.0)
      ,(1.0, 0.0, 1.0)
      ,(1.0, 1.0, 1.0)
      ,(0.0, 1.0, 1.0)
      ]

cubeEdge :: Array Int (Int,Int)
cubeEdge = listArray (0, 11) es
  where
    es =
      [(0,1)
      ,(1,2)
      ,(2,3)
      ,(3,0)
      ,(4,5)
      ,(5,6)
      ,(6,7)
      ,(7,4)
      ,(0,4)
      ,(1,5)
      ,(2,6)
      ,(3,7)
      ]

display :: IO ()
display = do
  clear [ColorBuffer]
  color $ Color3 0.0 0.0 (0.0::GLdouble)
  renderPrimitive Lines $ do
    forM_ [0..11] renderLine
  flush
  where
    renderLine :: Int -> IO ()
    renderLine i = do
      uncurry3 vertex3D f 
      uncurry3 vertex3D s
      where
        f = cubeVertex!(fst(cubeEdge!i))
        s = cubeVertex!(snd(cubeEdge!i))

vertex3D :: GLdouble -> GLdouble -> GLdouble -> IO ()
vertex3D x y z = vertex3d $ Vertex3 x y z
  where
    vertex3d = vertex :: Vertex3 GLdouble -> IO ()
  
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z

resize :: ReshapeCallback
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  loadIdentity
  perspective 30.0 (w'/h') 1.0 100.0
  translate $ Vector3 (-0.5) (-0.5) (-5.0::GLdouble)
  where
    (w', h') = (fromIntegral w, fromIntegral h)

init :: IO ()
init = clearColor $= Color4 1.0 1.0 1.0 1.0

main :: IO ()
main = do
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode]
  createWindow progName
  displayCallback $= display
  reshapeCallback $= Just resize
  init
  mainLoop
