module Main where

import Prelude hiding (init)
import Graphics.UI.GLUT
import Control.Monad
import Data.Array
import Data.IORef

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

idle :: Maybe Window -> IO ()
idle = postRedisplay

display :: IORef GLdouble -> IO ()
display r = do
  clear [ColorBuffer]
  loadIdentity
  lookAt
    (Vertex3 3.0 4.0 5.0)
    (Vertex3 0.0 0.0 0.0)
    (Vector3 0.0 1.0 0.0)
  rot <- readIORef r
  modifyIORef r ((% 360.0).(+1.0))
  rotate rot $ Vector3 0.0 1.0 0.0
  color $ Color3 0.0 0.0 (0.0::GLdouble)
  renderPrimitive Lines $ do
    forM_ [0..11] renderLine
  flush
  where
    (%) :: GLdouble -> GLdouble -> GLdouble
    x % y = if x > y then x - y else x
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
init = clearColor $= Color4 1.0 1.0 1.0 1.0

main :: IO ()
main = do
  r <- newIORef 0.0
  
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode]
  createWindow progName
  displayCallback $= display r
  reshapeCallback $= Just resize
  keyboardMouseCallback $= Just keymouse
  init
  mainLoop
