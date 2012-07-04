module Main where

import Prelude hiding (init)
import Graphics.UI.GLUT
import Data.IORef

display :: IORef [Position] -> IO ()
display r = do
  clear [ColorBuffer]
  ps <- readIORef r
  renderPrimitive Lines $ do
    color $ Color3 0.0 0.0 (0.0::GLdouble)
    mapM_ vertex2D $ reverse ps
  flush

resize :: Size -> IO ()
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  ortho (-0.5) (w'-0.5) (h'-0.5) (-0.5) (-1.0) 1.0
  matrixMode $= Modelview 0
  loadIdentity
  where
    (w', h') = (fromIntegral w, fromIntegral h)

keymouse :: IORef [Position] -> KeyboardMouseCallback
keymouse r (MouseButton LeftButton) Down _ p = do
  modifyIORef r (p:)
  putStr "down at "
  putStrLn $ show p
keymouse r (MouseButton LeftButton) Up _ p = do
  modifyIORef r (p:)
  putStr "up at "
  putStrLn $ show p
  (p1:p0:_) <- readIORef r
  renderPrimitive Lines $ do
    color $ Color3 0.0 0.0 (0.0::GLdouble)
    vertex2D p0
    vertex2D p1
  flush
keymouse _ _ _ _ _ = return ()

vertex2D :: Position -> IO ()
vertex2D (Position x y) = vertex2f $ Vertex2 x' y'
  where
    (x', y') = (fromIntegral x, fromIntegral y)
    vertex2f = vertex :: Vertex2 GLfloat -> IO ()

mouse :: IORef [Position] -> MotionCallback
mouse _ = print

init :: IO ()
init = clearColor $= Color4 1.0 1.0 1.0 1.0

main :: IO ()
main = do
  -- trace mouse position
  ref <- newIORef []
  
  initialWindowPosition $= Position 100 100
  initialWindowSize $= Size 320 240
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode]
  createWindow progName
  displayCallback $= display ref
  reshapeCallback $= Just resize
  keyboardMouseCallback $= Just (keymouse ref)
  motionCallback $= Just (mouse ref)
  init
  mainLoop
