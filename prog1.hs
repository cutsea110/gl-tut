module Main where

import Prelude hiding (init)
import Graphics.UI.GLUT
import Data.IORef
import Data.Maybe

display :: IORef [Position] -> IO ()
display rps = do
  clear [ColorBuffer]
  ps <- readIORef rps
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

keymouse :: IORef [Position] -> IORef (Maybe Position) -> KeyboardMouseCallback
keymouse rps rp (MouseButton LeftButton) Down _ p = do
  modifyIORef rps (p:)
  writeIORef rp (Just p)
  putStr "down at "
  putStrLn $ show p
keymouse rps rp (MouseButton LeftButton) Up _ p = do
  (p0:_) <- readIORef rps
  modifyIORef rps (p:)
  writeIORef rp Nothing
  putStr "up at "
  putStrLn $ show p
  renderPrimitive Lines $ do
    color $ Color3 0.0 0.0 (0.0::GLdouble)
    vertex2D p0
    vertex2D p
  flush
keymouse _ _ _ _ _ _ = return ()

vertex2D :: Position -> IO ()
vertex2D (Position x y) = vertex2f $ Vertex2 x' y'
  where
    (x', y') = (fromIntegral x, fromIntegral y)
    vertex2f = vertex :: Vertex2 GLfloat -> IO ()

mouse :: IORef [Position] -> IORef (Maybe Position) -> MotionCallback
mouse rps rp p = do
  (p0:_) <- readIORef rps
  mp <- readIORef rp
  writeIORef rp (Just p)
  logicOp $= Just Invert
  if isJust mp
    then do
    renderPrimitive Lines $ do
      color $ Color3 0.0 0.0 (0.0::GLdouble)
      vertex2D p0
      vertex2D $ fromJust mp
    flush
    else
    return ()
  logicOp $= Just Copy
  ps <- readIORef rps
  renderPrimitive Lines $ do
    color $ Color3 0.0 0.0 (0.0::GLdouble)
    vertex2D p0
    vertex2D p
    mapM_ vertex2D $ reverse ps
  flush
  
init :: IO ()
init = clearColor $= Color4 1.0 1.0 1.0 1.0

main :: IO ()
main = do
  -- trace mouse position
  rps <- newIORef []
  -- cache previous mouse position
  rp <- newIORef Nothing
  
  initialWindowPosition $= Position 100 100
  initialWindowSize $= Size 320 240
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode]
  createWindow progName
  displayCallback $= display rps
  reshapeCallback $= Just resize
  keyboardMouseCallback $= Just (keymouse rps rp)
  motionCallback $= Just (mouse rps rp)
  init
  mainLoop
