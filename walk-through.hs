module Main where

import Prelude hiding (init)
import Graphics.UI.GLUT
import Data.Array
import Data.IORef

data Obj = Obj { tra :: Vector3 GLdouble
               , col :: Color4 GLfloat
               , obj :: Object
               , flv :: Flavour
               }

type Sec = GLdouble

data St = St { win :: Size
             , cur :: Position
             , tim :: Sec
             }
          deriving Show

scene :: IO ()
scene = do
  mapM_ renderBlock [red, green, blue, yellow]
  mapM_ renderGround [ (i, j) | i <- [-5,-4..4], j <- [-5,-4..4] ]
  where
    renderBlock :: Obj -> IO ()
    renderBlock (Obj {obj=o, flv=f, tra=t, col=c}) =
      preservingMatrix $ do
        translate t
        materialDiffuse Front $= c
        renderObject f o
    renderGround :: (Int, Int) -> IO ()
    renderGround (i, j) =
      renderPrimitive Quads $ do
        normal $ Normal3 0.0 1.0 (0.0::GLdouble)
        materialDiffuse Front $= ground!(abs(i+j)`mod`2)
        vertex3D (fromIntegral i) (-0.5) (fromIntegral j)
        vertex3D (fromIntegral i) (-0.5) (fromIntegral (j+1))
        vertex3D (fromIntegral (i+1)) (-0.5) (fromIntegral (j+1))
        vertex3D (fromIntegral (i+1)) (-0.5) (fromIntegral j)
      where
        vertex3D :: GLdouble -> GLdouble -> GLdouble -> IO ()
        vertex3D = ((vertex.).).Vertex3
    red, green, blue, yellow :: Obj
    red = Obj { obj = Torus 0.4 0.8 10 10
              , flv = Solid
              , tra = Vector3 0.0 0.0 (-3.0)
              , col = Color4 0.8 0.2 0.2 1.0
              }
    green = Obj { obj = Teapot 0.8
                , flv = Wireframe -- SolidにするとcullFace Frontになる
                , tra = Vector3 0.0 0.0 3.0
                , col = Color4 0.2 0.8 0.2 1.0
                }
    blue = Obj { obj = Cube 1.0
               , flv = Solid
               , tra = Vector3 (-3.0) 0.0 0.0
               , col = Color4 0.2 0.2 0.8 1.0
               }
    yellow = Obj { obj = Sphere' 0.7 36 36
                 , flv = Solid
                 , tra = Vector3 3.0 0.0 0.0
                 , col = Color4 0.8 0.8 0.2 1.0
                 }
    ground :: Array Int (Color4 GLfloat)
    ground = listArray (0, 1)
             [ Color4 0.6 0.6 0.6 1.0
             , Color4 0.3 0.3 0.3 1.0
             ]

display :: DisplayCallback
display = do
  let lightpos = Vertex4 3.0 4.0 5.0 1.0
  
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  lookAt
    (Vertex3 0.0 0.0 0.0)
    (Vertex3 1.0 0.0 0.0)
    (Vector3 0.0 1.0 0.0)
  position (Light 0) $= lightpos
  scene
  flush

resize :: ReshapeCallback
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 30.0 (w'/h') 1.0 100.0
  matrixMode $= Modelview 0
  where
    (w', h') = (fromIntegral w, fromIntegral h)

keyboard :: KeyboardMouseCallback
keyboard (Char '\ESC') Down _ _ = exit
keyboard (Char 'q') Down _ _ = exit
keyboard _ _ _ _ = return ()

idle :: IORef St -> IdleCallback
idle r = do
  st <- readIORef r
  writeIORef r st { tim = tim st + 0.1 }
  readIORef r >>= print -- for debug
  postRedisplay Nothing

init :: IO ()
init = do
  clearColor $= Color4 1.0 1.0 1.0 0.0
  depthFunc $= Just Lequal
  cullFace $= Just Back
  frontFace $= CCW
  lighting $= Enabled
  light (Light 0) $= Enabled

main :: IO ()
main = do
  r <- newIORef St { win = Size 640 480
                   , cur = Position 320 240
                   , tim = 0.0
                   }

  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, WithDepthBuffer]
  createWindow progName
  displayCallback $= display
  reshapeCallback $= Just resize
  keyboardMouseCallback $= Just keyboard
  idleCallback $= Just (idle r)
  init
  mainLoop
