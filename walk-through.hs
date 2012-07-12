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

deg2rad :: GLdouble -> GLdouble 
deg2rad deg = deg*pi/180

data St = St { win :: Size
             , cur :: Position
             , r :: GLdouble -- 視点の向き
             , v :: GLdouble -- 進行方向に対する速度
             , theta :: GLdouble -- 角度
             , t :: Sec -- 時間
             , vel :: Vector3 GLdouble -- 速度ベクトル
             , pos :: Vector3 GLdouble -- 視点の位置
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

display :: IORef St -> DisplayCallback
display ref = do
  st@St { v=v'
        , t=t'
        , theta=theta'
        , r=r'
        , vel=Vector3 vx vy vz
        , pos = Vector3 ex ey ez } <- readIORef ref
  
  let r'' = theta'*t'
      (vx'', vz'') = (-v'*sin(deg2rad (abs r'))*signum r', v'*cos(deg2rad(abs r')))
      (ex'', ez'') = (ex+0.01*vx''*t', ez+0.01*vz''*t')
      
  writeIORef ref st { r=r''
                    , vel=Vector3 vx'' vy vz''
                    , pos=Vector3 ex'' ey ez''
                    }
  
  let lightpos = Vertex4 3.0 4.0 5.0 1.0
  
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  rotate r'' (Vector3 0.0 1.0 0.0)
  translate $ Vector3 ex'' 0.0 ez''
  position (Light 0) $= lightpos
  scene
  swapBuffers

resize :: IORef St -> ReshapeCallback
resize r s@(Size w h) = do
  st <- readIORef r
  writeIORef r st { win=s }

  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 60.0 (w'/h') 1.0 1000.0
  matrixMode $= Modelview 0
  where
    (w', h') = (fromIntegral w, fromIntegral h)

keyboard :: KeyboardMouseCallback
keyboard (Char '\ESC') Down _ _ = exit
keyboard (Char 'q') Down _ _ = exit
keyboard _ _ _ _ = return ()

mouse :: IORef St -> MotionCallback
mouse r c@(Position x y) = do
  st@St { win=Size w h } <- readIORef r
  let st' = st { cur = c, theta = theta', v = v' }
      theta' = signum (x'-w'/2) * abs (1.0-2*x'/w') * 2
      v' = signum (h'/2-y') * abs (h'/2-y') / 2000
      (x', y') = (fromIntegral x, fromIntegral y)
      (w', h') = (fromIntegral w, fromIntegral h)
  writeIORef r st'
  postRedisplay Nothing

idle :: IORef St -> IdleCallback
idle r = do
  st <- readIORef r
  writeIORef r st { t = t st + 0.1 }
  postRedisplay Nothing

init :: IO ()
init = do
  clearColor $= Color4 1.0 1.0 1.0 0.0
  depthFunc $= Just Lequal
  cullFace $= Just Back
  frontFace $= CCW
  lighting $= Enabled
  light (Light 0) $= Enabled


initSt = St { win = Size 640 480
            , cur = Position 320 220
            , t = 0.0
            , v = 0.0
            , theta = 0.0
            , r = 0.0
            , vel = Vector3 0.0 0.0 0.0
            , pos = Vector3 0.0 0.0 0.0
            }

main :: IO ()
main = do
  r <- newIORef initSt

  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  createWindow progName
  displayCallback $= display r
  reshapeCallback $= Just (resize r)
  keyboardMouseCallback $= Just keyboard
  idleCallback $= Just (idle r)
  passiveMotionCallback $= Just (mouse r)
  init
  mainLoop
