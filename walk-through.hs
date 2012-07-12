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
             , vel :: Vector3 GLdouble -- ワールド座標系での移動ベクトル
             , pos :: Vertex3 GLdouble -- ワールド座標系での位置
             }
        deriving Show

data Pole = Pole { r :: GLdouble
                 , theta :: GLdouble
                 }
        deriving Show

-- マウスカーソルの位置から自分視点での移動ベクトルを求める
direction :: Size -> Position -> Vector3 GLdouble
direction (Size w h) (Position x z) = Vector3 (x'-w'/2) 0.0 (h'/2-z')
  where
    (w', h') = (fromIntegral w, fromIntegral h)
    (x', z') = (fromIntegral x, fromIntegral z)
    v = sqrt $ x'^2+z'^2

-- 自分視点の移動ベクトルをワールド座標系での移動ベクトルに変換する.
-- ただし自分視点は直前の移動ベクトル基準なので変換が入る.
-- 
-- すでにワールド座標系で(x0, 0, z0)ベクトルで移動中だとする.
-- これはv0=√x0^2+y0^2の速さでsinθ=x0/v0,cosθ=z0/v0なる時計回りに角度θで移動していることになる.
-- 現在の自分視点はこのθ傾いた状態をz軸とみなした状態での移動ベクトルとなる.
-- 移動ベクトルを(x1, 0, z1)とすると、v1=√x1^2+z1^2でsinφ=x1/v1,cosφ=z1/v1なる時計回りの角度φで進行しようとしている.
-- この移動ベクトルはワールド座標系では(θ+φ)だけ傾いた方向への移動にあたる.
-- (x1, 0, z1)をワールド座標系に落した移動ベクトルを(x', 0, z')とすると,
-- sin(θ+φ)=x'/v1, cos(θ+φ)=z'/v1が成立している.(図を書け)
-- 予備式としては,
--   v0 = √x0^2+z0^2
--   sinθ=x0/v0
--   cosθ=z0/v0
--   v1 = √x1^2+z1^2
--   sinφ=x1/v1
--   cosφ=z1/v1
--
-- 三角関数の加法定理から,
--   sin(θ+φ)=x'/v1 = sinθcosφ+sinθcosφ = x0/v0*z1/v1+x1/v1*z0/v0
--             x' = (x0*z1+x1*z0)/v0
--   cos(θ+φ)=z'/v1 = cosθcosφ-sinθsinφ = z0/v0*z1/v1-x0/v0*x1/v1
--             z' = (z0*z1-x0x1)/v0
-- ∴ワールド座標系での移動ベクトルは (x', 0, z') = ((x0*z1+x1*z0)/v0, 0, (z0*z1-x0*x1)/v0)
--
direction' :: Vector3 GLdouble -> Vector3 GLdouble -> Vector3 GLdouble
direction' (Vector3 x0 _ z0) (Vector3 x1 _ z1) = Vector3 ((x0*z1+x1*z0)/v0) 0.0 ((z0*z1-x0*x1)/v0)
  where
    v0 = sqrt $ x0^2+z0^2

toPole :: Vector3 GLdouble -> Pole
toPole (Vector3 x y z) = Pole { r=r', theta=theta' }
  where
    r' = sqrt $ x^2+z^2
    theta' = asin (x/r') -- = acos (z/r')

deg2rad :: GLdouble -> GLdouble
deg2rad deg = deg*pi/180

rad2deg :: GLdouble -> GLdouble
rad2deg rad = rad*180/pi

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
display r = do
  st <- readIORef r
  print st

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

resize :: IORef St -> ReshapeCallback
resize r s@(Size w h) = do
  st <- readIORef r
  writeIORef r st { win=s }

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

mouse :: IORef St -> MotionCallback
mouse r c = do
  st <- readIORef r
  writeIORef r st { cur=c }
  postRedisplay Nothing

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
                   , vel = Vector3 0.0 0.0 0.0
                   , pos = Vertex3 0.0 0.0 0.0
                   }

  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, WithDepthBuffer]
  createWindow progName
  displayCallback $= display r
  reshapeCallback $= Just (resize r)
  keyboardMouseCallback $= Just keyboard
  idleCallback $= Just (idle r)
  passiveMotionCallback $= Just (mouse r)
  init
  mainLoop
