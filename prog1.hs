module Main where

import Graphics.UI.GLUT

display :: IO ()
display = return ()

main :: IO ()
main = do
  (progName, _) <- getArgsAndInitialize
  createWindow progName
  displayCallback $= display
  mainLoop
