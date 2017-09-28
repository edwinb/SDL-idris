module Main

import Graphics.SDL as SDL
import Graphics.Utils as Utils
import System as System

main : IO ()
main = do
  initSDL [Everything]
  screenMaybe <- setVideoMode 640 480 32 [SWSurface]
  bmpMaybe    <- loadBMP "hello.bmp"
  case (screenMaybe, bmpMaybe) of
    (Just screen, Just bmp) => do blitSurface bmp null screen null  
                                  flipBuffers screen 
    (Nothing    , Just _  ) => abortWithMessage "setVideoMode"
    (Just _     , Nothing ) => abortWithMessage "loadBMP"
  usleep 1000000
  usleep 1000000
