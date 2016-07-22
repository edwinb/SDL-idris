module Main

import Graphics.SDL

main : IO ()
main = (do
  surface <- startSDL 640 480
  rest surface)
    where
      rest : SDLSurface -> IO ()
      rest surface = do
        filledRect surface 100 100 50 50 255 0 0 128
        flipBuffers surface
        event <- pollEvent
        case event of
          Just (KeyDown KeyEscape) => pure ()
          Just (AppQuit) => pure ()
          _ => rest surface
