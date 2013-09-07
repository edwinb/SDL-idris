module Main

import SDL

main : IO ()
main = do surface <- startSDL 640 480
          flipBuffers surface

          eventLoop surface 320 200 0 0
  where eventLoop : SDLSurface -> Int -> Int -> Int -> Int -> IO ()
        processEvent : SDLSurface -> Int -> Int -> Int -> Int -> Maybe Event -> IO ()

        eventLoop s x y mx my
                        = do event <- pollEvent
                             filledRect s 0 0 640 480 0 0 0 128
                             filledRect s 100 100 50 50 255 0 0 128
                             filledEllipse s x y 20 20 0 255 0 128
                             flipBuffers s
                             processEvent s (x+mx) (y+my) mx my event

        processEvent s x y mx my (Just (KeyDown KeyLeftArrow)) 
                = eventLoop s x y (-1) my
        processEvent s x y mx my (Just (KeyUp KeyLeftArrow)) 
                = eventLoop s x y 0 my
        processEvent s x y mx my (Just (KeyDown KeyRightArrow)) 
                = eventLoop s x y 1 my
        processEvent s x y mx my (Just (KeyUp KeyRightArrow)) 
                = eventLoop s x y 0 my
        processEvent s x y mx my (Just (KeyDown KeyUpArrow)) 
                = eventLoop s x y mx (-1)
        processEvent s x y mx my (Just (KeyUp KeyUpArrow)) 
                = eventLoop s x y mx 0
        processEvent s x y mx my (Just (KeyDown KeyDownArrow)) 
                = eventLoop s x y mx 1
        processEvent s x y mx my (Just (KeyUp KeyDownArrow)) 
                = eventLoop s x y mx 0
        processEvent s x y mx my (Just AppQuit) = return ()
        processEvent s x y mx my _ = eventLoop s x y mx my


