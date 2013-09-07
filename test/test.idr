module Main

import Graphics.SDL

main : IO ()
main = do surface <- startSDL 640 480
          flipBuffers surface

          eventLoop surface 0 320 200 0 0
  where eventLoop : SDLSurface -> Integer -> Int -> Int -> Int -> Int -> IO ()
        processEvent : SDLSurface -> Integer -> Int -> Int -> Int -> Int -> Maybe Event -> IO ()

        eventLoop s f x y mx my
                        = do event <- pollEvent
                             filledRect s 0 0 640 480 0 0 0 128
                             filledRect s 100 100 50 50 255 0 0 128
                             filledEllipse s x y 20 20 0 255 0 128
                             when ((f `mod` 100) == 0) $ print f
                             flipBuffers s
                             processEvent s (f+1) (x+mx) (y+my) mx my event

        processEvent s f x y mx my (Just (KeyDown KeyLeftArrow)) 
                = eventLoop s f x y (-1) my
        processEvent s f x y mx my (Just (KeyUp KeyLeftArrow)) 
                = eventLoop s f x y 0 my
        processEvent s f x y mx my (Just (KeyDown KeyRightArrow)) 
                = eventLoop s f x y 1 my
        processEvent s f x y mx my (Just (KeyUp KeyRightArrow)) 
                = eventLoop s f x y 0 my
        processEvent s f x y mx my (Just (KeyDown KeyUpArrow)) 
                = eventLoop s f x y mx (-1)
        processEvent s f x y mx my (Just (KeyUp KeyUpArrow)) 
                = eventLoop s f x y mx 0
        processEvent s f x y mx my (Just (KeyDown KeyDownArrow)) 
                = eventLoop s f x y mx 1
        processEvent s f x y mx my (Just (KeyUp KeyDownArrow)) 
                = eventLoop s f x y mx 0
        processEvent s f x y mx my (Just AppQuit) = return ()
        processEvent s f x y mx my (Just (KeyDown (KeyAny k)))
                = do print k
                     eventLoop s f x y mx my
        processEvent s f x y mx my (Just (MouseMotion mousex mousey _ _))
                = do print (mousex, mousey)
                     eventLoop s f x y mx my
        processEvent s f x y mx my (Just (MouseButtonUp Left mousex mousey))
                = do print (mousex, mousey)
                     eventLoop s f mousex mousey mx my
        processEvent s f x y mx my _ = eventLoop s f x y mx my


