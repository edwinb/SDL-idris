module Main

import Effects

import Effect.SDL
import Effect.State
import Effect.StdIO

data Vars = Position | XMove | YMove | Frames

Prog : Type -> Type -> Type
Prog i t = Eff IO [SDL i, 
                   Position ::: STATE (Int, Int), 
                   XMove ::: STATE Int,
                   YMove ::: STATE Int,
                   Frames ::: STATE Integer,
                   STDIO] t

Running : Type -> Type
Running t = Prog SDLSurface t

emain : Prog () ()
emain = do initialise 640 480
           Position :- put (320, 200)
           eventLoop
           quit
  where process : Maybe Event -> Running Bool
        process (Just AppQuit) = return False

        process (Just (KeyDown KeyLeftArrow))  = do XMove :- put (-1)
                                                    return True
        process (Just (KeyUp KeyLeftArrow))    = do XMove :- put 0
                                                    return True
        process (Just (KeyDown KeyRightArrow)) = do XMove :- put 1
                                                    return True
        process (Just (KeyUp KeyRightArrow))   = do XMove :- put 0
                                                    return True
        process (Just (KeyDown KeyUpArrow))    = do YMove :- put (-1)
                                                    return True
        process (Just (KeyUp KeyUpArrow))      = do YMove :- put 0
                                                    return True
        process (Just (KeyDown KeyDownArrow))  = do YMove :- put 1
                                                    return True
        process (Just (KeyUp KeyDownArrow))    = do YMove :- put 0
                                                    return True
        process _ = return True
  
        draw : Running ()
        draw = do rectangle black 0 0 640 480
                  rectangle red 50 50 50 50
                  (x, y) <- Position :- get
                  ellipse green x y 20 20
                  flip

        update : Running ()
        update = do xm <- XMove :- get
                    ym <- YMove :- get
                    (x, y) <- Position :- get
                    Position :- put (x + xm, y + ym)
                    f <- Frames :- get
                    Frames :- put (f + 1)
                    when ((f `mod` 100) == 0) (putStrLn (show f))

        eventLoop : Running ()
        eventLoop = do draw
                       update
                       e <- poll
                       continue <- process e
                       when continue eventLoop

main : IO ()
main = run [(), Position := (320,200), 
                XMove := 0, 
                YMove := 0, 
                Frames := 0,
                ()] emain


