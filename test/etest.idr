module Main

import Effects

import Effect.SDL
import Effect.State
import Effect.StdIO
import Effect.Random

data Vars = Position | XMove | YMove | Frames | Starfield

Prog : Type -> Type -> Type
Prog i t = Eff IO [SDL i, 
                   Position ::: STATE (Int, Int), 
                   XMove ::: STATE Int,
                   YMove ::: STATE Int,
                   Frames ::: STATE Integer,
                   Starfield ::: STATE (List (Int, Int)),
                   RND,
                   STDIO] t

Running : Type -> Type
Running t = Prog SDLSurface t

initStarfield : List (Int, Int) -> Int -> Eff m [RND] (List (Int, Int))
initStarfield acc 0 = return acc
initStarfield acc n = do x <- rndInt 0 639
                         y <- rndInt 0 479
                         initStarfield ((fromInteger x, fromInteger y) :: acc) (n - 1)

updateStarfield : List (Int, Int) -> Eff m [RND] (List (Int, Int))
updateStarfield xs = upd [] xs where
  upd : List (Int, Int) -> List (Int, Int) -> Eff m [RND] (List (Int, Int))
  upd acc [] = return acc
  upd acc ((x, y) :: xs)
      = if (y > 479) then do
             x <- rndInt 0 639
             upd ((fromInteger x, 0) :: acc) xs
           else
             upd ((x, y+1) :: acc) xs

drawStarfield : List (Int, Int) -> Eff IO [SDL_ON] ()
drawStarfield [] = return ()
drawStarfield ((x, y) :: xs) = do line white x y x y
                                  drawStarfield xs

emain : Prog () ()
emain = do initialise 640 480
           Position :- put (320, 200)
           s <- initStarfield [] 100
           Starfield :- put s
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
                  rectangle cyan 50 50 50 50
                  (x, y) <- Position :- get
                  ellipse yellow x y 20 20
                  s <- Starfield :- get
                  drawStarfield s
                  flip

        update : Running ()
        update = do xm <- XMove :- get
                    ym <- YMove :- get
                    (x, y) <- Position :- get
                    Position :- put (x + xm, y + ym)
                    f <- Frames :- get
                    s <- Starfield :- get
                    s' <- updateStarfield s
                    Starfield :- put s'
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
                Starfield := List.Nil,
                1234567890,
                ()] emain


