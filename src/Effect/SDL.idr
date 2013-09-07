module Effect.SDL

import Effects
import Graphics.SDL

Srf : Type
Srf = SDLSurface

data Colour = MkCol Int Int Int Int

black : Colour
black = MkCol 0 0 0 255

white : Colour
white = MkCol 255 255 255 255

red : Colour
red = MkCol 255 0 0 255

green : Colour
green = MkCol 0 255 0 255

blue : Colour
blue = MkCol 0 0 255 255

yellow : Colour
yellow = MkCol 255 255 0 255

cyan : Colour
cyan = MkCol 0 255 255 255

magenta : Colour
magenta = MkCol 255 0 255 255

data Sdl : Effect where
     Initialise : Int -> Int -> Sdl () Srf ()
     Quit : Sdl Srf () ()
     Flip : Sdl Srf Srf ()
     Poll : Sdl a a (Maybe Event)

     WithSurface : (Srf -> IO a) -> Sdl Srf Srf a

instance Handler Sdl IO where
     handle () (Initialise x y) k = do srf <- startSDL x y; k srf ()
     handle s Quit k = do endSDL; k () ()

     handle s Flip k = do flipBuffers s; k s ()
     handle s Poll k = do x <- pollEvent; k s x
     handle s (WithSurface f) k = do r <- f s; k s r 

SDL : Type -> EFFECT
SDL res = MkEff res Sdl

SDL_ON : EFFECT
SDL_ON = SDL SDLSurface

initialise : Handler Sdl e => 
             Int -> Int -> EffM e [SDL ()] [SDL_ON] ()
initialise x y = Initialise x y

quit : Handler Sdl e =>
       EffM e [SDL_ON] [SDL ()] ()
quit = Quit

flip : Handler Sdl e => Eff e [SDL_ON] ()
flip = Flip

poll : Handler Sdl e => Eff e [SDL_ON] (Maybe Event)
poll = Poll

rectangle : Handler Sdl e =>
            Colour -> Int -> Int -> Int -> Int -> Eff e [SDL_ON] ()
rectangle (MkCol r g b a) x y w h 
     = WithSurface (\s => filledRect s x y w h r g b a)

ellipse : Handler Sdl e =>
          Colour -> Int -> Int -> Int -> Int -> Eff e [SDL_ON] ()
ellipse (MkCol r g b a) x y rx ry 
     = WithSurface (\s => filledEllipse s x y rx ry r g b a)

line : Handler Sdl e =>
       Colour -> Int -> Int -> Int -> Int -> Eff e [SDL_ON] ()
line (MkCol r g b a) x y ex ey 
     = WithSurface (\s => drawLine s x y ex ey r g b a)


