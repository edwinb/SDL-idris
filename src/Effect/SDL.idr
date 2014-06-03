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
     Initialise : Int -> Int -> Sdl () () (\v => Srf)
     Quit : Sdl () Srf (\v => ())
     Flip : Sdl () Srf (\v => Srf)
     Poll : Sdl (Maybe Event) a (\v => a)

     WithSurface : (Srf -> IO a) -> Sdl a Srf (\v => Srf)

instance Handler Sdl IO where
     handle () (Initialise x y) k = do srf <- startSDL x y; k () srf
     handle s Quit k = do endSDL; k () ()

     handle s Flip k = do flipBuffers s; k () s
     handle s Poll k = do x <- pollEvent; k x s
     handle s (WithSurface f) k = do r <- f s; k r s 

SDL : Type -> EFFECT
SDL res = MkEff res Sdl

SDL_ON : EFFECT
SDL_ON = SDL SDLSurface

initialise : Handler Sdl e => 
             Int -> Int -> { [SDL ()] ==> [SDL_ON] } Eff e () 
initialise x y = call $ Initialise x y

quit : Handler Sdl e =>
       { [SDL_ON] ==> [SDL ()] } Eff e () 
quit = call Quit

flip : Handler Sdl e => { [SDL_ON] } Eff e ()
flip = call Flip

poll : Handler Sdl e => { [SDL_ON] } Eff e (Maybe Event) 
poll = call Poll

getSurface : Handler Sdl e => { [SDL_ON] } Eff e SDLSurface
getSurface = call $ WithSurface (\s => return s)

rectangle : Handler Sdl e =>
            Colour -> Int -> Int -> Int -> Int -> { [SDL_ON] } Eff e () 
rectangle (MkCol r g b a) x y w h 
     = call $ WithSurface (\s => filledRect s x y w h r g b a)

ellipse : Handler Sdl e =>
          Colour -> Int -> Int -> Int -> Int -> { [SDL_ON] } Eff e () 
ellipse (MkCol r g b a) x y rx ry 
     = call $ WithSurface (\s => filledEllipse s x y rx ry r g b a)

line : Handler Sdl e =>
       Colour -> Int -> Int -> Int -> Int -> { [SDL_ON] } Eff e () 
line (MkCol r g b a) x y ex ey 
     = call $ WithSurface (\s => drawLine s x y ex ey r g b a)


