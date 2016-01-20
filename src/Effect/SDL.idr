module Effect.SDL

import Effects
import public Graphics.SDL

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

Handler Sdl IO where
     handle () (Initialise x y) k = do srf <- startSDL x y; k () srf
     handle s Quit k = do endSDL; k () ()

     handle s Flip k = do flipBuffers s; k () s
     handle s Poll k = do x <- pollEvent; k x s
     handle s (WithSurface f) k = do r <- f s; k r s 

SDL : Type -> EFFECT
SDL res = MkEff res Sdl

SDL_ON : EFFECT
SDL_ON = SDL SDLSurface

initialise : Int -> Int -> { [SDL ()] ==> [SDL_ON] } Eff () 
initialise x y = call $ Initialise x y

quit : { [SDL_ON] ==> [SDL ()] } Eff () 
quit = call Quit

flip : { [SDL_ON] } Eff ()
flip = call Flip

poll : { [SDL_ON] } Eff (Maybe Event) 
poll = call Poll

getSurface : { [SDL_ON] } Eff SDLSurface
getSurface = call $ WithSurface (\s => return s)

rectangle : Colour -> Int -> Int -> Int -> Int -> { [SDL_ON] } Eff () 
rectangle (MkCol r g b a) x y w h 
     = call $ WithSurface (\s => filledRect s x y w h r g b a)

ellipse : Colour -> Int -> Int -> Int -> Int -> { [SDL_ON] } Eff () 
ellipse (MkCol r g b a) x y rx ry 
     = call $ WithSurface (\s => filledEllipse s x y rx ry r g b a)

line : Colour -> Int -> Int -> Int -> Int -> { [SDL_ON] } Eff () 
line (MkCol r g b a) x y ex ey 
     = call $ WithSurface (\s => drawLine s x y ex ey r g b a)


