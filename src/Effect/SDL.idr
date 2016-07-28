module Effect.SDL

import Effects
import public Graphics.SDL

public export
data Colour = MkCol Int Int Int Int

export
black : Colour
black = MkCol 0 0 0 255

export
white : Colour
white = MkCol 255 255 255 255

export
red : Colour
red = MkCol 255 0 0 255

export
green : Colour
green = MkCol 0 255 0 255

export
blue : Colour
blue = MkCol 0 0 255 255

export
yellow : Colour
yellow = MkCol 255 255 0 255

export
cyan : Colour
cyan = MkCol 0 255 255 255

export
magenta : Colour
magenta = MkCol 255 0 255 255

Srf : Type
Srf = SDLSurface

export
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

public export
SDL : Type -> EFFECT
SDL res = MkEff res Sdl

public export
SDL_ON : EFFECT
SDL_ON = SDL SDLSurface

export
initialise : Int -> Int -> { [SDL ()] ==> [SDL_ON] } Eff ()
initialise x y = call $ Initialise x y

export
quit : { [SDL_ON] ==> [SDL ()] } Eff ()
quit = call Quit

export
flip : { [SDL_ON] } Eff ()
flip = call Flip

export
poll : { [SDL_ON] } Eff (Maybe Event)
poll = call Poll

export
getSurface : { [SDL_ON] } Eff SDLSurface
getSurface = call $ WithSurface (\s => return s)

export
rectangle : Colour -> Int -> Int -> Int -> Int -> { [SDL_ON] } Eff ()
rectangle (MkCol r g b a) x y w h
     = call $ WithSurface (\s => filledRect s x y w h r g b a)

export
ellipse : Colour -> Int -> Int -> Int -> Int -> { [SDL_ON] } Eff ()
ellipse (MkCol r g b a) x y rx ry
     = call $ WithSurface (\s => filledEllipse s x y rx ry r g b a)

export
line : Colour -> Int -> Int -> Int -> Int -> { [SDL_ON] } Eff ()
line (MkCol r g b a) x y ex ey
     = call $ WithSurface (\s => drawLine s x y ex ey r g b a)
