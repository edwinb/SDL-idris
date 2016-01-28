module Effect.SDL

import Effects
import public Graphics.SDL

Srf : Type
Srf = SDLSurface

||| A colour
data Colour : Type where
  ||| Construct a colour from its components.
  ||| @ r how much red? (0-255)
  ||| @ g how much red? (0-255)
  ||| @ b how much red? (0-255)
  ||| @ a how much alpha? (0-255)
  MkCol : (r, g, b, a : Int) -> Colour

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

||| A running SDL program
SDL_ON : EFFECT
SDL_ON = SDL SDLSurface

||| Initialise the SDL window at some size.
|||
||| @ x the width
||| @ y the height
initialise : (x, y : Int) -> { [SDL ()] ==> [SDL_ON] } Eff () 
initialise x y = call $ Initialise x y

||| Stop the SDL program.
quit : { [SDL_ON] ==> [SDL ()] } Eff () 
quit = call Quit

||| Flip the buffers.
flip : { [SDL_ON] } Eff ()
flip = call Flip

||| Get the current event, if there is one. Don't block.
poll : { [SDL_ON] } Eff (Maybe Event) 
poll = call Poll

||| Get ahold of the SDL surface for low-level manipulation.
getSurface : { [SDL_ON] } Eff SDLSurface
getSurface = call $ WithSurface (\s => return s)

||| Draw a rectangle.
|||
||| @ x the horizontal position of the upper-left corner of the rectangle
||| @ y the vertical position of the upper-left corner of the rectangle
||| @ w the width of the rectangle
||| @ h the height of the rectangle
rectangle : Colour -> (x, y : Int) -> (w, h : Int) -> { [SDL_ON] } Eff () 
rectangle (MkCol r g b a) x y w h 
     = call $ WithSurface (\s => filledRect s x y w h r g b a)

||| Draw an ellipse.
|||
||| @ x the horizontal position of the center of the ellipse
||| @ y the vertical position of the center of the ellipse
||| @ rx the horizontal distance from the center to the edge
||| @ ry the vertical distance from the center to the edge
ellipse : Colour -> (x, y : Int) -> (rx, ry : Int) -> { [SDL_ON] } Eff () 
ellipse (MkCol r g b a) x y rx ry 
     = call $ WithSurface (\s => filledEllipse s x y rx ry r g b a)

||| Draw a line.
|||
||| @ x the horizontal position of the start of the lin
||| @ y the vertical position of the start of the line
||| @ ex the horizontal position of the end of the lin
||| @ ey the vertical position of the end of the line
line : Colour -> (x, y : Int) -> (ex, ey : Int) -> { [SDL_ON] } Eff () 
line (MkCol r g b a) x y ex ey 
     = call $ WithSurface (\s => drawLine s x y ex ey r g b a)


