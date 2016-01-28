module Graphics.SDL

import Graphics.Config

%include C "sdlrun.h"
%include C "SDL/SDL.h"
%link C "sdlrun.o"
%lib C "SDL_gfx"

-- Set up a window

||| A reference to an SDL surface, that is, a window.
abstract
data SDLSurface = MkSurface Ptr

||| Start SDL running in a window.
|||
||| @ x the width of the SDL window.
||| @ y the height of the SDL window.
public
startSDL : (x, y : Int) -> IO SDLSurface
startSDL x y = do ptr <- do_startSDL
		  return (MkSurface ptr)
  where do_startSDL = foreign FFI_C "startSDL" (Int -> Int -> IO Ptr) x y

||| Quit SDL.
public
endSDL : IO ()
endSDL = foreign FFI_C "SDL_Quit" (IO ())

||| Flip the drawing and displayed buffers.
public
flipBuffers : SDLSurface -> IO ();
flipBuffers (MkSurface ptr) 
     = foreign FFI_C "flipBuffers" (Ptr -> IO ()) ptr


-- Some drawing primitives

||| Draw a filled rectangle.
||| @ x the horizontal position of the upper-left corner.
||| @ y the vertical position of the upper-left corner.
||| @ w the width of the rectangle.
||| @ r how much red to use (0-255)
||| @ g how much green to use (0-255)
||| @ b how much blue to use (0-255)
||| @ a how much alpha to use (0-255)
public
filledRect : SDLSurface ->
             (x, y, w, h, r, g, b, a : Int) ->
             IO ()
filledRect (MkSurface ptr) x y w h r g b a 
      = foreign FFI_C "filledRect"
           (Ptr -> Int -> Int -> Int -> Int ->
            Int -> Int -> Int -> Int -> IO ()) ptr x y w h r g b a

||| Draw a filled ellipse.
|||
||| @ x the horiontal position of the center
||| @ y the vertical position of the center
||| @ rx the horizontal distance from the center to the edge
||| @ ry the vertical distance from the center to the edge
||| @ r how much red to use (0-255)
||| @ g how much green to use (0-255)
||| @ b how much blue to use (0-255)
||| @ a how much alpha to use (0-255)
public
filledEllipse : SDLSurface ->
                (x, y, rx, ry, r, g, b, a : Int) ->
                IO ()
filledEllipse (MkSurface ptr) x y rx ry r g b a 
      = foreign FFI_C "filledEllipse"
           (Ptr -> Int -> Int -> Int -> Int ->
            Int -> Int -> Int -> Int -> IO ()) ptr x y rx ry r g b a

public
drawLine : SDLSurface -> Int -> Int -> Int -> Int ->
                         Int -> Int -> Int -> Int -> IO ()
drawLine (MkSurface ptr) x y ex ey r g b a 
      = foreign FFI_C "drawLine"
           (Ptr -> Int -> Int -> Int -> Int ->
            Int -> Int -> Int -> Int -> IO ()) ptr x y ex ey r g b a

-- TODO: More keys still to add... careful to do the right mappings in
-- KEY in sdlrun.c
||| SDL keys
public
data Key = KeyUpArrow
         | KeyDownArrow
	 | KeyLeftArrow
	 | KeyRightArrow
         | KeyEsc
         | KeySpace
         | KeyTab
         | KeyF1
         | KeyF2
         | KeyF3
         | KeyF4
         | KeyF5
         | KeyF6
         | KeyF7
         | KeyF8
         | KeyF9
         | KeyF10
         | KeyF11
         | KeyF12
         | KeyF13
         | KeyF14
         | KeyF15
         | KeyLShift
         | KeyRShift
         | KeyLCtrl
         | KeyRCtrl
	 | KeyAny Char

Eq Key where
  KeyUpArrow    == KeyUpArrow     = True
  KeyDownArrow  == KeyDownArrow   = True
  KeyLeftArrow  == KeyLeftArrow   = True
  KeyRightArrow == KeyRightArrow  = True

  KeyEsc   == KeyEsc   = True
  KeyTab   == KeyTab   = True
  KeySpace == KeySpace = True

  KeyF1    == KeyF1    = True
  KeyF2    == KeyF2    = True
  KeyF3    == KeyF3    = True
  KeyF4    == KeyF4    = True
  KeyF5    == KeyF5    = True
  KeyF6    == KeyF6    = True
  KeyF7    == KeyF7    = True
  KeyF8    == KeyF8    = True
  KeyF9    == KeyF9    = True
  KeyF10   == KeyF10   = True
  KeyF11   == KeyF11   = True
  KeyF12   == KeyF12   = True
  KeyF13   == KeyF13   = True
  KeyF14   == KeyF14   = True
  KeyF15   == KeyF15   = True

  KeyLShift == KeyLShift = True
  KeyRShift == KeyRShift = True
  KeyLCtrl  == KeyLCtrl  = True
  KeyRCtrl  == KeyRCtrl  = True

  (KeyAny x)    == (KeyAny y)     = x == y
  _             == _              = False

||| Mouse button presses
public
data Button = Left | Middle | Right | WheelUp | WheelDown

Eq Button where
  Left == Left = True
  Middle == Middle = True
  Right == Right = True
  WheelUp == WheelUp = True
  WheelDown == WheelDown = True
  _ == _ = False

||| SDL events.
public
data Event = KeyDown Key
           | KeyUp Key
           | MouseMotion Int Int Int Int
           | MouseButtonDown Button Int Int
           | MouseButtonUp Button Int Int
           | Resize Int Int
	   | AppQuit

Eq Event where
  (KeyDown x) == (KeyDown y) = x == y
  (KeyUp x)   == (KeyUp y)   = x == y
  AppQuit     == AppQuit     = True
  (MouseMotion x y rx ry) == (MouseMotion x' y' rx' ry')
      = x == x' && y == y' && rx == rx' && ry == ry'
  (MouseButtonDown b x y) == (MouseButtonDown b' x' y')
      = b == b' && x == x' && y == y'
  (MouseButtonUp b x y) == (MouseButtonUp b' x' y')
      = b == b' && x == x' && y == y'
  _           == _           = False

||| Poll for events.
public
pollEvent : IO (Maybe Event)
pollEvent 
    = do MkRaw e <- 
            foreign FFI_C "pollEvent" (Ptr -> IO (Raw (Maybe Event))) prim__vm
         return e

