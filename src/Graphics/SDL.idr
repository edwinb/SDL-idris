module Graphics.SDL

import Graphics.Config

%include C "sdlrun.h"
%include C "SDL/SDL.h"
%link C "sdlrun.o"
%lib C "SDL_gfx"

-- Set up a window

abstract 
data SDLSurface = MkSurface Ptr

public
startSDL : Int -> Int -> IO SDLSurface
startSDL x y = do ptr <- do_startSDL
		  return (MkSurface ptr)
  where do_startSDL = mkForeign (FFun "startSDL" [FInt, FInt] FPtr) x y

public
endSDL : IO ()
endSDL = mkForeign (FFun "SDL_Quit" [] FUnit)

public
flipBuffers : SDLSurface -> IO ();
flipBuffers (MkSurface ptr) 
     = mkForeign (FFun "flipBuffers" [FPtr] FUnit) ptr


-- Some drawing primitives

public
pixel : SDLSurface -> Int -> Int ->
                      Int -> Int -> Int -> Int -> IO ()
pixel (MkSurface ptr) x y r g b a
      = mkForeign (FFun "pixel" [FPtr, FInt, FInt,
                                       FInt, FInt, FInt, FInt] FUnit)
                  ptr x y r g b a

public
filledRect : SDLSurface -> Int -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int -> IO ()
filledRect (MkSurface ptr) x y w h r g b a 
      = mkForeign (FFun "filledRect" [FPtr, FInt, FInt, FInt, FInt,
                                            FInt, FInt, FInt, FInt] FUnit)
                  ptr x y w h r g b a

public
filledEllipse : SDLSurface -> Int -> Int -> Int -> Int ->
                              Int -> Int -> Int -> Int -> IO ()
filledEllipse (MkSurface ptr) x y rx ry r g b a 
      = mkForeign (FFun "filledEllipse" [FPtr, FInt, FInt, FInt, FInt,
                                               FInt, FInt, FInt, FInt] FUnit)
                  ptr x y rx ry r g b a

public
drawLine : SDLSurface -> Int -> Int -> Int -> Int ->
                         Int -> Int -> Int -> Int -> IO ()
drawLine (MkSurface ptr) x y ex ey r g b a 
      = mkForeign (FFun "drawLine" [FPtr, FInt, FInt, FInt, FInt,
                                          FInt, FInt, FInt, FInt] FUnit)
                  ptr x y ex ey r g b a

-- TODO: More keys still to add... careful to do the right mappings in
-- KEY in sdlrun.c

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

instance Eq Key where
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

public
data Button = Left | Middle | Right | WheelUp | WheelDown

instance Eq Button where
  Left == Left = True
  Middle == Middle = True
  Right == Right = True
  WheelUp == WheelUp = True
  WheelDown == WheelDown = True
  _ == _ = False

public
data Event = KeyDown Key
           | KeyUp Key
           | MouseMotion Int Int Int Int
           | MouseButtonDown Button Int Int
           | MouseButtonUp Button Int Int
           | Resize Int Int
	   | AppQuit

instance Eq Event where
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

public
pollEvent : IO (Maybe Event)
pollEvent 
    = mkForeign (FFun "pollEvent" [FPtr] (FAny (Maybe Event))) prim__vm

