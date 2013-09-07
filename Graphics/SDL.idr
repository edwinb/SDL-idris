module SDL

%include C "sdlrun.h"
%include C "SDL/SDL.h"
%link C "sdlrun.o"
%lib C "SDL_gfx"

%flag C "`sdl-config --libs`"
%flag C "`sdl-config --cflags`"

-- Set up a window

abstract 
data SDLSurface = MkSurface Ptr

public
startSDL : Int -> Int -> IO SDLSurface
startSDL x y = do ptr <- do_startSDL
		  return (MkSurface ptr)
  where do_startSDL = mkForeign (FFun "startSDL" [FInt, FInt] FPtr) x y

public
flipBuffers : SDLSurface -> IO ();
flipBuffers (MkSurface ptr) 
     = mkForeign (FFun "flipBuffers" [FPtr] FUnit) ptr


-- Some drawing primitives

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

public
data Key = KeyUpArrow
         | KeyDownArrow
	 | KeyLeftArrow
	 | KeyRightArrow
	 | KeyAny Int

instance Eq Key where
  KeyUpArrow    == KeyUpArrow     = True
  KeyDownArrow  == KeyDownArrow   = True
  KeyLeftArrow  == KeyLeftArrow   = True
  KeyRightArrow == KeyRightArrow  = True
  (KeyAny x)    == (KeyAny y)     = x == y
  _             == _              = False

public
data Event = KeyDown Key
           | KeyUp Key
	   | AppQuit

instance Eq Event where
  (KeyDown x) == (KeyDown y) = x == y
  (KeyUp x)   == (KeyUp y)   = x == y
  AppQuit     == AppQuit     = True
  _           == _           = False

public
pollEvent : IO (Maybe Event)
pollEvent 
    = mkForeign (FFun "pollEvent" [FPtr] (FAny (Maybe Event))) prim__vm

