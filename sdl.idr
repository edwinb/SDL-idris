module SDL

%include C "sdlrun.h"
%include C "SDL/SDL.h"
%link C "sdlrun.o"
%lib C "SDL_gfx"

%flag C "`sdl-config --libs`"
%flag C "`sdl-config --cflags`"

-- Set up a window

data SDLSurface = MkSurface Ptr

do_startSDL : Int -> Int -> IO Ptr
do_startSDL x y = mkForeign (FFun "startSDL" [FInt, FInt] FPtr) x y

startSDL : Int -> Int -> IO SDLSurface
startSDL x y = do ptr <- do_startSDL x y
		  return (MkSurface ptr)

-- Some drawing primitives

{-
_filledRect = mkForeign (FFun "filledRect"
         (Cons FPtr (Cons FInt (Cons FInt (Cons FInt (Cons FInt
		    (Cons FInt (Cons FInt (Cons FInt (Cons FInt Nil)))))))))
	 FUnit) %eval;

filledRect : SDLSurface -> Int -> Int -> Int -> Int ->
			 Int -> Int -> Int -> Int -> IO ();
filledRect (mkSurface ptr) x y w h r g b a = _filledRect ptr x y w h r g b a;

_filledEllipse = mkForeign (FFun "filledEllipse"
         (Cons FPtr (Cons FInt (Cons FInt (Cons FInt (Cons FInt
		    (Cons FInt (Cons FInt (Cons FInt (Cons FInt Nil)))))))))
	 FUnit) %eval;

filledEllipse : SDLSurface -> Int -> Int -> Int -> Int ->
			 Int -> Int -> Int -> Int -> IO ();
filledEllipse (mkSurface ptr) x y w h r g b a = _filledEllipse ptr x y w h r g b a;

_drawLine = mkForeign (FFun "drawLine"
         (Cons FPtr (Cons FInt (Cons FInt (Cons FInt (Cons FInt
		    (Cons FInt (Cons FInt (Cons FInt (Cons FInt Nil)))))))))
	 FUnit) %eval;

drawLine : SDLSurface -> Int -> Int -> Int -> Int ->
			 Int -> Int -> Int -> Int -> IO ();
drawLine (mkSurface ptr) x y ex ey r g b a = _drawLine ptr x y ex ey r g b a;

_flip = mkForeign (FFun "flipBuffers" (Cons FPtr Nil) FUnit) %eval;

flipBuffers : SDLSurface -> IO ();
flipBuffers (mkSurface ptr) = _flip ptr;

-- Events

data Key = KeyUpArrow
         | KeyDownArrow
	 | KeyLeftArrow
	 | KeyRightArrow
	 | KeyAny Int;

keyEq : Key -> Key -> Bool;
keyEq KeyUpArrow KeyUpArrow = True;
keyEq KeyDownArrow KeyDownArrow = True;
keyEq KeyLeftArrow KeyLeftArrow = True;
keyEq KeyRightArrow KeyRightArrow = True;
keyEq (KeyAny x) (KeyAny y) = x == y;
keyEq _ _ = False;

data Event = KeyDown Key
           | KeyUp Key
	   | AppQuit;

pollEvent = mkForeign (FFun "pollEvent" Nil (FAny (Maybe Event))) %eval;
-}
