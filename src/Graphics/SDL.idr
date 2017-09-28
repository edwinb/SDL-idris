module Graphics.SDL

import Graphics.Config
import Graphics.Utils

%include C "sdlrun.h"
%include C "SDL/SDL.h"
%link C "sdlrun.o"
%lib C "SDL_gfx"

-- Set up a window

export 
data SDLSurface = MkSurface Ptr

export
startSDL : Int -> Int -> IO (Maybe SDLSurface)
startSDL x y = do ptr <- do_startSDL
                  if !(nullPtr ptr)
                     then pure Nothing
                     else pure (Just (MkSurface ptr))
  where do_startSDL = foreign FFI_C "startSDL" (Int -> Int -> IO Ptr) x y

export
initSDL : List SDLInitFlag -> IO () 
initSDL flags = do ret <- sdl_init
                   when (ret == (the Int (-1))) (printError "SDL_init failed")
  where sdl_init = foreign FFI_C "SDL_Init" (Bits32 -> IO Int) (sdlInitflagsToBitmap flags) -- bitmap

export
setVideoMode : Int -> Int -> Int -> List SDLVideoFlag -> IO (Maybe SDLSurface)
setVideoMode width height bpp flags
     = do ptr <- do_setVideoMode
          if !(nullPtr ptr)
             then pure Nothing
             else pure (Just (MkSurface ptr)) --(Just (MkSurface prt))
   where do_setVideoMode = foreign FFI_C "SDL_SetVideoMode" (Int -> Int -> Int -> Bits32 -> IO Ptr) 
                                                             width height bpp bitmap 
         where bitmap = sdlVideoFlagsToBitmap flags 

-- export
-- loadBMP : String -> IO SDLSurface
-- loadBMP bmpPath = do
--   surface <- foreign FFI_C "SDL_LoadBMP" (String -> IO Ptr) bmpPath
--   pure $ MkSurface surface

export
loadBMP : String -> IO (Maybe SDLSurface)
loadBMP bmpPath = do surface <- do_sdl_load_bmp
                     if !(nullPtr surface) 
                        then pure Nothing 
                        else pure (Just (MkSurface surface))
   where do_sdl_load_bmp = foreign FFI_C "SDL_LoadBMP" (String -> IO Ptr) bmpPath

export
endSDL : IO ()
endSDL = foreign FFI_C "SDL_Quit" (IO ())

export
flipBuffers : SDLSurface -> IO ()
flipBuffers (MkSurface ptr) 
     = foreign FFI_C "flipBuffers" (Ptr -> IO ()) ptr



-- int SDL_UpperBlit(SDL_Surface *src, SDL_Rect *srcrect, SDL_Surface *dst, SDL_Rect *dstrect);
-- foreign import ccall unsafe "SDL_UpperBlit" sdlBlitSurface
--     :: Ptr SurfaceStruct -> Ptr Rect -> Ptr SurfaceStruct -> Ptr Rect -> IO CInt
export
blitSurface : SDLSurface -> Ptr -> SDLSurface -> Ptr -> IO () 
blitSurface (MkSurface src_ptr) src_rect (MkSurface dst_ptr) dst_rect
     = do ret <- sdl_upper_blit
          when (ret == (the Int (-1))) (printError "blitSurface failed")
  where sdl_upper_blit = 
             foreign FFI_C "SDL_UpperBlit" (Ptr -> Ptr -> Ptr -> Ptr -> IO Int) 
                                            src_ptr src_rect dst_ptr dst_rect 


-- void SDL_FreeSurface(SDL_Surface *surface);
-- foreign import ccall unsafe "SDL_FreeSurface" sdlFreeSurface :: Ptr SurfaceStruct -> IO ()
-- | Frees (deletes) a @Surface@. Don\'t use it unless you really know what you're doing. All surfaces
--   are automatically deleted when they're out of scope or forced with @finalizeForeignPtr@.
export
freeSurface : SDLSurface -> IO ()
freeSurface (MkSurface srf_ptr)
     = foreign FFI_C "SDL_FreeSurface" (Ptr -> IO ()) srf_ptr

-- Some drawing primitives
export
filledRect : SDLSurface -> Int -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int -> IO ()
filledRect (MkSurface ptr) x y w h r g b a 
      = foreign FFI_C "filledRect"
           (Ptr -> Int -> Int -> Int -> Int ->
            Int -> Int -> Int -> Int -> IO ()) ptr x y w h r g b a

export
filledEllipse : SDLSurface -> Int -> Int -> Int -> Int ->
                              Int -> Int -> Int -> Int -> IO ()
filledEllipse (MkSurface ptr) x y rx ry r g b a 
      = foreign FFI_C "filledEllipse"
           (Ptr -> Int -> Int -> Int -> Int ->
            Int -> Int -> Int -> Int -> IO ()) ptr x y rx ry r g b a

export
drawLine : SDLSurface -> Int -> Int -> Int -> Int ->
                         Int -> Int -> Int -> Int -> IO ()
drawLine (MkSurface ptr) x y ex ey r g b a 
      = foreign FFI_C "drawLine"
           (Ptr -> Int -> Int -> Int -> Int ->
            Int -> Int -> Int -> Int -> IO ()) ptr x y ex ey r g b a

-- TODO: More keys still to add... careful to do the right mappings in
-- KEY in sdlrun.c

public export
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

public export
data Button = Left | Middle | Right | WheelUp | WheelDown

Eq Button where
  Left == Left = True
  Middle == Middle = True
  Right == Right = True
  WheelUp == WheelUp = True
  WheelDown == WheelDown = True
  _ == _ = False

public export
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

public export
pollEvent : IO (Maybe Event)
pollEvent 
    = do vm <- getMyVM
         MkRaw e <- 
            foreign FFI_C "pollEvent" (Ptr -> IO (Raw (Maybe Event))) vm
         pure e

-- Utils
public export
delay : Int -> IO ()
delay ms = do foreign FFI_C "SDL_Delay" (Bits32 -> IO ()) fromMs 
   where fromMs = prim__truncInt_B32 ms
