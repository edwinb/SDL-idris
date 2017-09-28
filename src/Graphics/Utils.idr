module Graphics.Utils

import Data.Vect
import System

%default total

export 
getError : IO String
getError = foreign FFI_C "SDL_GetError" (IO String)

-- Error
export 
printError : (msg: String) -> IO ()
printError msg = do
  err <- getError
  fPutStr stderr $ msg ++ " failed:" ++ err
  fflush stderr
  --System.exit 1

export 
abortWithMessage: (msg: String) -> IO ()
abortWithMessage msg = do
  err <- getError
  fPutStr stderr $ msg ++ " failed:" ++ err
  fflush stderr
  System.exit 1

-- Flag Utils
isFlagEnabled : (bitMaskFromFlag : a -> Bits32) -> (initBits : Bits32) -> (flag : a) -> Bool
isFlagEnabled bitMaskFromFlag initBits flag = (prim__andB32 mask initBits) == mask
    where
        mask = bitMaskFromFlag flag  

flagsFromBitmap : (a -> Bits32) -> List a -> Bits32 -> List a
flagsFromBitmap f []           initBits = [] 
flagsFromBitmap f (flag :: xs) initBits = case isFlagEnabled f initBits flag of
                                               False => flagsFromBitmap f xs initBits 
                                               True  => flag :: flagsFromBitmap f xs initBits 

flagsToBitmap : (a -> Bits32) -> List a -> Bits32
flagsToBitmap f [] = 0x00000000
flagsToBitmap f (x :: xs) = prim__orB32 (f x) (flagsToBitmap f xs)


-- Initflags
public export
data SDLInitFlag = Timer | Audio | Video | CDrom | Joystick | NoParachute | EventThread | Everything

export
sdlInitFlagsList : List SDLInitFlag  
sdlInitFlagsList =  [Timer, Audio, Video, CDrom, Joystick, NoParachute, EventThread, Everything]

export
sdlInitFlagToBitMask : SDLInitFlag -> Bits32
sdlInitFlagToBitMask Timer       = the Bits32 0x00000001
sdlInitFlagToBitMask Audio       = the Bits32 0x00000010
sdlInitFlagToBitMask Video       = the Bits32 0x00000020 
sdlInitFlagToBitMask CDrom       = the Bits32 0x00000100
sdlInitFlagToBitMask Joystick    = the Bits32 0x00000200
sdlInitFlagToBitMask NoParachute = the Bits32 0x00001000
sdlInitFlagToBitMask EventThread = the Bits32 0x00002000
sdlInitFlagToBitMask Everything  = the Bits32 0x0000FFFF

export
sdlGetInitFlagsFromBitmap : List SDLInitFlag -> Bits32 -> List SDLInitFlag
sdlGetInitFlagsFromBitmap = flagsFromBitmap sdlInitFlagToBitMask

export
sdlInitflagsToBitmap : List SDLInitFlag -> Bits32
sdlInitflagsToBitmap xs = flagsToBitmap sdlInitFlagToBitMask xs 

-- setVideoMode flags
public export
data SDLVideoFlag = SWSurface | HWSurface | AsyncBlit | AnyFormat | HWPalette | DoubleBuf | FullScreen | OpenGl | OpenGLBlit | Resizable | NoFrame

export
sdlVideoFlagsList : List SDLVideoFlag  
sdlVideoFlagsList =  [SWSurface, HWSurface, AsyncBlit, AnyFormat, HWPalette, DoubleBuf, FullScreen, OpenGl, OpenGLBlit, Resizable, NoFrame]

export
sdlVideoFlagToBitMask : SDLVideoFlag -> Bits32
sdlVideoFlagToBitMask SWSurface   = the Bits32 0x00000000
sdlVideoFlagToBitMask HWSurface   = the Bits32 0x00000001
sdlVideoFlagToBitMask AsyncBlit   = the Bits32 0x00000004 
sdlVideoFlagToBitMask AnyFormat   = the Bits32 0x10000000
sdlVideoFlagToBitMask HWPalette   = the Bits32 0x20000000
sdlVideoFlagToBitMask DoubleBuf   = the Bits32 0x40000000
sdlVideoFlagToBitMask FullScreen  = the Bits32 0x80000000
sdlVideoFlagToBitMask OpenGl      = the Bits32 0x00000002
sdlVideoFlagToBitMask OpenGLBlit  = the Bits32 0x0000000A
sdlVideoFlagToBitMask Resizable   = the Bits32 0x00000010
sdlVideoFlagToBitMask NoFrame     = the Bits32 0x00000020

export
sdlGetVideoFlagsFromBitmap : List SDLVideoFlag -> Bits32 -> List SDLVideoFlag
sdlGetVideoFlagsFromBitmap = flagsFromBitmap sdlVideoFlagToBitMask

export
sdlVideoFlagsToBitmap : List SDLVideoFlag -> Bits32
sdlVideoFlagsToBitmap xs = flagsToBitmap sdlVideoFlagToBitMask xs 
