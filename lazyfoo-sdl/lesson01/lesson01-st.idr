module Main

import Control.ST.ImplicitCall
import Control.ST     as ST
import Draw           as Drw
import Graphics.SDL   as SDL
import Graphics.Utils as Utils
import System         as Sys

Path   : Type
Path   = String

Height : Type
Height = Int

Width  : Type
Width  = Int

interface Lesson01 (m : Type -> Type) where
  Window : Type
  Bmp    : Type

  start   : Width                        -> Height -> ST.ST m (Maybe ST.Var) [addIfJust Window]
  close   : (win : ST.Var)                         -> ST.ST m ()             [ST.Remove win Window]
  freeBmp : (bmp : ST.Var)                         -> ST.ST m ()             [ST.Remove bmp Bmp]
  addBMP  : Path                                   -> ST.ST m (Maybe ST.Var) [addIfJust Bmp]
  render  : (win : ST.Var, bmp : ST.Var)           -> ST.ST m ()             [win ::: Window, bmp ::: Bmp]

-- 'Draw' provides a Surface to draw on, and three states.
Draw m => Lesson01 m where
  Window = Drw.Surface {m} -- surface to draw on
  Bmp    = Drw.Surface {m} -- Bitmap image

  start   width height = with ST.ST do 
                          Just srf <- Drw.initWindow width height -- initSDL and setVideoMode
                               | Nothing => pure Nothing
                          pure (Just srf)

  close   win          = Drw.closeWindow win
  freeBmp bmp          = Drw.freeBMP bmp
  
  addBMP  path         = with ST.ST do 
                           maybebmpSrf <- Drw.loadBMP path 
                           case maybebmpSrf  of
                               Nothing     => pure Nothing 
                               Just bmpSrf => pure (Just bmpSrf) 

  render  win bmp      = with ST.ST do 
                           Drw.filledRectangle win (0, 0) (640, 480) black
                           Drw.blitSurface win bmp
                           Drw.flip win 
                           Drw.sleep
                           {-Just ev <- Drw.poll-}
                                {-| Nothing => render win bmp-}
                           {-case ev of-}
                                {-KeyUp _ => pure ()-}
                                {-_       => render win bmp-}

lesson01 : (ConsoleIO m, Lesson01 m) => ST m () []
lesson01 = with ST.ST do 
             Just win <- start 640 480
                  | Nothing => putStr "Can't make Window\n"
             Just bmp <- addBMP "hello.bmp"
                  | Nothing => do putStr "Unable to add Bmp\n"
                                  close win
             render  win bmp
             freeBmp bmp
             close   win

{-lesson01 : (ConsoleIO m, Lesson01 m) => ST m () []-}
{-lesson01 = with ST.ST do -}
             {-Just bmp <- addBMP "hello.bmp"-}
                  {-| Nothing => do putStr "Unable to add Bmp\n"-}
             {-freeBmp bmp-}
             {-putStr "Done!\n"-}

main : IO ()
main = do run lesson01
-------------------------------------------------------------------------------
