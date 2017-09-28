module Draw

import public Graphics.SDL as SDL
import Control.ST
import Control.ST.ImplicitCall
import System              as Sys

%access public export

data Col = MkCol Int Int Int Int

black : Col 
black = MkCol 0 0 0 255

red : Col 
red = MkCol 255 0 0 255

green : Col 
green = MkCol 0 255 0 255

blue : Col 
blue = MkCol 0 0 255 255

cyan : Col 
cyan = MkCol 0 255 255 255

magenta : Col 
magenta = MkCol 255 0 255 255

yellow : Col 
yellow = MkCol 255 255 0 255

white : Col 
white = MkCol 255 255 255 255

interface Draw (m : Type -> Type) where
  Surface : Type

  initWindow       : Int         -> Int     -> ST m (Maybe Var) [addIfJust Surface]
  closeWindow      : (win : Var)            -> ST m ()          [remove win Surface] 
  flip             : (win : Var)            -> ST m ()          [win ::: Surface]
  poll             :                           ST m (Maybe Event) []
  filledRectangle  : (win : Var) -> (Int, Int) -> (Int, Int) -> Col ->
                                               ST m ()          [win ::: Surface]
  drawLine         : (win : Var) -> (Int, Int) -> (Int, Int) -> Col ->
                                               ST m ()          [win ::: Surface]
  loadBMP          : (path : String)        -> ST m (Maybe Var) [addIfJust Surface]
  freeBMP          : (bmp : Var)            -> ST m ()          [remove bmp Surface]
  blitSurface      : (win : Var, bmp : Var) -> ST m ()          [win ::: Surface, bmp ::: Surface] 
  sleep            :                           ST m ()          [] 

implementation Draw IO where
  Surface = State SDL.SDLSurface

  initWindow x y = do Just srf <- lift (SDL.startSDL x y)
                           | pure Nothing
                      var <- new srf
                      pure (Just var)

  closeWindow win = do lift SDL.endSDL
                       delete win

  flip win = do srf <- read win
                lift (SDL.flipBuffers srf)

  blitSurface win bmp = do winSrf <- read win
                           bmpSrf <- read bmp
                           lift (SDL.blitSurface bmpSrf null winSrf null)

  poll = lift SDL.pollEvent

  filledRectangle win (x, y) (ex, ey) (MkCol r g b a)
       = do srf <- read win
            lift $ SDL.filledRect srf x y ex ey r g b a

  drawLine win (x, y) (ex, ey) (MkCol r g b a)
       = do srf <- read win
            lift $ SDL.drawLine srf x y ex ey r g b a

  loadBMP path = do Just bmpSurface <- lift (SDL.loadBMP path)
                         | pure Nothing 
                    var <- new bmpSurface
                    pure (Just var)

  freeBMP bmp  = do srf <- read bmp
                    lift (SDL.freeSurface srf)
                    delete bmp

  sleep = lift (SDL.delay 2000)
 
{-render : Draw m => (win : Var) -> ST m () [win ::: Surface {m}]-}
{-render win = do filledRectangle win (0,0) (640,480) green -}
                {-drawLine win (100,100) (200,200) red-}
                {-flip win-}
    
{-loop : (ConsoleIO m, Draw m) => (win : Var) -> ST m () [win ::: Surface {m}]-}
{-loop win = do render win-}
              {-Just AppQuit <- poll-}
                   {-| _ => loop win-}
              {-putStrLn "Quit Clicked!"-}
              {-pure ()-}

{-drawMain : (ConsoleIO m, Draw m) => ST m () []-}
{-drawMain = with ST do -}
              {-Just win <- initWindow 640 480-}
                 {-| Nothing => putStrLn "Can't open window"-}
              {-loop win-}
              {-closeWindow win-}
