{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Foldable (for_)
import Foreign.C.Types
import Linear
import Linear.Affine
import Data.Word
import qualified Data.Map as Map
import SDL.Input.Keyboard.Codes ()
import SDL (($=))
import qualified SDL
import Control.Lens

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

scale :: CInt
scale = 2 -- pixels scaled up by a factor of 2

frameSkip :: Int
frameSkip = 1 -- 30 fps on a 60 hz screen

data MyVars = MyVars {
    _pixelGrid :: Map.Map (CInt,CInt) (Word8, Word8, Word8)
  , _currentColor :: (Word8, Word8, Word8)
                     
  -- Add your custom variables below.
  , _pixelX :: CInt
  , _pixelY :: CInt
}
makeLenses ''MyVars

myVars = MyVars { _pixelGrid = Map.empty,
                  _currentColor = (maxBound, maxBound, maxBound),
                  -- Custom starting values go here.
                  _pixelX = 10,
                  _pixelY = 10
                }

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"
  window <-
    SDL.createWindow
      "SDL Tutorial"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window
  renderer <-
    SDL.createRenderer
      window
      (-1)
      (SDL.RendererConfig
         { SDL.rendererType = SDL.AcceleratedVSyncRenderer
         , SDL.rendererTargetTexture = False
         })
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  let loop :: Int -> Int -> MyVars -> IO ()
      loop 0 maxTick previousFrame = do
        events <- SDL.pollEvents
        SDL.rendererDrawColor renderer $= V4 minBound minBound minBound maxBound
        SDL.clear renderer
        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
        scancodes <- SDL.getKeyboardState
        let thisFrame = execState (program scancodes) previousFrame
        Map.traverseWithKey (\(x, y) (r, g, b) -> do
                                let nx = scale * x
                                    ny = scale * y
                                SDL.rendererDrawColor renderer $= V4 r g b maxBound
                                mapM_ (\(x, y) -> SDL.drawPoint renderer (P (V2 x y))) [(x,y) | x <- [nx..nx+scale], y <- [ny..ny+scale]]) $ _pixelGrid thisFrame
        SDL.present renderer
        unless quit (loop maxTick maxTick thisFrame)
      loop tick maxTick previousFrame = SDL.present renderer >> loop (pred tick) maxTick previousFrame
  loop 0 1 myVars
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

setColor :: Word8 -> Word8 -> Word8 -> State MyVars ()
setColor r g b = currentColor .= (r, g, b)

draw :: CInt -> CInt -> State MyVars ()
draw x y = do
  ((pixelGrid %=) . Map.insert (x, y)) =<< use currentColor

clear :: State MyVars ()
clear = pixelGrid .= Map.empty

program :: (SDL.Scancode -> Bool) -> State MyVars ()
program scancodes = do
  clear
  pixelX %= (+ (left + right))
  pixelY %= (+ (up + down))
  setColor 255 255 255
  x <- use pixelX
  y <- use pixelY
  draw x y
  where 
    up = if scancodes SDL.ScancodeW then -1 else 0
    down = if scancodes SDL.ScancodeS then 1 else 0
    left = if scancodes SDL.ScancodeA then -1 else 0
    right = if scancodes SDL.ScancodeD then 1 else 0
  
  
  
