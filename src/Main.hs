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
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import SDL.Input.Keyboard.Codes ()
import SDL (($=))
import qualified SDL
import Control.Lens
import Control.Concurrent (yield)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

screenWidth, screenHeight :: Int
(screenWidth, screenHeight) = (320, 288)

scale :: CFloat
scale = (fromIntegral 2) -- pixels scaled up by a factor of 3

frameSkip :: Int
frameSkip = 0 -- 60 fps on a 60 hz screen

blankScreen = []

sprites = [("hello.bmp", 8, 8, (RGB 255 255 255))]

data RGB = RGB Word8 Word8 Word8

data Pixel = Pixel (V2 Int) RGB

type Grid a = Map.Map (V2 Int) a

data SpriteSheet = SpriteSheet (IO SDL.Texture) Int Int

spriteSheet :: SDL.Renderer -> String -> RGB -> Int -> Int -> IO SpriteSheet
spriteSheet rendr name (RGB r g b) wx wy = do
  surface <- SDL.loadBMP name
  SDL.surfaceColorKey surface $= (Just (V4 r g b maxBound))
  texture <- SDL.createTextureFromSurface rendr surface
  let sheet = (SpriteSheet (return texture) wx wy)
  return sheet

spriteIndex :: Int -> Int -> SpriteSheet -> SDL.Rectangle CInt
spriteIndex ix iy (SpriteSheet s wx wy) = do SDL.Rectangle (P (V2 (fromIntegral ix* fromIntegral wx) (fromIntegral iy* fromIntegral wy))) (V2 (fromIntegral wx) (fromIntegral wy))

data MyVars = MyVars {
    _renderer :: SDL.Renderer
  , _window :: SDL.Window
  , _pixelGrid :: [Pixel]
  , _currentColor :: RGB
  , _spriteSheets :: Vector.Vector (IO SpriteSheet)
                     
  -- Add your custom variables below.
  , _pixelX :: Int
  , _pixelY :: Int
  , _drawPixels :: Grid RGB
  , _dustCells :: Grid (Bool, Bool, Bool, Bool)
  , _playerShots :: [(Int,Int)]
  , _playerShotTimer :: Int
  , _playerEngineFlareTimer :: Int
}

gridMap fn g = Map.traverseWithKey (\(V2 x y) k -> fn x y k) g

gridSet x y k g = Map.insert (V2 x y) k g

makeLenses ''MyVars

myVars r w s = MyVars { _renderer = r,
                        _window = w,
                        _pixelGrid = blankScreen,
                        _currentColor = (RGB minBound minBound minBound),
                        _spriteSheets = foldl (\v (name, wx, wy, colorKey) -> Vector.cons (spriteSheet r name colorKey wx wy) v) Vector.empty s,
                        -- Custom starting values go here.
                        _pixelX = 10,
                        _pixelY = 10,
                        _drawPixels = Map.empty,
                        _dustCells = Map.empty,
                        _playerShots = [],
                        _playerShotTimer = 0,
                        _playerEngineFlareTimer = 0
                      }

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  -- SDL.HintRenderScaleQuality $= SDL.ScaleBest
  --do renderQuality <- SDL.get SDL.HintRenderScaleQuality
  --   when (renderQuality /= SDL.ScaleLinear) $
  --     putStrLn "Warning: Linear texture filtering not enabled!"
  window <-
    SDL.createWindow
      "SDL Tutorial"
      SDL.defaultWindow {SDL.windowInitialSize = V2 (fromIntegral screenWidth) (fromIntegral screenHeight)}
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
  SDL.rendererScale renderer $= (V2 scale scale)
  SDL.rendererLogicalSize renderer $= Just (V2 160 144)
  let loop :: Int -> Int -> MyVars -> IO ()
      loop 0 maxTick previousFrame = do
        events <- SDL.pollEvents
        SDL.rendererDrawColor renderer $= V4 128 128 64 maxBound
        SDL.clear renderer
        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
        scancodes <- SDL.getKeyboardState
        thisFrame <- execStateT (program scancodes) previousFrame
        sequence $ fmap (\(Pixel (V2 x y) (RGB r g b)) -> do
                            SDL.rendererDrawColor renderer $= V4 r g b maxBound
                            SDL.drawPoint renderer (P (V2 (fromIntegral x) (fromIntegral y)))) $ (_pixelGrid thisFrame)
        SDL.present renderer
        yield
        unless quit (loop maxTick maxTick thisFrame)
      loop tick maxTick previousFrame = SDL.present renderer >> loop (pred tick) maxTick previousFrame
  loop 0 frameSkip $ myVars renderer window sprites
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

setColor :: Word8 -> Word8 -> Word8 -> StateT MyVars IO ()
setColor r g b = currentColor .= (RGB r g b)

draw :: Int -> Int -> StateT MyVars IO ()
draw x y = ((pixelGrid %=) . (\(RGB r g b) -> (((Pixel (V2 x y) (RGB r g b)) :)))) =<< use currentColor

clear :: StateT MyVars IO ()
clear = pixelGrid .= blankScreen

blit :: Int -> Int -> Int -> Int -> Int -> StateT MyVars IO ()
blit is ix iy x y = do
  sheets <- use spriteSheets
  sheet@(SpriteSheet t wx wy) <- liftIO $ sheets Vector.! is
  texture <- liftIO t
  rendr <- use renderer
  liftIO $ SDL.copy rendr texture (Just (spriteIndex ix iy sheet)) (Just (SDL.Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral wx) (fromIntegral wy))))

program :: (SDL.Scancode -> Bool) -> StateT MyVars IO ()
program scancodes = do
  let blitPlayerShip = case (left + right) of
        -1 -> blit 0 2 0
        1 -> blit 0 3 0
        _ -> blit 0 0 0
  clear
  setColor 0 0 0
  pixelX %= (+ (left + right))
  pixelY %= (+ (up + down))
  x <- use pixelX
  y <- use pixelY
  blitPlayerShip x y
  playerShots %= filter (\(x,y) -> y > 0)
  playerShots %= fmap (\(x,y) -> (x,(y-2)))
  shots <- use playerShots
  mapM_ (\(x,y) -> draw x y >> draw x (y-1)) shots
  shotTimer <- use playerShotTimer
  if (shotTimer > 0)
    then (playerShotTimer %= (subtract 1))
    else when shooting (do playerShots %= ([(x+2+right,y+2), (x+5+left,y+2)] ++)
                           playerShotTimer .= 30)
  engineFlareTimer <- use playerEngineFlareTimer
  if (engineFlareTimer > 0)
    then playerEngineFlareTimer %= (subtract 1)
    else (playerEngineFlareTimer .= 20)
  when (engineFlareTimer > 10) (do draw (x+3) (y+6)
                                   draw (x+4) (y+6))
  where 
    up = if scancodes SDL.ScancodeW then -1 else 0
    down = if scancodes SDL.ScancodeS then 1 else 0
    left = if scancodes SDL.ScancodeA then -1 else 0
    right = if scancodes SDL.ScancodeD then 1 else 0
    shooting = scancodes SDL.ScancodeSpace
  
  
  
