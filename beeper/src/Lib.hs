{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Lib
    ( 
        someFunc,
        wave,
        sound,
        toInt16
    ) where

import Control.Monad (unless, forM_)
import SDL (openAudioDevice, Changeable(Mandate), pollEvents, EventPayload(..))
import qualified SDL
import  Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as Vector
import Data.Int (Int16)
import Data.IORef (newIORef, readIORef, writeIORef, IORef(..))
-- audible spectrum
lowHz = 20
highHz = 15000 -- 20k is audible but..

hz :: Float
hz = 15000

someFunc :: IO ()
someFunc = do
    counter <- newIORef 0
    SDL.initializeAll
    window <- SDL.createWindow "Beeper" SDL.defaultWindow {
      SDL.windowInitialSize = SDL.V2 1280 720
    }
    let openAudioDeviceSpec = SDL.OpenDeviceSpec {
        SDL.openDeviceFreq = Mandate 48000,
        SDL.openDeviceFormat = Mandate SDL.Signed16BitLEAudio,
        SDL.openDeviceChannels = Mandate SDL.Stereo,
        SDL.openDeviceSamples = 4096,
        SDL.openDeviceCallback = audioCallback counter,
        SDL.openDeviceUsage = SDL.ForPlayback,
        SDL.openDeviceName = Nothing
    }

    (audioDevice, _audioSpec) <- SDL.openAudioDevice openAudioDeviceSpec
    SDL.setAudioDevicePlaybackState audioDevice SDL.Play
    loop
    putStrLn "we done"

loop :: IO()
loop = do 
  events <- pollEvents
  
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  unless quit loop
          
sound :: [Int16]
sound = map toInt16 $ wave 1000 440

wave :: Float -> Float -> [Float]
wave amp freq
  | freq > lowHz && freq < highHz = map (*amp) $ map sin $ map (*freq) [0,1..]
  | otherwise = error "freq outside audible range"

toInt16 :: Float -> Int16
toInt16 f = fromIntegral $ floor f

--openAudioDevice :: MonadIO m => OpenDeviceSpec -> m (AudioDevice, AudioSpec)

audioCallback :: IORef Int -> SDL.AudioFormat sampleType -> IOVector sampleType -> IO ()
audioCallback counterRef format buffer = do
  case format of
    SDL.Signed16BitLEAudio -> do -- witness sampleType ~ Int16
      putStrLn "callback"
      counter <- readIORef counterRef
      Vector.set buffer 0 -- safety silence
      let size = Vector.length buffer
      let values = zip [0..size-1] $ drop counter sound
      forM_ values $ \(index, value) -> do
        --Vector.write buffer index value
        print (index, value, counter)
      -- TODO: write your 16-bit ints of samples here 

      writeIORef counterRef (counter+size)
    _ -> error "Unsupported audio format."
