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
-- audible spectrum
lowHz = 20
highHz = 20000

hz :: Float
hz = 10000

someFunc :: IO ()
someFunc = do
    SDL.initializeAll
    window <- SDL.createWindow "Beeper" SDL.defaultWindow {
      SDL.windowInitialSize = SDL.V2 1280 720
    }
    let openAudioDeviceSpec = SDL.OpenDeviceSpec {
        SDL.openDeviceFreq = Mandate 48000,
        SDL.openDeviceFormat = Mandate SDL.Signed16BitLEAudio,
        SDL.openDeviceChannels = Mandate SDL.Stereo,
        SDL.openDeviceSamples = 4096,
        SDL.openDeviceCallback = audioCallback,
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
sound = map toInt16 $ map (*4800) $ wave 0.01 0.5

wave :: Float -> Float -> [Float]
wave pitchF volume = map (*volume) $ map sin $ map (*pitchF) [0..hz]

toInt16 :: Float -> Int16
toInt16 f = fromIntegral $ floor f 

--openAudioDevice :: MonadIO m => OpenDeviceSpec -> m (AudioDevice, AudioSpec)

audioCallback :: SDL.AudioFormat sampleType -> IOVector sampleType -> IO ()
audioCallback format buffer = do
  case format of
    SDL.Signed16BitLEAudio -> do -- witness sampleType ~ Int16
      Vector.set buffer 0 -- safety silence
      let size = Vector.length buffer
      let z = zip [0..size-1] sound
      forM_ z $ \(i, value) -> do
        putStrLn $ show (i, value)
        Vector.write buffer i value
      -- TODO: write your 16-bit ints of samples here 
    _ -> error "Unsupported audio format."
