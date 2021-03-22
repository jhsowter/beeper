module Lib
    ( 
        someFunc,
        wave
    ) where

-- audible spectrum
lowHz = 20
highHz = 20000

hz :: Float
hz = 10000

someFunc :: IO ()
someFunc = putStrLn "weelo hold"

sound = wave 0.01 0.5

wave :: Float -> Float -> [Float]
wave pitchF volume = map (*volume) $ map sin $ map (*pitchF) [0..hz]
