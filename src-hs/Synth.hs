module Synth where

import Sound.Pulse.Simple
import Data.IORef
import Data.Word (Word16)
import Data.Array


data ChannelState = Release | Attack | Hold
data ChannelWave = Pulse | Triangle | Sine | Noise

data Channel = Channel { chPhase :: Double,
                         chShiftReq :: Int,
                         chSpeed :: Double,
                         chVolume :: Double,
                         chPanLeft :: Double,
                         chPanRight :: Double,
                         chAttack :: Double,
                         chDecay :: Double,
                         chSustain :: Double,
                         chRelease :: Double,
                         chState :: ChannelState,
                         chLevel :: Double,
                         chWave :: ChannelWave,
                         chPulseWidth :: Double
                       }

defaultChannel = Channel { chPhase = 0,
                           chShiftReq = 0x7ffff8,
                           chSpeed = 0,
                           chVolume = 100,
                           chPanLeft = sqrt 0.5,
                           chPanRight = sqrt 0.5,
                           chAttack = 2,
                           chDecay = 30,
                           chSustain = 50,
                           chRelease = 2,
                           chWave = Pulse,
                           chPulseWidth = 50,
                           chState = Release,
                           chLevel = 0
                         }

data Synthesizer = Synthesizer { synthChannels :: Array Int Channel,  -- TODO: Array
                                 synthChannelIndex :: Int,
                                 synthFrameSize :: Int,
                                 synthPulse :: Simple
                               }
type SynthRef = IORef Synthesizer

new :: IO SynthRef
new = do pulse <- simpleNew Nothing "minimal" Play Nothing "minimal synthesizer with golfscript interpreter"
                  (SampleSpec (S16 LittleEndian) 48000 2) Nothing Nothing
         newIORef $ Synthesizer { synthChannels = array (0, 0) [(0, defaultChannel)],
                                  synthChannelIndex = 0,
                                  synthFrameSize = 10000,
                                  synthPulse = pulse
                                }

close :: SynthRef -> IO ()
close s = readIORef s >>= simpleFree . synthPulse

play :: SynthRef -> IO ()
play s = do synth <- readIORef s
            let pulse = synthPulse synth
                writeSample sample synth
                    | sample < synthFrameSize synth
                        = do let (l, r, synth') = mix synth
                             simpleWrite pulse [l, r]
                             writeSample (sample + 1) synth
                    | otherwise
                        = return synth   
            synth' <- writeSample 0 synth
            writeIORef s synth'
            simpleDrain pulse

mix :: Synthesizer -> (Word16, Word16, Synthesizer)
mix synth = undefined
