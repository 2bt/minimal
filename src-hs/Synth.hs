module Synth where

import Sound.Pulse.Simple
import Data.IORef
import Data.Word (Word16)
import Data.Array
import Data.Bits
import Data.Word


data ChannelState = Release | Attack | Hold
                    deriving (Show, Eq, Ord)
data ChannelWave = Pulse | Triangle | Sine | Noise
                   deriving (Show, Eq, Ord)

data Channel = Channel { chPhase :: Double,
                         chShiftReg :: Word32,
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
             deriving (Show)

defaultChannel = Channel { chPhase = 0,
                           chShiftReg = 0x7ffff8,
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
                 deriving (Show)
type SynthRef = IORef Synthesizer

instance Show Simple where
  show _ = "<Pulse>"

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
                        = do let (synth', l, r) = mix synth
                             putStrLn $ show synth'
                             simpleWrite pulse [truncate $ l * 6000,
                                                truncate $ r * 6000 :: Word16]
                             writeSample (sample + 1) synth
                    | otherwise
                        = return synth
            synth' <- writeSample 0 synth
            writeIORef s synth'
            simpleDrain pulse

mix :: Synthesizer -> (Synthesizer, Double, Double)
mix synth = let (channels, l, r) = foldl (\(channels, l, r) channel ->
                                           let (channel', l', r') = generate channel
                                           in (channel' : channels, l + l', r + r')
                                         ) ([], 0, 0) $ elems $ synthChannels synth
                channels' = array (0, length channels - 1) $
                            zip [(length channels - 1)..0] channels
            in (synth { synthChannels = channels' }, l, r)

generate :: Channel -> (Channel, Double, Double)
generate = osc . adsr
    where adsr :: Channel -> Channel
          adsr c = case chState c of
                     Release ->
                         c { chLevel = chLevel c * chRelease c }
                     Attack ->
                         let level = chLevel c + chAttack c
                         in if level > 1
                            then c { chLevel = 1,
                                     chState = Hold }
                            else c { chLevel = level }
                     Hold ->
                         c { chLevel = chSustain c + (chLevel c - chSustain c) * chDecay c }
          osc :: Channel -> (Channel, Double, Double)
          osc c = let c' = c { chPhase = chPhase c + chSpeed c }
                      c'' | chWave c' /= Noise = c' { chPhase = chPhase c' -
                                                                fromIntegral (truncate $ chPhase c') }
                          | otherwise = c'
                      (amp, c''') = case chWave c'' of
                                      Pulse ->
                                          (if chPhase c'' < chPulseWidth c''
                                           then -1
                                           else 1, 
                                           c'')
                                      Triangle ->
                                          (if chPhase c'' < chPulseWidth c''
                                           then (2 / chPulseWidth c'') * chPhase c'' - 1
                                           else (-2 / (1 - chPulseWidth c'')) * 
                                                    (chPhase c'' - chPulseWidth c'') + 1,
                                           c'')
                                      Sine ->
                                          (sin $ chPhase c'' * 2 * pi,
                                           c'')
                                      Noise ->
                                          let r :: Word32 -> Word32 -> Double -> (Word32, Word32, Double)
                                              r s b phase
                                                  | phase > 0.1 =
                                                      let phase' = phase - 0.1
                                                          b' = ((s `shiftR` 22) `xor` (s `shiftR` 17)) .&. 1
                                                          s' = ((s `shiftL` 1) .&. 0x7fffff) + b'
                                                      in r s' b' phase'
                                                  | otherwise = 
                                                      (s, b, phase)
                                              (s, b, phase) = r (chShiftReg c'') 0 $ chPhase c''
                                          in ((fromIntegral $
                                               ((s .&. 0x400000) `shiftR` 11) .|.
                                               ((s .&. 0x100000) `shiftR` 10) .|.
					       ((s .&. 0x010000) `shiftR` 7) .|.
					       ((s .&. 0x002000) `shiftR` 5) .|.
					       ((s .&. 0x000800) `shiftR` 4) .|.
					       ((s .&. 0x000080) `shiftR` 1) .|.
					       ((s .&. 0x000010) `shiftL` 1) .|.
					       ((s .&. 0x000004) `shiftL` 2)) *
                                              (1.0 / (fromIntegral (1 `shiftL` 12 :: Word32))) - 0.5,
                                              c'' { chPhase = phase,
                                                    chShiftReg = s })

                      amp' = amp * chVolume c''' * chLevel c'''
                  in (c''', amp' * chPanLeft c''', amp' * chPanRight c''')
                     