module SynthOps where

import Control.Monad.State.Lazy
import Data.IORef
import Data.Array
import Debug.Trace

import GolfScript.Value
import GolfScript.Interpreter
import Synth

-- |SIOS: Synthesizer Input/Output System
boot :: SynthRef -> GolfValue
boot s = GolfBuiltin $
         do let synthCall :: String -> (Synthesizer -> Interpreter Synthesizer) -> Interpreter ()
                synthCall identifier f =
                  vmAssign identifier $
                  GolfBuiltin $
                  do synth <- liftIO $ readIORef s
                     synth' <- f synth
                     liftIO $ writeIORef s synth'
                     
                channelCall :: String -> (Channel -> Interpreter Channel) -> Interpreter ()
                channelCall identifier f =
                  synthCall identifier $ \synth ->
                  do let channels = synthChannels synth
                         idx = synthChannelIndex synth
                         channel = trace ("channel: " ++ show idx ++ "\nchannel: " ++ show channel) $
                                   channels ! idx
                     channel' <- f channel
                     return $ synth { synthChannels = channels // [(idx, channel')] }
                     
            synthCall "channel" $ \synth ->
              do GolfNumber idx <- vmPop
                 let channels = synthChannels synth
                     channels' = if idx <= snd (bounds channels)
                                 then channels
                                 else listArray (0, idx) $ elems channels ++ repeat defaultChannel
                 return $ synth { synthChannelIndex = idx,
                                  synthChannels = channels' }

            synthCall "frame" $ \synth ->
              do GolfNumber frameSize <- vmPop
                 return $ synth { synthFrameSize = frameSize }
                 
            channelCall "wave" $ \channel ->
              do GolfNumber wave <- vmPop
                 let wave' = [Pulse, Triangle, Sine, Noise] !! wave
                 return $ channel { chWave = wave' }
                 
            channelCall "state" $ \channel ->
              do GolfNumber state <- vmPop
                 let state' = [Release, Attack, Hold] !! state
                 return $ channel { chState = state' }
                 
            channelCall "volume" $ \channel ->
              do GolfNumber vol <- vmPop
                 return $ channel { chVolume = fromIntegral vol / 100.0 }
            
            channelCall "panning" $ \channel ->
              do GolfNumber pan <- vmPop
                 let pan' = fromIntegral pan / 100.0 + 0.5
                 return $ channel { chPanLeft = sqrt (1 - pan'), 
                                    chPanRight = sqrt pan' }
            
            channelCall "sustain" $ \channel ->
              do GolfNumber sustain <- vmPop
                 return $ channel { chSustain = fromIntegral sustain / 100.0 }
            
            channelCall "decay" $ \channel ->
              do GolfNumber decay <- vmPop
                 let f = fromIntegral decay * 1.5 / 100.0
                 return $ channel { chDecay = 1 - 1 / (f * f * 48000 + 1) }
            
            channelCall "release" $ \channel ->
              do GolfNumber rel <- vmPop
                 let f = fromIntegral rel * 1.5 / 100.0
                 return $ channel { chRelease = 1 - 1 / (f * f * 48000 + 1) }
            
            channelCall "pulse" $ \channel ->
              do GolfNumber pulse <- vmPop
                 return $ channel { chPulseWidth = fromIntegral pulse / 100.0 }
            
