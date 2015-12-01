{-# LANGUAGE ViewPatterns #-}
module MidiComm where


import Prelude hiding (show)

import Sound.ALSA.Sequencer.Address
import Sound.ALSA.Sequencer.Client
import Sound.ALSA.Sequencer.Port
import Sound.ALSA.Sequencer.Event
import Sound.ALSA.Sequencer.Connect
import Sound.ALSA.Sequencer 
import Sound.ALSA.Exception  



import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

data E = Non Int Int | Noff Int Int | C Int Int

-- | Loop-accept control midi message on a specific channel
midiIn  :: String  -- ^ client name
        -> Int     -- ^ listening midi channel
        -> TChan E  -- ^ event channel
        -> IO ()
midiIn name recha incha = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T InputMode) name
        c <- getId h
        withSimple h "midi in" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> forever $ do
                ev <-  input h
                case body ev of
                        CtrlEv Controller (Ctrl 
                                        (Channel (fromIntegral -> cha)) 
                                        (Parameter (fromIntegral -> par)) 
                                        (Value (fromIntegral -> val))
                                        ) -> when  (cha == recha) $ atomically $ writeTChan incha $ C par val
                
                        NoteEv NoteOn (Note
                            (Channel (fromIntegral -> cha)) 
                            (Pitch (fromIntegral -> par)) 
                            (Velocity (fromIntegral -> val))
                            _ _
                            ) -> when  (cha == recha) $ atomically $ writeTChan incha $ Non par val

                        NoteEv NoteOff (Note
                                (Channel (fromIntegral -> cha)) 
                                (Pitch (fromIntegral -> par)) 
                                (Velocity (fromIntegral -> val))
                                _ _
                            ) -> when  (cha == recha) $ atomically $ writeTChan incha $ Noff par val
                        _ -> return ()           
-- | Loop-broadcast control midi message on a specific channel
midiOut :: String  -- ^ client name
        -> Int     -- ^ broadcast midi channel
        -> TChan E  -- ^ event channel
        -> IO ()
midiOut name recha ech = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e) $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T OutputMode) name
        c <- getId h
        withSimple h "midi out" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> forever $ do
                e <- atomically $ readTChan ech
                let ev =  forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $ case e of
                                C par val -> CtrlEv Controller (Ctrl 
                                        (Channel $ fromIntegral recha) 
                                        (Parameter $ fromIntegral par) 
                                        (Value $ fromIntegral val)
                                        )
                                Non par val ->  NoteEv NoteOn (Note
                                        (Channel $ fromIntegral recha) 
                                        (Pitch $ fromIntegral par) 
                                        (Velocity $ fromIntegral val)
                                        (Velocity 0) (Duration 0)
                                        ) 

                                Noff par val -> NoteEv NoteOff (Note
                                        (Channel $ fromIntegral recha) 
                                        (Pitch $ fromIntegral par) 
                                        (Velocity $ fromIntegral val)
                                        (Velocity 0) (Duration 0)
                                        ) 

                void $ outputDirect h $ ev 
-- | Light fork midiIn an midiOut threads
midiInOut  :: String  -- ^ client name
        -> Int     -- ^ broadcast and listening midi channel
        -> IO (TChan E, TChan E, IO ()) --  ^ communication channels and the kill both threads thread action
midiInOut name recha  = do
        incha <- newTChanIO
        outcha <- newTChanIO
        ti <- forkIO $ midiIn name recha incha
        to <- forkIO $ midiOut name recha outcha
        return $ (incha, outcha, killThread ti >> killThread to)
        

