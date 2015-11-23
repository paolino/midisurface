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


-- | Loop-accept control midi message on a specific channel
midiIn  :: String  -- ^ client name
        -> Int     -- ^ listening midi channel
        -> TChan (Int,Int)  -- ^ event channel
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
                                        ) -> when  (cha == recha) $ atomically $ writeTChan incha (par,val)
                     _ -> return ()           
                
-- | Loop-broadcast control midi message on a specific channel
midiOut :: String  -- ^ client name
        -> Int     -- ^ broadcast midi channel
        -> TChan (Int,Int)  -- ^ event channel
        -> IO ()
midiOut name recha ech = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e) $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T OutputMode) name
        c <- getId h
        withSimple h "midi out" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> forever $ do
                (par,val) <- atomically $ readTChan ech
                let ev =  forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $ 
                                CtrlEv Controller (Ctrl 
                                        (Channel $ fromIntegral recha) 
                                        (Parameter $ fromIntegral par) 
                                        (Value $ fromIntegral val)
                                        )
                void $ outputDirect h $ ev 
-- | Light fork midiIn an midiOut threads
midiInOut  :: String  -- ^ client name
        -> Int     -- ^ broadcast and listening midi channel
        -> IO (TChan (Int,Int), TChan (Int,Int), IO ()) --  ^ communication channels and the kill both threads thread action
midiInOut name recha  = do
        incha <- newTChanIO
        outcha <- newTChanIO
        ti <- forkIO $ midiIn name recha incha
        to <- forkIO $ midiOut name recha outcha
        return $ (incha, outcha, killThread ti >> killThread to)
        

