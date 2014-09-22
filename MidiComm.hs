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
        -> TChan (Int,Int,Int)  -- ^ event channel
        -> IO ()
midiIn name incha = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T InputMode) name
        c <- getId h
        withSimple h "in control" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> forever $ do
                ev <-  input h
                case body ev of
                     CtrlEv Controller (Ctrl 
                                        (Channel (fromIntegral -> cha)) 
                                        (Parameter (fromIntegral -> par)) 
                                        (Value (fromIntegral -> val))
                                        ) -> atomically $ writeTChan incha (cha,par,val)
                     _ -> return ()   


midiInPgm  :: String  -- ^ client name
        -> TChan (Int,Int,Int)  -- ^ event channel
        -> IO ()
midiInPgm name  incha = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T InputMode) name
        c <- getId h
        withSimple h "in program change" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> forever $ do
                ev <-  input h
		case body ev of
                     CtrlEv PgmChange  (Ctrl 
                                        (Channel (fromIntegral -> cha)) 
                                        (Parameter (fromIntegral -> par)) 
                                        (Value (fromIntegral -> val))
                                        ) ->
			atomically $ writeTChan incha (cha,par,val)

                     _ -> return ()           


midiInAll :: String  -- ^ client name
        -> TChan Sound.ALSA.Sequencer.Event.T  -- ^ event channel
        -> IO ()
midiInAll name  incha = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T InputMode) name
        c <- getId h
        withSimple h "in program change" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> forever $ do
                ev <-  input h
		atomically $ writeTChan incha ev
        
                -- | Loop-accept control midi message on a specific channel
midiInNote  :: String  -- ^ client name
        -> TChan (Int,Int,Int)  -- ^ event channel
        -> IO ()
midiInNote name  incha = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T InputMode) name
        c <- getId h
        withSimple h "in notes" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> forever $ do
                ev <-  input h
                case body ev of
                     NoteEv NoteOn (Note 
                                        (Channel (fromIntegral -> cha)) 
                                        (Pitch (fromIntegral -> par)) 
                                        (Velocity (fromIntegral -> val)) _ _
                                        ) ->  atomically $ writeTChan incha (cha,par,val)
                     _ -> return ()        
-- | Loop-broadcast control midi message on a specific channel
midiOut :: String  -- ^ client name
        -> TChan (Int,Int,Int)  -- ^ event channel
        -> IO ()
midiOut name  ech = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e) $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T OutputMode) name
        c <- getId h
        withSimple h "out control" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> forever $ do
                (cha,par,val) <- atomically $ readTChan ech
                let ev =  forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $ 
                                CtrlEv Controller (Ctrl 
                                        (Channel $ fromIntegral cha) 
                                        (Parameter $ fromIntegral par) 
                                        (Value $ fromIntegral val)
                                        )
                void $ outputDirect h $ ev 
type TC3I = TChan (Int,Int,Int)
-- | Light fork midiIn an midiOut threads
midi3 :: String  -- ^ client name
        -> IO (TC3I, TC3I, TC3I ,TC3I , IO ()) --  ^ communication channels and the kill both threads thread action
midi3 name = do
        incha <- newTChanIO
        inchapgm <- newTChanIO
        outcha <- newTChanIO
	notecha <- newTChanIO
        ti <- forkIO $ midiIn (name ++ ".cin") incha
        tp <- forkIO $ midiInPgm (name ++ ".pin") inchapgm
        to <- forkIO $ midiOut (name ++ ".cout") outcha
        ns <- forkIO $ midiInNote (name ++ ".nin") notecha
        return $ (outcha, incha, notecha , inchapgm, killThread ti >> killThread to >> killThread ns >> killThread tp) 
        

