import MidiComm
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (forever, forM_, when)
import qualified Data.Map as M
import Store
import System.Exit
import Sound.ALSA.Sequencer.Address
import Sound.ALSA.Sequencer.Client
import Sound.ALSA.Sequencer.Port
import Sound.ALSA.Sequencer.Event
import Sound.ALSA.Sequencer.Connect
import Sound.ALSA.Sequencer 
import Sound.ALSA.Exception hiding (show) 
-- insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k aSource
import System.Time
import qualified Data.Map as M
import System.Environment
main = do
	f:_ <- getArgs
	d1 <- read `fmap` readFile f 
	print (d1 :: [(Int,[(Int,Int,Int)])])
	chplay <- newTChanIO 
	forkIO $ midiOut "play" chplay

	ch <- newTChanIO 
	forkIO $ midiInAll "sinc play" ch
	
	let f state d counter = do 
		ev <- atomically $ readTChan ch
		case body ev of 
			 QueueEv QueueStart _ -> do 
					putStrLn "start"
					f True d1 0

			 QueueEv QueueStop _ -> do 
					putStrLn "stop"
					f False d counter
				
			 QueueEv QueueClock _ -> do
					let (plat,keep) = break (\(c,_) -> c > counter) d
					when state $ do 
						forkIO $ forM_ (concatMap snd plat) $ atomically . writeTChan chplay 
						return ()
					when  ((counter `mod` 6) == 0 && state) $ putStrLn $ "beat " ++ show (counter `div` 6)
					f state keep (counter  +1) 
		         x ->  f state d counter
	forkIO $ f False d1 0
			
		
	getLine



