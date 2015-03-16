
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

main = do
	let n = 16
	ch <- newTChanIO 
	forkIO $ midiInAll "sinc rec" ch
	counter <- newTVarIO 0
	d <- newTVarIO M.empty
	let f state = do 
		ev <- atomically $ readTChan ch
		f =<< case body ev of 
			 QueueEv QueueStart _ -> do 
					putStrLn "start"
					atomically $  do
						writeTVar d M.empty	
						writeTVar counter 0
					
					return True

			 QueueEv QueueStop _ -> do 
					putStrLn "stop"
					t <- getClockTime >>= toCalendarTime
					m <- atomically (readTVar d)
					writeFile (show (ctHour t,ctMin t,ctSec t)) $ show (M.assocs m)
					return False
				
			 QueueEv QueueClock _ -> do
					count <- atomically $ do 
						r <- readTVar counter
						writeTVar counter (r + 1)
						return r
					when  ((count `mod` (n*6)) == 0 && state) $ putStr "loop, " 
					when  ((count `mod` 6) == 0 && state) $ putStrLn $ "beat " ++ show (count `div` 6)
					return state
		         x ->  return state
	forkIO $ f False
	ch <- newTChanIO 
	forkIO $ midiIn "rec" ch
	forkIO . forever . atomically $ do 
			ev <- readTChan ch
			count <- readTVar counter
			modifyTVar d $ M.insertWith (flip (++)) count [ev]
			
		
	getLine



