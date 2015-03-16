import MidiComm
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (forever, when)
import Data.Foldable (forM_)
import qualified Data.Map as M
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


type Point = M.Map Int Double

type Control = (Int,Int,Int)

correct :: Control -> Point -> Point
correct (_,n,v) = M.insert n (fromIntegral v/128)

type Points = M.Map Int Point

main = do
	chplay <- newTChanIO 
	forkIO $ midiOut "Qout" chplay
	ch <- newTChanIO 
	forkIO $ midiIn "Qin" ch
	chn  <- newTChanIO
	forkIO $ midiInNote "Qnote" chn
	y <- newTVarIO M.empty
	ys' <- read `fmap` readFile "params.txt"
	ys <- newTVarIO ys'
	k <- newTVarIO 0
	let dump = do
		x <- readTVar k
		y <- flip (M.!) x `fmap` readTVar ys 
		forM_ (M.assocs y) $ \(n,v) -> writeTChan chplay (1,n,floor $ v*128)
	atomically dump
	forkIO $ forever $ atomically $ do
		x <- readTChan ch
		z <-  readTVar k
		modifyTVar ys $ M.adjust (correct x) z
	forkIO $ forever $ do
		(_,x,v) <- atomically $ readTChan chn
		atomically $ do
				writeTVar k (x-36)
				dump 
		ys' <- atomically $ readTVar ys
		writeFile "params.txt" $ show ys'
		

	forever $ do
		ys' <- atomically $ readTVar ys
		writeFile "params.txt" $ show ys'
		n <- getLine
		case reads n of
			[(x,_)] -> atomically $ do
				writeTVar k x
				dump 
			_ -> return ()
	

	


