import Prelude hiding ((.), id)
import System.IO
import System.Hardware.Serialport
import System.Environment
import Control.Monad
import Data.List (nub)
import Data.Maybe
import Control.Category
import Control.Arrow
import Data.Machine.Mealy


	

import MidiComm
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (forever, when)
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
import System.Exit







changing n = let 
	get [] = Nothing
	get xs = if length (nub xs) < n then Nothing else Just (last xs)
	add xs x = (get xs, mk . take n $ x:xs)
	mk xs =  Mealy (add xs)
	in mk []

medium n = let
	get xs = sum xs / fromIntegral n
	add xs x = (get xs, mk . take n $ x:xs)
	mk xs = Mealy (add xs)
	in mk []

normalize  = let
	add mi ma x 
		| x < mi = (0, mk x ma)
		| x > ma = (1, mk mi x)
		| otherwise = ((x - mi) / (ma - mi), mk mi ma)
	mk mi ma = Mealy (add mi ma)
	in mk 0 0

roundn n x = fromIntegral (round (x * (10 ^ n))) / (10 ^ n)

myarrow n m = (changing m) . arr floor .  arr ((*) 128) . normalize . arr (roundn 2)  . (medium n)

data Actions = 
	Play 
	| Quit 
	| Pause 
	| Set Int
	deriving (Show,Read) 
main = do
	port <- head `fmap` getArgs 
	let news= defaultSerialSettings{commSpeed=CS115200}
	h <- hOpenSerial port news
	chplay <- newTChanIO 
	sem <- newTVarIO $ return ()
	forkIO $ midiOut "Qout" chplay
	let aq ws = do
		 let parse = do
		 	l <- hGetLine h
			case reads l of 
				[(rs,_)] -> return rs
				_ -> putStrLn l >> parse
		 rs <- parse
		 let 	(qs,ws') = unzip $ zipWith runMealy ws rs
		 when  (any isJust qs) $ do
			forM_ (zip [0..] qs) $ \(n,mv) -> do
			case mv of 
				Nothing -> return ()
				Just v -> do 
						atomically $ do 
							join $ readTVar sem
							writeTChan chplay (1,n,v)
						-- print qs
			
		 
		 aq ws' 
	forkIO $ aq (replicate 9 (myarrow 10 3))
	forever $ do
		l <- getLine
		case reads l of
			[(Set x,_)] -> do
				atomically $ writeTChan chplay (1,x,0)
				

			[(Quit,_)] -> exitSuccess
			[(Pause,_)] -> do
				atomically $ writeTVar sem $ retry
				putStrLn "pausing"
			[(Play,_)] -> do
				atomically $ writeTVar sem $ return ()

			_ -> putStrLn "error parsing"

	hClose h
