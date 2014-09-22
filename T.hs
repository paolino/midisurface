import MidiComm
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (forever, forM_, when)
import qualified Data.Map as M
import Store
import System.Exit

main = do
	(oc,ic,ino,inp, k) <- midi3 "prova" 
	s <- newTVarIO new
	tk <- newTVarIO 0
	trec <- newTVarIO True
	forkIO $ forever $ do 
		atomically $ do 
			(chan,c,v) <- readTChan ic
			rec <- readTVar trec
			when rec $ do 
				k <- readTVar tk
				modifyTVar s $ insertion k ((chan,c), v)
			
	forkIO $ forever $ do
		(_,_,v) <- atomically $ readTChan inp
		atomically $ writeTVar trec (v > 0)

	forkIO $ forever $ do 
		(chan,v,r) <- atomically $ readTChan ino
		rec <- atomically $ readTVar trec
		if not rec then 
			atomically $ do
				vs <- readTVar s
				let t = M.findWithDefault M.empty v vs
				forM_ (M.assocs $ M.union  t (M.findWithDefault M.empty 0 vs)) $ \((chan,c),v) -> writeTChan oc (chan,c,v)
		else atomically $ do
			k <- readTVar tk
			modifyTVar s $ \vs -> M.insert v (M.findWithDefault M.empty k vs) vs
			
		atomically $ writeTVar tk v
	forever $ do
		threadDelay 1000000
		r <- atomically $ readTVar trec
		putStr "Recording: " 
		print r
		k <- atomically $ readTVar tk
		putStr "Key: " 
		print k



	{-
	data L 
	= Q -- Quit
	| K Int -- record to key
	| X Int -- fire
	deriving (Show,Read)

	forever $ do
		v <- getLine
		case reads v of 
			[(Q,_)] -> exitSuccess 
			[(K k,_)] -> atomically $ writeTVar tk k
			[(X q,_)] -> atomically $ do
				t <- (flip (M.!) q) `fmap` readTVar s
				forM_ (M.assocs t) $ \((chan,c),v) -> writeTChan oc (chan,c,v)
		-}
