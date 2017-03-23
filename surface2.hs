{-# LANGUAGE ScopedTypeVariables, OverloadedStrings , TemplateHaskell,  ViewPatterns, Rank2Types#-}
-- module GUI where
import Prelude hiding (readFile)
import Graphics.UI.Gtk

import Control.Monad
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Strict
import Data.IORef
import Data.Text hiding (zip,null)
import qualified Data.Map as M
import Text.Printf
import Control.Lens (over,view,preview,Prism',_1,(^.),(.~),(%~),ix,at)
import Control.Lens.TH

import System.Directory
import System.Environment
import MidiComm


midichannel = 0

data Cntrl = Cntrl {
  _base :: Int,
  _width :: Int
                   }deriving (Show,Read)

data Value = Value {
  _cntrl :: Cntrl,
  _actual :: Int
                   }deriving (Show,Read)
makeLenses ''Value
zero = Value (Cntrl 0 127) 0

makeLenses ''Cntrl

data Channell = Channell {
        _controls :: M.Map Int Value,
        _played :: M.Map Int Bool
        } deriving (Show,Read)
makeLenses ''Channell

matchEv :: Prism' E (Int,Int) -> Int -> E -> Maybe Bool
matchEv x n = fmap ((==) n . view _1).preview x

main :: IO ()
main = do
  (midiinchan, midioutchan, _) <- midiInOut "midi control GUI" midichannel
  tboard <- newTVarIO $ M.fromList $ zip [0..15] $ repeat (Channell (M.fromList $ zip [0..127] $ repeat zero) (M.fromList $ zip [0..127] $ repeat False)):: IO (TVar (M.Map Int Channell))

  update <- newBroadcastTChanIO -- duplicable messages from midi
  forkIO . forever . atomically $ readTChan midiinchan >>= writeTChan update

  tsel <- newTVarIO 0 -- patch selection

  reset <- newBroadcastTChanIO -- rewrite condition

  name:_ <- getArgs -- persistence file name

  t <- doesFileExist name
  when t $ do
      r <- readFile name
      atomically . writeTVar tboard . read $ r
      forkIO $ do
          threadDelay 1000000
          atomically (writeTChan reset ())
      return ()

  initGUI
  window <- windowNew

  forkIO . forever $ do
      threadDelay 1000000
      r <- atomically $ readTVar tboard
      writeFile name $ show r
  -- midi listening
  --


  -- main box

  mainbox    <- vBoxNew False 1
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
                          containerBorderWidth := 10, containerChild := mainbox]
  -- main buttons
  -- knobs
  -- ad <- adjustmentNew 0 0 400 1 10 400
  -- sw <- scrolledWindowNew Nothing (Just ad)
  commandBox <- hBoxNew False 1
  boxPackStart mainbox commandBox PackNatural 0

  mute <- checkButtonNewWithLabel ("mute" ::Text)
  boxPackStart commandBox mute PackNatural 0

  copy <- checkButtonNewWithLabel ("copy" ::Text)
  boxPackStart commandBox copy PackNatural 0

  versions <- hBoxNew False 1
  boxPackStart mainbox versions PackNatural 0
  (\f -> foldM_ f Nothing [0..15]) $ \b0 n -> do
    b <- case b0 of
            Nothing -> radioButtonNewWithLabel (pack $ show $ n)
            Just b0 ->  radioButtonNewWithLabelFromWidget b0 (pack $ show $ n)
    boxPackStart versions b PackNatural 0
    on b buttonActivated $ do
      t <- toggleButtonGetActive copy
      when t $ do
        atomically $ do
          sel <- readTVar tsel
          wx <- flip (M.!) sel <$> readTVar tboard
          modifyTVar tboard $ ix n .~ wx
      atomically $ do
          writeTVar tsel n
          writeTChan reset ()
    return $ Just b
  controlbox <- hBoxNew False 1
  boxPackStart mainbox controlbox PackNatural 0

  let z = 27
  forM_ [0..z - 1] $ \param -> do
    paramBox    <- vBoxNew False 1
    widgetSetSizeRequest paramBox (-1) 540
    boxPackStart controlbox paramBox PackNatural 0

    paramLearn <- buttonNewWithLabel (pack $ show param)
    on paramLearn buttonActivated $ atomically $ do
            sel <- readTVar tsel
            wx <- flip (M.!) param  <$> view controls <$> flip (M.!) sel <$> readTVar tboard
            let     vx = view actual wx
            writeTChan midioutchan . C param . floor $
                              fromIntegral vx /128 *fromIntegral (wx ^. cntrl . width)  + fromIntegral (wx ^. cntrl . base)

    boxPackStart paramBox paramLearn PackNatural 0

    input <- vScaleNewWithRange 0 127 1
    rangeSetValue input 0
    boxPackStart paramBox input PackGrow 0

    widthScale <- vScaleNewWithRange 0 127 1
    rangeSetValue widthScale 127
    boxPackStart paramBox widthScale PackGrow 0

    shiftScale <- vScaleNewWithRange 0 127 1
    rangeSetValue shiftScale 0
    boxPackStart paramBox shiftScale PackGrow 0

    output <- vScaleNewWithRange 0 127 1
    rangeSetValue output 0
    boxPackStart paramBox output PackGrow 0

    -- track load of new parameter sets or midiin
    forkIO $ do
      update' <- atomically $ dupTChan update
      reset' <- atomically $ dupTChan reset
      forever $ do
        let f n = postGUISync $ rangeSetValue input  (fromIntegral n)
        x <- atomically $ (Just <$> readTChan update') `orElse` (Nothing <$ readTChan reset')
        case x of
          Just (C ((==) param -> True) n) -> f n
          Nothing -> do
              n <- atomically $ do
                sel <- readTVar tsel
                view actual <$> flip (M.!) param  <$> view controls <$> flip (M.!) sel <$> readTVar tboard
              f n
          _ -> return ()


    forkIO $ do
      reset' <- atomically $ dupTChan reset
      forever $ do
        wx <- atomically $ do
            readTChan reset'
            sel <- readTVar tsel
            flip (M.!) param  <$> view controls <$> flip (M.!) sel <$> readTVar tboard
        postGUISync $  do
          rangeSetValue shiftScale . fromIntegral $ wx ^. cntrl . base
          rangeSetValue widthScale . fromIntegral $ wx ^. cntrl . width

    on input valueChanged $ do
        x <- floor <$> rangeGetValue input -- progressBarGetFraction input
        tiffe <- toggleButtonGetActive mute
        when (not tiffe) $ do
            t <- atomically $ do
              sel <- readTVar tsel
              wx <- flip (M.!) param  <$> view controls <$> flip (M.!) sel <$> readTVar tboard
              let t = floor $ fromIntegral x /128 *fromIntegral (wx ^. cntrl . width)  + fromIntegral (wx ^. cntrl . base)
              writeTChan midioutchan . C param $ t
              modifyTVar tboard $ ix sel . controls . ix param %~ actual  .~ z
              return t
            rangeSetValue output $ fromIntegral t

    on widthScale valueChanged $ do
      x <- floor <$> rangeGetValue widthScale -- progressBarGetFraction input
      atomically $ do
          sel <- readTVar tsel
          modifyTVar tboard $ ix sel . controls . ix param %~ cntrl . width  .~ x

    on shiftScale valueChanged $ do
      x <- floor <$> rangeGetValue shiftScale -- progressBarGetFraction input
      atomically $ do
          sel <- readTVar tsel
          modifyTVar tboard $ ix sel . controls . ix param %~ cntrl . base  .~ x

  window `on` deleteEvent $ liftIO mainQuit >> return False
  widgetShowAll window
  mainGUI

limitedAdd xm d x
        | x + d > xm = xm
        | otherwise = x + d
limitedSubtract xm d x
        | x - d < xm = xm
        | otherwise = x - d
