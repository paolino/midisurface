{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
-- module GUI where

import Graphics.UI.Gtk
import Control.Monad
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import Data.IORef
import Data.Text hiding (zip,null)
import qualified Data.Map as M
import Text.Printf


import MidiComm

                
midichannel = 1
k = 1/127

main :: IO ()
main = do
  (midiinchan, midioutchan, _) <- midiInOut "midi control GUI" midichannel 
  thandle <- newTVarIO Nothing
  tboard <- newTVarIO $ M.fromList $ zip [0..15] $ repeat (M.fromList $ zip [0..127] $ repeat 0) :: IO (TVar (M.Map Int (M.Map Int Int)))
  update <- newBroadcastTChanIO
  tsel <- newTVarIO 0

  initGUI
  window <- windowNew
  -- midi listening
  forkIO . forever . atomically $ do
                (tp,wx) <- readTChan midiinchan
                sel <- readTVar tsel
                modifyTVar tboard $ M.adjust (M.insert tp wx) sel
                writeTChan update ()


  -- main box   
  mainbox    <- vBoxNew False 1
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
                          containerBorderWidth := 10, containerChild := mainbox]
  -- main buttons
  coms <- hBoxNew False 1
  boxPackStart mainbox coms PackNatural 0
  load <- buttonNewFromStock "Load"
  boxPackStart coms load PackNatural 0
  new <- buttonNewWithLabel ("New" :: Text)
  boxPackStart coms new PackNatural 0
  filename <- entryNew
  boxPackStart coms filename PackNatural 0
  fc <- fileChooserButtonNew ("Select a file"::Text) FileChooserActionOpen
  boxPackStart coms fc PackGrow 0
  save <- buttonNewFromStock "Save"
  boxPackStart coms save PackNatural 0
  quit <- buttonNewFromStock "Quit"
  boxPackStart coms quit PackNatural 0

  selecter <- vBoxNew False 1
  res0 <- radioButtonNewWithLabel ("00"::String)

  forM_ [0..15] $ \n ->  do
      lb <- hBoxNew False 1
      boxPackStart selecter lb PackNatural 0
      res <- case n of 
        0 -> return res0 
        _ -> radioButtonNewWithLabelFromWidget res0 (printf "%02d" n :: String) 
      
      res `on` buttonActivated $ do
        atomically $ do 
            writeTVar tsel $ n
            writeTChan update ()

      copy <- buttonNewWithLabel $ ("C"::Text)
      boxPackStart lb copy PackNatural 0
      boxPackStart lb res PackNatural 0
      copy `on` buttonActivated $ do
        atomically $ do 
            sel <- readTVar tsel 
            modifyTVar tboard $ \b -> M.insert n (b M.! sel) b 
            writeTChan update ()
        buttonClicked res
 
  new `on` buttonActivated $ do
        name <- entryGetText filename
        when (not . null $ name) $ do 
          readTVarIO tboard >>= writeFile name . show
          fileChooserSetFilename fc $ name
          return ()
  fc `on` fileActivated $  do 
        print "ah"
        Just name <- fileChooserGetFilename fc
        entrySetText filename $ name
  -- main buttons actions
  quit `on` buttonActivated $ mainQuit
  save `on` buttonActivated $ do
        mname <- fileChooserGetFilename fc
        case mname of 
                Nothing -> return ()
                Just name -> readTVarIO tboard >>= writeFile name . show
  load `on` buttonActivated $ do
        mname <- fileChooserGetFilename fc
        case mname of 
                Nothing -> return ()
                Just name -> do 
                        readFile name >>= atomically . writeTVar tboard . read
                        atomically $ writeTChan update ()
  -- knobs
  -- ad <- adjustmentNew 0 0 400 1 10 400
  -- sw <- scrolledWindowNew Nothing (Just ad)
  controlbox <- hBoxNew False 1
  boxPackStart mainbox controlbox PackNatural 0
  boxPackStart controlbox selecter PackNatural 0
  fillnobbox <- hBoxNew False 1
  boxPackStart controlbox fillnobbox PackNatural 0
  knoblines <- vBoxNew False 1
  boxPackStart fillnobbox knoblines PackNatural 0
   
  forM_ [0..3] $ \paramv' -> do
     knobboxspace    <- hBoxNew True 1
     widgetSetSizeRequest knobboxspace (-1) 10
     boxPackStart knoblines knobboxspace PackNatural 0
     knobbox    <- hBoxNew True 1
     boxPackStart knoblines knobbox PackNatural 0
     forM_ [0..31] $ \paramv'' -> do
          let paramv= paramv' *32 + paramv''
          when (paramv `mod` 8 == 0) $ do
                  hbox    <- vBoxNew False 1
                  boxPackStart knobbox hbox PackNatural 0
                 
          hbox    <- vBoxNew False 1
          widgetSetSizeRequest hbox (-1) 165
          boxPackStart knobbox hbox PackNatural 0

          param <- labelNew (Just $ show paramv)
          widgetSetSizeRequest param 28 15
          frame <- frameNew
          set frame [containerChild:= param]
          boxPackStart hbox frame PackNatural 0


          eb <- eventBoxNew
          memory <- newIORef 0
          ad <- adjustmentNew 0 0 127 1 1 1
          level <- vScaleNewWithRange 0 127 1 -- progressBarNew 
          rangeSetAdjustment level ad
          -- progressBarSetOrientation level ProgressBottomToTop
          set eb [containerChild:= level]
          widgetAddEvents eb [Button1MotionMask]
          boxPackStart hbox eb PackGrow 0
          rangeSetValue level 0 -- progressBarSetFraction level 0
         
          -- track load of new parameter sets or midiin
          update' <- atomically $ dupTChan update
          forkIO . forever $ do 
                wx <- atomically $ do
                  readTChan update'
                  sel <- readTVar tsel
                  flip (M.!) paramv <$> flip (M.!) sel <$> readTVar tboard
                atomically $ writeTChan midioutchan (paramv,wx)
                postGUISync $ do
                  rangeSetValue level  (fromIntegral wx)  -- progressBarSetFraction level (fromIntegral wx * k)
                  labelSetText param $ show paramv

          on level valueChanged $ do 
              x <- rangeGetValue level -- progressBarGetFraction level 
              atomically $ do 
                  let z = floor x
                  writeTChan midioutchan (paramv,z)
                  sel <- readTVar tsel
                  modifyTVar tboard $ M.adjust (M.insert paramv z) sel

  window `on` deleteEvent $ liftIO mainQuit >> return False
  widgetShowAll window
  mainGUI

limitedAdd xm d x
        | x + d > xm = xm
        | otherwise = x + d
limitedSubtract xm d x
        | x - d < xm = xm
        | otherwise = x - d
