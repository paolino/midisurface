{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
-- module GUI where

import Graphics.UI.Gtk
import Control.Monad
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import Data.IORef
import Data.Text


import MidiComm

                
midichannel = 1
k = 1/127

main :: IO ()
main = do
  (midiinchan, midioutchan, _) <- midiInOut "midi control GUI" midichannel 
  thandle <- newTVarIO Nothing


  initGUI
  window <- windowNew

  -- main box   
  mainbox    <- vBoxNew False 1
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
                          containerBorderWidth := 10, containerChild := mainbox]
  -- main buttons
  coms <- hBoxNew False 1
  boxPackStart mainbox coms PackNatural 0
  res <- buttonNewWithLabel ("Sync"::Text)
  boxPackStart coms res PackNatural 0
  load <- buttonNewFromStock "Load"
  boxPackStart coms load PackNatural 0
  fc <- fileChooserButtonNew ("Select a file"::Text) FileChooserActionOpen
  boxPackStart coms fc PackGrow 0
  save <- buttonNewFromStock "Save"
  boxPackStart coms save PackNatural 0
  quit <- buttonNewFromStock "Quit"
  boxPackStart coms quit PackNatural 0

  -- main buttons actions
  quit `on` buttonActivated $ mainQuit
  save `on` buttonActivated $ do
        mname <- fileChooserGetFilename fc
        case mname of 
                Nothing -> return ()
                Just name -> do
                        h <- openFile name WriteMode 
                        atomically $ writeTVar thandle (Just h)
  load `on` buttonActivated $ do
        mname <- fileChooserGetFilename fc
        case mname of 
                Nothing -> return ()
                Just name -> do
                        h <- openFile name ReadMode 
                        atomically $ writeTVar thandle (Just h)
 
  -- knobs
  -- ad <- adjustmentNew 0 0 400 1 10 400
  -- sw <- scrolledWindowNew Nothing (Just ad)
  fillnobbox <- hBoxNew False 1
  boxPackStart mainbox fillnobbox PackNatural 0
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
          widgetSetSizeRequest param 30 15
          frame <- frameNew
          set frame [containerChild:= param]
          boxPackStart hbox frame PackNatural 0

          label <- labelNew (Just ("0"::Text))
          widgetSetSizeRequest label 30 15
          frame <- frameNew
          set frame [containerChild:= label]
          boxPackStart hbox frame PackNatural 0

          eb <- eventBoxNew
          memory <- labelNew (Just $ show (0,0))
          level <- progressBarNew 
          progressBarSetOrientation level ProgressBottomToTop
          set eb [containerChild:= level]
          widgetAddEvents eb [Button1MotionMask]
          boxPackStart hbox eb PackGrow 0
          progressBarSetFraction level 0
         
          exvalue <- newIORef 0
          mute <- toggleButtonNewWithLabel ("m" :: Text)
          widgetSetSizeRequest mute 30 16
          frame <- frameNew
          set frame [containerChild:= mute]
          boxPackStart hbox frame PackNatural 0
          mute `on` toggled $ do
                        a <- toggleButtonGetActive mute
                        if a then do 
                                x <- progressBarGetFraction level 
                                writeIORef exvalue x
                                progressBarSetFraction level 0
                                atomically $ writeTChan midioutchan (paramv,0)
                        else do 
                                x <- readIORef exvalue
                                progressBarSetFraction level x
                                atomically $ writeTChan midioutchan (paramv,floor $ x/k)
                 


          save `on` buttonActivated $ do
                        x <- progressBarGetFraction level 
                        mh <- atomically $ readTVar thandle
                        case mh of 
                                Nothing -> return ()
                                Just h -> hPutStrLn h $ show (floor $ x/k :: Int)
          load `on` buttonActivated $ do
                        mh <- atomically $ readTVar thandle
                        case mh of 
                                Nothing -> return ()
                                Just h -> do
                                        l <- hGetLine h
                                        let (wx :: Int) = read l      
                                        progressBarSetFraction level (fromIntegral wx * k)
                                        labelSetText label $ show wx
                                        labelSetText param $ show paramv
          dupchan <- atomically $ dupTChan midiinchan
          forkIO . forever $ do
                         
                        (tp,wx) <- atomically $ readTChan dupchan
                        case tp == paramv of 
                                False -> return ()
                                True ->  postGUISync $ do 
                                        wx' <- read `fmap` (labelGetText label)
                                        case  wx' /= wx of
                                                False -> return ()
                                                True -> do 
                                                        progressBarSetFraction level (fromIntegral wx * k)
                                                        labelSetText label $ show wx


          res `on` buttonActivated $ do
                        x <- progressBarGetFraction level 
                        atomically $ writeTChan midioutchan (paramv,floor $ x/k)
          on eb motionNotifyEvent $ do 
                (_,r) <- eventCoordinates
                (0,r') <- liftIO $ fmap read $ labelGetText memory
                liftIO $ do 
                        x <- progressBarGetFraction level 
                        let y = (if r < r' then limitedAdd 1 else limitedSubtract 0) k x
                        progressBarSetFraction level y
                        atomically $ writeTChan midioutchan (paramv,floor $ y/k)
                        labelSetText label $ show (floor $ y/k)
                        labelSetText memory $ show (0,r)
                return True
          on level scrollEvent $  tryEvent $ do 
                ScrollUp <- eventScrollDirection
                liftIO $ do 
                        x <- progressBarGetFraction level 
                        let y = limitedAdd 1 k x
                        atomically $ writeTChan midioutchan (paramv,floor $ y/k)
                        progressBarSetFraction level y
                        labelSetText label $ show (floor $ y/k)
          on level scrollEvent $  tryEvent $ do 
                ScrollDown <- eventScrollDirection
                liftIO $ do 
                        x <- progressBarGetFraction level 
                        let y = limitedSubtract 0 k x
                        atomically $ writeTChan midioutchan (paramv,floor $ y/k)
                        progressBarSetFraction level y
                        labelSetText label $ show (floor $ y/k)
  save `on` buttonActivated $ do
        mh <- atomically $ readTVar thandle
        case mh of 
                Nothing -> return ()
                Just h -> hClose h
  load `on` buttonActivated $ do
        mh <- atomically $ readTVar thandle
        case mh of 
                Nothing -> return ()
                Just h -> hClose h

  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

limitedAdd xm d x
        | x + d > xm = xm
        | otherwise = x + d
limitedSubtract xm d x
        | x - d < xm = xm
        | otherwise = x - d
