{-# LANGUAGE ScopedTypeVariables, OverloadedStrings , TemplateHaskell,  ViewPatterns, Rank2Types#-}
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
import Control.Lens (over,view,preview,Prism',_1)
import Control.Lens.TH


import MidiComm

                
midichannel = 1

data Channell = Channell {
        _controls :: M.Map Int Int,
        _noteoos :: M.Map Int Bool
        } deriving (Show,Read)
makeLenses ''Channell

matchEv :: Prism' E (Int,Int) -> Int -> E -> Maybe Bool
matchEv x n = fmap ((==) n . view _1).preview x

main :: IO ()
main = do
  (midiinchan, midioutchan, _) <- midiInOut "midi control GUI" midichannel 
  thandle <- newTVarIO Nothing
  tboard <- newTVarIO $ M.fromList $ zip [0..15] $ repeat (Channell (M.fromList $ zip [0..127] $ repeat 0) (M.fromList $ zip [0..127] $ repeat False)):: IO (TVar (M.Map Int Channell))
  update <- newBroadcastTChanIO
  tsel <- newTVarIO 0

  initGUI
  window <- windowNew
  -- midi listening
  forkIO . forever . atomically $ do
                e <- readTChan midiinchan
                sel <- readTVar tsel
                modifyTVar tboard . flip M.adjust sel $ case e of
                        C tp wx -> over controls $ M.insert tp wx
                        Non n v -> over noteoos $ M.insert n True
                        Noff n v -> over noteoos $ M.insert n False
                writeTChan midioutchan e
                        
                writeTChan update $ Just e


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
            writeTChan update Nothing

      copy <- buttonNewWithLabel $ ("C"::Text)
      boxPackStart lb copy PackNatural 0
      boxPackStart lb res PackNatural 0
      copy `on` buttonActivated $ do
        atomically $ do 
            sel <- readTVar tsel 
            modifyTVar tboard $ \b -> M.insert n (b M.! sel) b 
            writeTChan update Nothing
        buttonClicked res
 
  new `on` buttonActivated $ do
        name <- entryGetText filename
        when (not . null $ name) $ do 
          readTVarIO tboard >>= writeFile name . show
          fileChooserSetFilename fc $ name
          return ()
  {- broken ?
  fc `on` fileActivated $  do 
        Just name <- fileChooserGetFilename fc
        entrySetText filename $ name
  -}
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
                        atomically $ writeTChan update Nothing
  -- knobs
  -- ad <- adjustmentNew 0 0 400 1 10 400
  -- sw <- scrolledWindowNew Nothing (Just ad)
  controlbox <- hBoxNew False 1
  notes <- vBoxNew False 1
  boxPackStart mainbox notes PackNatural 0
  boxPackStart mainbox controlbox PackNatural 0
  boxPackStart controlbox selecter PackNatural 0

  forM_ [0..3::Int] $ \m ->  do
          noteline <- hBoxNew False 1
          boxPackStart notes noteline PackNatural 0
          forM_ [0..11::Int] $ \n ->  do
              let n' = 36 + n + m * 12
              lb <- checkButtonNewWithLabel (printf "%03d" n' :: String)  
              boxPackStart noteline lb PackNatural 0
              lb `on` buttonPressEvent $ liftIO . atomically $ do
                        sel <- readTVar tsel
                        writeTChan midioutchan $ Non n' 127
                        modifyTVar tboard $ M.adjust (over noteoos $ M.insert n' True) sel
                        return False
              lb `on` buttonReleaseEvent $ liftIO . atomically $ do
                        sel <- readTVar tsel
                        writeTChan midioutchan $ Noff n' 0
                        modifyTVar tboard $ M.adjust (over noteoos $ M.insert n' False) sel
                        return False
              update' <- atomically $ dupTChan update
              forkIO . forever $ do 
                        x <- atomically $ do
                          r <- readTChan update' 
                          sel <- readTVar tsel
                          wx <- flip (M.!) n'  <$> view noteoos <$> flip (M.!) sel <$> readTVar tboard
                          return (r,wx)
                        let     t (fmap (matchEv _Non n') -> Just (Just True), True)  = do 
                                        postGUISync $ toggleButtonSetActive lb True
                                t (fmap (matchEv _Noff n') -> Just (Just True), False) = do
                                        postGUISync $ toggleButtonSetActive lb False
                                        print ("noff",n')
                                t (fmap (matchEv _Non n') -> Nothing, True)  = do
                                        print ("non",n')
                                        postGUISync $ buttonPressed lb
                                        atomically $ writeTChan midioutchan $ Noff n' 0
                                t (fmap (matchEv _Noff n') -> Nothing, False) =  do
                                        print ("noff",n')
                                        postGUISync $ buttonReleased lb
                                        atomically $ writeTChan midioutchan $ Non n' 127
                                t _ = return ()
                        t x
        

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
          let c = paramv `mod` 8
          when (c == 0) $ do
                  hbox    <- vBoxNew False 1
                  boxPackStart knobbox hbox PackNatural 0
                 
          hbox    <- vBoxNew False 1
          widgetSetSizeRequest hbox (-1) 135
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
                (x,wx) <- atomically $ do
                  r <- readTChan update'
                  sel <- readTVar tsel
                  wx <- flip (M.!) paramv  <$> view controls <$> flip (M.!) sel <$> readTVar tboard
                  return (r,wx)
                let     t (fmap (matchEv _C paramv) -> Just (Just True)) =
                                postGUISync $ rangeSetValue level  (fromIntegral wx)  -- progressBarSetFraction level (fromIntegral wx * k)
                        t (fmap (matchEv _C paramv) -> Nothing) = do
                                postGUISync $ rangeSetValue level  (fromIntegral wx)  -- progressBarSetFraction level (fromIntegral wx * k)
                                atomically $ writeTChan midioutchan $ C paramv wx
                        t _ = return ()
                t x
          on level valueChanged $ do 
              x <- rangeGetValue level -- progressBarGetFraction level 
              atomically $ do 
                  let z = floor x
                  writeTChan midioutchan $ C paramv z
                  sel <- readTVar tsel
                  modifyTVar tboard $ M.adjust (over controls $ M.insert paramv z) sel

  window `on` deleteEvent $ liftIO mainQuit >> return False
  widgetShowAll window
  mainGUI

limitedAdd xm d x
        | x + d > xm = xm
        | otherwise = x + d
limitedSubtract xm d x
        | x - d < xm = xm
        | otherwise = x - d
