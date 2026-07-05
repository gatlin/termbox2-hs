{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.STM (TChan, newTChanIO, tryReadTChan, writeTChan, atomically)
import Control.Exception (Exception(..), bracket_, throwIO, finally)
import Control.Monad (forM_, when, forever)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (chr, isPrint)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime, NominalDiffTime)
import Termbox2 (Termbox2, runTermbox2)
import qualified Termbox2 as Tb2

-----------------------------------------------------------------------------------------
-- Data Types & State
-----------------------------------------------------------------------------------------

data GameStatus = Waiting | Typing | Finished deriving (Eq, Show)

data GameState = GameState
  { targetText   :: String
  , typedText    :: String
  , viewOffset   :: Int
  , startTime    :: Maybe UTCTime
  , endTime      :: Maybe UTCTime
  , mistakeCount :: Int
  , status       :: GameStatus
  , flashUntil   :: Maybe UTCTime
  }

-- Game configuration
gameDuration :: NominalDiffTime
gameDuration = 30 -- seconds

-- A static source of words to create our infinite stream
wordSource :: [String]
wordSource = 
  [ "haskell", "functional", "terminal", "cursor", "infinite", "lazy", "stream"
  , "software", "developer", "keyboard", "typing", "speed", "refactor", "monad"
  , "architecture", "recursive", "state", "machine", "buffer", "interface"
  ]

-- Generates an infinite stream of words separated by spaces
generateStream :: String
generateStream = unwords $ cycle wordSource

initialState :: GameState
initialState = GameState
  { targetText   = generateStream
  , typedText    = ""
  , viewOffset   = 0
  , startTime    = Nothing
  , endTime      = Nothing
  , mistakeCount = 0
  , status       = Waiting
  , flashUntil   = Nothing
  }

-----------------------------------------------------------------------------------------
-- drawing utilities
-----------------------------------------------------------------------------------------

drawRect :: Int -> Int -> Int -> Int -> Termbox2 ()
drawRect left top w h = do
  let bottom = top+h-1
  let right = left+w-1
  let setCell x y ch = Tb2.setCell x y ch Tb2.colorWhite Tb2.colorDefault
  setCell left top 0x250C
  setCell right top 0x2510
  setCell left bottom 0x2514
  setCell right bottom 0x2518
  forM_ [left+1..right-1] $ \i -> do
    setCell i top 0x2500
    setCell i bottom 0x2500
  forM_ [top+1..bottom-1] $ \i -> do
    setCell left i 0x2502
    setCell right i 0x2502

screenBorder :: Int -> Int -> Int -> Termbox2 ()
screenBorder border w h = do
  drawRect border border (w-2*border) (h-2*border)

-- Renders the "Press any key to start" screen
renderStartScreen :: Int -> Int -> Termbox2 ()
renderStartScreen w h = do
  let msg = "Press any key to start typing!"
  let x = (w - length msg) `div` 2
  let y = h `div` 2
  Tb2.print x y Tb2.colorCyan Tb2.colorDefault msg

-- Renders the final summary screen
renderSummaryScreen :: Int -> Int -> GameState -> Termbox2 ()
renderSummaryScreen w h state = do
  let centerY = h `div` 2
  let startX = (w - 30) `div` 2
  
  -- Calculate final stats
  let elapsed = case (startTime state, endTime state) of
                  (Just s, Just e) -> realToFrac (diffUTCTime e s) / 60
                  _               -> 0
  let charsTyped = length (typedText state)
  
  let wpm = if elapsed > 0 
            then max 0 ((fromIntegral charsTyped / 5.0) - fromIntegral (mistakeCount state)) / elapsed
            else 0
            
  let accuracy = if charsTyped == 0 
                 then 100 
                 else (1.0 - (fromIntegral (mistakeCount state) / fromIntegral (max 1 charsTyped))) * 100
  
  let title = "TEST COMPLETE!"
  let stats = "WPM: " ++ show (round wpm :: Int) ++ " | Acc: " ++ show (round accuracy :: Int) ++ "%"
  let prompt = "Press any key to try again"
  
  Tb2.print ((w - length title) `div` 2) (centerY - 2) Tb2.colorYellow Tb2.colorDefault title
  Tb2.print ((w - length stats) `div` 2) centerY Tb2.colorWhite Tb2.colorDefault stats
  Tb2.print ( ( la l l l l l l l l l l l l l l l l l l l l l l l l l l l l l l l