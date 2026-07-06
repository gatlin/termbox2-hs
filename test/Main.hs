{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception(..), bracket_, throwIO)
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
  Tb2.print ((w - length prompt) `div` 2) (centerY + 2) Tb2.colorCyan Tb2.colorDefault prompt

-- Renders the active typing screen
renderGameScreen :: Int -> Int -> UTCTime -> GameState -> Termbox2 ()
renderGameScreen w h now state = do
  let target = targetText state
  let typed = typedText state
  let offset = viewOffset state
  let visibleLen = w - 4
  
  -- 1. Draw the target text (background)
  let displayTarget = take visibleLen (drop offset target)
  Tb2.print 2 2 Tb2.colorWhite Tb2.colorDefault displayTarget
  
  -- 2. Draw the typed text overlay
  let typedLen = length typed
  let typedVisible = take visibleLen (drop offset typed)
  
  forM_ (zip [0..] typedVisible) $ \(i, char) -> do
    let targetChar = target !! (offset + i)
    let color = if char == targetChar then Tb2.colorGreen else Tb2.colorRed
    Tb2.print (2 + i) 2 color Tb2.colorDefault [char]

  -- 3. Draw cursor
  when (typedLen >= offset && typedLen <= offset + visibleLen) $ do
    let cursorX = 2 + (typedLen - offset)
    Tb2.setCell cursorX 2 0x2588 Tb2.colorCyan Tb2.colorDefault

  -- 4. Draw Timer
  let timeRemaining = case startTime state of
        Just s -> max 0 (gameDuration - diffUTCTime now s)
        Nothing -> gameDuration
  let timerMsg = "Time Remaining: " ++ show (round timeRemaining :: Int) ++ "s"
  Tb2.print ((w - length timerMsg) `div` 2) 1 Tb2.colorYellow Tb2.colorDefault timerMsg

---------------------------------------------------------------------------------------
-- Game Logic
-------------------------------------------------------------------------

handleEvent :: Int -> Tb2.Tb2Event -> GameState -> UTCTime -> GameState
handleEvent w event state now = case (status state, event) of
  (Waiting, Tb2.Tb2Event (Tb2.Tb2Key c)) -> 
    state { status = Typing, startTime = Just now }
    
  (Typing, Tb2.Tb2Event (Tb2.Tb2Key c)) ->
    let currentTyped = typedText state
        targetChar = targetText state !! length currentTyped
        isMistake = c /= targetChar
        newMistakes = if isMistake then mistakeCount state + 1 else mistakeCount state
        newTyped = currentTyped ++ [c]
        -- Update offset for scrolling based on actual width
        newOffset = if length newTyped > (viewOffset state + (w - 4)) 
                    then viewOffset state + 1 else viewOffset state
    in state { typedText = newTyped, mistakeCount = newMistakes, viewOffset = newOffset }
    
  (Finished, Tb2.Tb2Event _) -> 
    initialState
    
  _ -> state

updateState :: Int -> UTCTime -> Maybe Tb2.Tb2Event -> GameState -> GameState
updateState w now mEvent state =
  let stateAfterEvent = case mEvent of
        Just ev -> handleEvent w ev state now
        Nothing -> state
  in case status stateAfterEvent of
    Typing -> 
      case startTime stateAfterEvent of
        Just s | diffUTCTime now s >= gameDuration -> 
          stateAfterEvent { status = Finished, endTime = Just now }
        _ -> stateAfterEvent
    _ -> stateAfterEvent

appLoop :: GameState -> Termbox2 ()
appLoop state = do
  (w, h) <- Tb2.termSize
  now <- liftIO getCurrentTime
  mEvent <- liftIO Tb2.pollEvent
  
  let newState = updateState w now mEvent state
  
  Tb2.clear
  screenBorder 1 w h
  case status newState of
    Waiting -> renderStartScreen w h
    Typing  -> renderGameScreen w h now newState
    Finished -> renderSummaryScreen w h newState
  Tb2.sync
  
  liftIO $ threadDelay 10000 -- 10ms
  appLoop newState

main :: IO ()
main = runTermbox2 (appLoop initialState)
