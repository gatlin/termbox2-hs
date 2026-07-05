{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Exception (Exception(..), bracket_, throwIO)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (chr, isPrint)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import Termbox2 (Termbox2, runTermbox2)
import qualified Termbox2 as Tb2

-----------------------------------------------------------------------------------------
-- supports graceful exits
-----------------------------------------------------------------------------------------

data Shutdown = Shutdown deriving (Show)
instance Exception Shutdown
halt :: MonadIO m => m a
halt = liftIO $! throwIO Shutdown

-----------------------------------------------------------------------------------------
-- Data Types & State
-----------------------------------------------------------------------------------------

data GameStatus = Waiting | Typing deriving (Eq, Show)

data GameState = GameState
  { targetText   :: String
  , typedText    :: String
  , viewOffset   :: Int
  , startTime    :: Maybe UTCTime
  , mistakeCount :: Int
  , status       :: GameStatus
  , flashUntil   :: Maybe UTCTime
  }

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

screenBorder :: Int -> Termbox2 ()
screenBorder border = do
  w <- Tb2.width
  h <- Tb2.height
  drawRect border border (w-2*border) (h-2*border)

-- Renders the "Press any key to start" screen
renderStartScreen :: Int -> Int -> Termbox2 ()
renderStartScreen w h = do
  let msg = "Press any key to start typing!"
  let x = (w - length msg) `div` 2
  let y = h `div` 2
  Tb2.print x y Tb2.colorCyan Tb2.colorDefault msg

-- Renders the typing test line in the middle of the screen
renderTypingTest :: Int -> Int -> UTCTime -> GameState -> Termbox2 ()
renderTypingTest w h now state = do
  let border = 2
  let lineWidth = w - 2 * border
  let centerY = h `div` 2
  let startX = border
  
  -- Determine if we are currently in a "mistake flash" state
  let isFlashing = case flashUntil state of
                     Just t  -> now < t
                     Nothing -> False
  
  -- Extract the slice of text that fits on the current line
  let lineText = take lineWidth $ drop (viewOffset state) (targetText state)
  -- Extract the slice of what the user has typed that corresponds to this line
  let typedLine = drop (viewOffset state) (typedText state)
  let relativeCursor = length typedLine
  
  -- Draw the text
  forM_ (zip [0..] lineText) $ \(i, char) -> do
    let x = startX + i
    let y = centerY
    
    let (fg, bg) = if isFlashing
                   then -- Flash colors: Red background, White text
                        if i < relativeCursor
                        then let typedChar = typedLine !! i
                                 isCorrect = typedChar == char
                             in if isCorrect 
                                then (Tb2.colorWhite, Tb2.colorRed)
                                else (Tb2.colorBlack, Tb2.colorRed)
                        else if i == relativeCursor
                             then (Tb2.colorBlack, Tb2.colorWhite) -- Cursor
                             else (Tb2.colorWhite, Tb2.colorRed)
                   else -- Normal colors
                        if i < relativeCursor
                        then let typedChar = typedLine !! i
                                 isCorrect = typedChar == char
                             in if isCorrect 
                                then (Tb2.colorGreen, Tb2.colorDefault)
                                else (Tb2.colorRed, Tb2.colorDefault)
                        else if i == relativeCursor
                             then (Tb2.colorBlack, Tb2.colorGreen) -- Cursor
                             else (Tb2.colorWhite, Tb2.colorDefault) -- Upcoming
    
    Tb2.print x y fg bg [char]

-- Renders the WPM and Accuracy stats in the top right
renderStats :: Int -> Int -> UTCTime -> GameState -> Termbox2 ()
renderStats w h now state = do
  let border = 2
  let x = w - 20
  let y = border
  
  let elapsed = maybe 0 (\t -> realToFrac (diffUTCTime now t) / 60) (startTime state)
  let charsTyped = length (typedText state)
  
  -- Net WPM = ((Total Chars / 5) - Mistakes) / Minutes
  let wpm = if elapsed > 0 
            then max 0 ((fromIntegral charsTyped / 5.0) - fromIntegral (mistakeCount state)) / elapsed
            else 0
            
  let accuracy = if charsTyped == 0 
                 then 100 
                 else (1.0 - (fromIntegral (mistakeCount state) / fromIntegral (max 1 charsTyped))) * 100
  
  let statsStr = "WPM: " ++ show (round wpm :: Int) ++ " | Acc: " ++ show (round accuracy :: Int) ++ "%"
  Tb2.print x y Tb2.colorCyan Tb2.colorDefault statsStr

-----------------------------------------------------------------------------------------
-- Application Logic
-----------------------------------------------------------------------------------------

-- Pure logic to handle state transitions
handleEvent :: Tb2.Tb2Event -> Int -> UTCTime -> GameState -> Maybe GameState
handleEvent evt lineWidth now state = 
  case (Tb2._key evt, Tb2._ch evt) of
    (k, _) | k == Tb2.keyCtrlQ -> Nothing -- Signal to halt
    
    -- If we are waiting, any key starts the game
    (_, _) | status state == Waiting -> 
      Just state { status = Typing, startTime = Just now }
    
    -- Now we handle the Typing state
    (k, c) | k == Tb2.keyBackspace || c == 8 || c == 127 -> 
      let newTypedText = if null (typedText state) then "" else init (typedText state)
          newCursorIdx = length newTypedText
          -- If the cursor moves back before the current view, shift the view back
          nextOffset = if newCursorIdx < viewOffset state
                       then max 0 (viewOffset state - lineWidth)
                       else viewOffset state
      in Just state { typedText = newTypedText, viewOffset = nextOffset }
    
    -- Character input: only if it's a printable character
    (_, c) -> 
      let char = chr (fromIntegral c)
      in if isPrint char
         then let currentIdx = length (typedText state)
                  isCorrect = char == (targetText state !! currentIdx)
                  
                  newTypedText = typedText state ++ [char]
                  newMistakes = if isCorrect then mistakeCount state else mistakeCount state + 1
                  
                  -- Trigger a flash if the character is incorrect
                  newFlash = if isCorrect 
                             then flashUntil state 
                             else Just (addUTCTime 0.1 now)
                                   
                  newCursorIdx = length newTypedText
                  -- If the cursor moves past the end of the current line, shift the view offset
                  nextOffset = if (newCursorIdx - viewOffset state) >= lineWidth
                               then viewOffset state + lineWidth
                               else viewOffset state
              in Just state { typedText = newTypedText
                            , viewOffset = nextOffset
                            , mistakeCount = newMistakes 
                            , flashUntil = newFlash
                            }
         else Just state -- Ignore non-printable characters

-- The main recursive loop
appLoop :: GameState -> Termbox2 ()
appLoop state = do
  w <- Tb2.width
  h <- Tb2.height
  now <- liftIO getCurrentTime
  
  -- 1. Render
  Tb2.clear
  screenBorder 2
  if status state == Waiting
    then renderStartScreen w h
    else do
      renderTypingTest w h now state
      renderStats w h now state
  Tb2.present
  
  -- 2. Poll
  evt <- Tb2.pollEvent
  case evt of
    Nothing -> appLoop state
    Just e  -> do
      -- 3. Update State
      let lineWidth = w - 4 -- matching the border of 2
      case handleEvent e lineWidth now state of
        Nothing -> return () -- Exit loop
        Just newState -> appLoop newState

setup :: Termbox2 ()
setup = do
  Tb2.init
  _ <- Tb2.setInputMode (Tb2.inputEsc <> Tb2.inputMouse)
  Tb2.clear
  Tb2.present

dispose :: Termbox2 ()
dispose = Tb2.shutdown

main :: IO ()
main = bracket_ (runTermbox2 setup) (runTermbox2 dispose) (runTermbox2 (appLoop initialState))
