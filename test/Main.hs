module Main (main) where

import Control.Monad (forM_, unless)
import Termbox2 (Termbox2, runTermbox2)
import qualified Termbox2 as Tb2

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

centerText :: String -> Termbox2 ()
centerText msg = do
  w <- Tb2.width
  h <- Tb2.height
  let cx = ((w `div` 2) - ((length msg) `div` 2))
  let cy = h `div` 2
  let fgAttrs = Tb2.colorGreen <> Tb2.attrUnderline <> Tb2.attrBold
  let bgAttrs = Tb2.colorMagenta
  Tb2.print cx cy fgAttrs bgAttrs msg

loop :: Termbox2 ()
loop = do
  evt <- Tb2.pollEvent
  unless (Tb2._key evt == Tb2.keyCtrlC) $! do
    Tb2.clear
    screenBorder 2
    centerText $! show evt
    Tb2.present
    loop

main :: IO ()
main = do
  ret <- runTermbox2 $! do
    Tb2.init
    _ <- Tb2.setInputMode (Tb2.inputEsc <> Tb2.inputMouse)
    Tb2.clear
    screenBorder 2
    centerText "=> Press the Any key ..."
    Tb2.present
    loop
    Tb2.shutdown
    return ()
  case ret of
    Left err -> putStrLn . show $ concat
      [ "Error: ", show err ]
    Right () -> putStrLn "Great success"
