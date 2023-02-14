{-|
Module: Termbox2
Description: Bindings to the termbox2 console UI library.
License: AGPL-3-or-later
Maintainer: gatlin@niltag.net
Stability: experimental
Portability: POSIX

Bindings to the C-language termbox2 console UI library.
 -}
module Termbox2
  (
  -- * Termbox2 monad
    Termbox2
  , runTermbox2
  , init
  , initFile
  , initFd
  , initRwFd
  , shutdown
  , width
  , height
  , present
  , clear
  , setClearAttrs
  , setCursor
  , hideCursor
  , setCell
  , setInputMode
  , setOutputMode
  , print
  , pollEvent
  , peekEvent
  -- * Events
  , Tb2Event(..)
  -- * Constants
  -- ** Errors
  , Tb2Err(..)
  , errOk
  , errErr
  , errNeedMore
  , errInitAlready
  , errInitOpen
  , errMem
  , errNoEvent
  , errNoTerm
  , errNotInit
  , errOutOfBounds
  , errRead
  , errResizeIOCTL
  , errResizePipe
  , errResizeSigaction
  , errPoll
  , errUnsupportedTerm
  , errResizeWrite
  , errResizePoll
  , errResizeRead
  , errResizeSscanf
  , errCapCollision
  -- ** Keys
  , Tb2Key(..)
  , keyCtrlTilde
  , keyCtrl2
  , keyCtrlA
  , keyCtrlB
  , keyCtrlC
  , keyCtrlD
  , keyCtrlE
  , keyCtrlF
  , keyCtrlG
  , keyCtrlBackspace
  , keyCtrlH
  , keyCtrlTab
  , keyCtrlI
  , keyCtrlJ
  , keyCtrlK
  , keyCtrlL
  , keyCtrlEnter
  , keyCtrlM
  , keyCtrlN
  , keyCtrlO
  , keyCtrlP
  , keyCtrlQ
  , keyCtrlR
  , keyCtrlS
  , keyCtrlT
  , keyCtrlU
  , keyCtrlV
  , keyCtrlW
  , keyCtrlX
  , keyCtrlY
  , keyCtrlZ
  , keyCtrlEsc
  , keyCtrlLsqBracket
  , keyCtrl3
  , keyCtrl4
  , keyCtrlBackslash
  , keyCtrl5
  , keyCtrlRsqBracket
  , keyCtrl6
  , keyCtrl7
  , keyCtrlSlash
  , keyCtrlUnderscore
  , keySpace
  , keyBackspace2
  , keyCtrl8
  , keyF1
  , keyF2
  , keyF3
  , keyF4
  , keyF5
  , keyF6
  , keyF7
  , keyF8
  , keyF9
  , keyF10
  , keyF11
  , keyF12
  , keyInsert
  , keyDelete
  , keyHome
  , keyEnd
  , keyPgUp
  , keyPgDn
  , keyArrowUp
  , keyArrowDown
  , keyArrowLeft
  , keyArrowRight
  , keyBackTab
  , keyMouseLeft
  , keyMouseRight
  , keyMouseMiddle
  , keyMouseRelease
  , keyMouseWheelUp
  , keyMouseWheelDown
  -- ** Modifiers
  , Tb2Mod(..)
  , modAlt
  , modCtrl
  , modShift
  , modMotion
  -- ** Caps
  , Tb2Cap(..)
  , capF1
  , capF2
  , capF3
  , capF4
  , capF5
  , capF6
  , capF7
  , capF8
  , capF9
  , capF10
  , capF11
  , capF12
  , capInsert
  , capDelete
  , capHome
  , capEnd
  , capPgUp
  , capPgDn
  , capArrowUp
  , capArrowDown
  , capArrowLeft
  , capArrowRight
  , capBackTab
  , capEnterCA
  , capExitCA
  , capShowCursor
  , capHideCursor
  , capClearScreen
  , capSGR0
  , capUnderline
  , capBold
  , capBlink
  , capItalic
  , capReverse
  , capEnterKeypad
  , capExitKeypad
  -- ** Event types
  , Tb2EventType(..)
  , eventKey
  , eventResize
  , eventMouse
  -- ** Colors & Attributes
  , Tb2ColorAttr(..)
  , colorBlack
  , colorRed
  , colorGreen
  , colorYellow
  , colorBlue
  , colorMagenta
  , colorCyan
  , colorWhite
  , colorDefault
  , attrBold
  , attrUnderline
  , attrReverse
  , attrItalic
  , attrBlink
  -- ** Input modes
  , Tb2Input(..)
  , inputCurrent
  , inputEsc
  , inputAlt
  , inputMouse
  -- ** Output modes
  , Tb2Output(..)
  , outputCurrent
  , outputNormal
  , output256
  , output216
  , outputGrayscale
  )
where

import Prelude hiding (init, print)
import Data.Bits (Bits(..), (.|.))
import Data.Int (Int32)
import Foreign.C (CInt(..))
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT)

#include "termbox.h"

-- * Events

-- | Incoming event from the tty.
data Tb2Event = Tb2Event
  { _type :: Tb2EventType -- ^ one of TB_EVENT_* constants
  , _mod  :: Tb2Mod       -- ^ bitwise TB_MOD_* constants
  , _key  :: Tb2Key       -- ^ one of TB_KEY_* constants
  , _ch   :: Int32        -- ^ a Unicode code point
  , _w    :: Int32        -- ^ resize width
  , _h    :: Int32        -- ^ resize height
  , _x    :: Int32        -- ^ mouse x
  , _y    :: Int32        -- ^ mouse y
  } deriving (Show, Eq, Ord)

instance Storable Tb2Event where
  alignment _ = (#alignment struct tb_event)
  sizeOf    _ = (#size struct tb_event)
  peek ptr    = Tb2Event
    <$> (#peek struct tb_event, type) ptr
    <*> (#peek struct tb_event, mod) ptr
    <*> (#peek struct tb_event, key) ptr
    <*> (#peek struct tb_event, ch) ptr
    <*> (#peek struct tb_event, w) ptr
    <*> (#peek struct tb_event, h) ptr
    <*> (#peek struct tb_event, x) ptr
    <*> (#peek struct tb_event, y) ptr
  poke ptr (Tb2Event _t _m _k _c _w _h _x _y) = do
    (#poke struct tb_event, type) ptr _t
    (#poke struct tb_event, mod) ptr  _m
    (#poke struct tb_event, key) ptr  _k
    (#poke struct tb_event, ch) ptr   _c
    (#poke struct tb_event, w) ptr    _w
    (#poke struct tb_event, h) ptr    _h
    (#poke struct tb_event, x) ptr    _x
    (#poke struct tb_event, y) ptr    _y

-- Foreign functions

foreign import ccall unsafe "tb_init"
  ffi_tb_init :: IO CInt
foreign import ccall unsafe "tb_init_file"
  ffi_tb_init_file :: CString -> IO CInt
foreign import ccall unsafe "tb_init_fd"
  ffi_tb_init_fd :: CInt -> IO CInt
foreign import ccall unsafe "tb_init_rwfd"
  ffi_tb_init_rwfd :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "tb_shutdown"
  ffi_tb_shutdown :: IO CInt
foreign import ccall unsafe "tb_width"
  ffi_tb_width :: IO CInt
foreign import ccall unsafe "tb_height"
  ffi_tb_height :: IO CInt
foreign import ccall unsafe "tb_clear"
  ffi_tb_clear:: IO CInt
foreign import ccall unsafe "tb_set_clear_attrs"
  ffi_tb_set_clear_attrs :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "tb_present"
  ffi_tb_present :: IO CInt
foreign import ccall unsafe "tb_set_cursor"
  ffi_tb_set_cursor :: CInt -> CInt -> IO CInt
foreign import ccall unsafe  "tb_hide_cursor"
  ffi_tb_hide_cursor :: IO CInt
foreign import ccall unsafe  "tb_set_cell"
  ffi_tb_set_cell :: CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe  "tb_set_input_mode"
  ffi_tb_set_input_mode :: CInt -> IO CInt
foreign import ccall unsafe  "tb_set_output_mode"
  ffi_tb_set_output_mode :: CInt -> IO CInt
foreign import ccall unsafe  "tb_peek_event"
  ffi_tb_peek_event :: Ptr Tb2Event -> CInt -> IO CInt
foreign import ccall unsafe  "tb_poll_event"
  ffi_tb_poll_event :: Ptr Tb2Event -> IO CInt
foreign import ccall unsafe  "tb_print"
  ffi_tb_print :: CInt -> CInt -> CInt -> CInt -> CString -> IO CInt

-- * Constants
newtype Tb2Key = Tb2Key CInt
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Storable)
#{enum Tb2Key, Tb2Key
  , keyCtrlTilde                  = TB_KEY_CTRL_TILDE
  , keyCtrl2                      = TB_KEY_CTRL_2
  , keyCtrlA                      = TB_KEY_CTRL_A
  , keyCtrlB                      = TB_KEY_CTRL_B
  , keyCtrlC                      = TB_KEY_CTRL_C
  , keyCtrlD                      = TB_KEY_CTRL_D
  , keyCtrlE                      = TB_KEY_CTRL_E
  , keyCtrlF                      = TB_KEY_CTRL_F
  , keyCtrlG                      = TB_KEY_CTRL_G
  , keyCtrlBackspace              = TB_KEY_BACKSPACE
  , keyCtrlH                      = TB_KEY_CTRL_H
  , keyCtrlTab                    = TB_KEY_TAB
  , keyCtrlI                      = TB_KEY_CTRL_I
  , keyCtrlJ                      = TB_KEY_CTRL_J
  , keyCtrlK                      = TB_KEY_CTRL_K
  , keyCtrlL                      = TB_KEY_CTRL_L
  , keyCtrlEnter                  = TB_KEY_ENTER
  , keyCtrlM                      = TB_KEY_CTRL_M
  , keyCtrlN                      = TB_KEY_CTRL_N
  , keyCtrlO                      = TB_KEY_CTRL_O
  , keyCtrlP                      = TB_KEY_CTRL_P
  , keyCtrlQ                      = TB_KEY_CTRL_Q
  , keyCtrlR                      = TB_KEY_CTRL_R
  , keyCtrlS                      = TB_KEY_CTRL_S
  , keyCtrlT                      = TB_KEY_CTRL_T
  , keyCtrlU                      = TB_KEY_CTRL_U
  , keyCtrlV                      = TB_KEY_CTRL_V
  , keyCtrlW                      = TB_KEY_CTRL_W
  , keyCtrlX                      = TB_KEY_CTRL_X
  , keyCtrlY                      = TB_KEY_CTRL_Y
  , keyCtrlZ                      = TB_KEY_CTRL_Z
  , keyCtrlEsc                    = TB_KEY_ESC
  , keyCtrlLsqBracket             = TB_KEY_CTRL_LSQ_BRACKET
  , keyCtrl3                      = TB_KEY_CTRL_3
  , keyCtrl4                      = TB_KEY_CTRL_4
  , keyCtrlBackslash              = TB_KEY_CTRL_BACKSLASH
  , keyCtrl5                      = TB_KEY_CTRL_5
  , keyCtrlRsqBracket             = TB_KEY_CTRL_RSQ_BRACKET
  , keyCtrl6                      = TB_KEY_CTRL_6
  , keyCtrl7                      = TB_KEY_CTRL_7
  , keyCtrlSlash                  = TB_KEY_CTRL_SLASH
  , keyCtrlUnderscore             = TB_KEY_CTRL_UNDERSCORE
  , keySpace                      = TB_KEY_SPACE
  , keyBackspace2                 = TB_KEY_BACKSPACE2
  , keyCtrl8                      = TB_KEY_CTRL_8
  , keyF1                         = TB_KEY_F1
  , keyF2                         = TB_KEY_F2
  , keyF3                         = TB_KEY_F3
  , keyF4                         = TB_KEY_F4
  , keyF5                         = TB_KEY_F5
  , keyF6                         = TB_KEY_F6
  , keyF7                         = TB_KEY_F7
  , keyF8                         = TB_KEY_F8
  , keyF9                         = TB_KEY_F9
  , keyF10                        = TB_KEY_F10
  , keyF11                        = TB_KEY_F11
  , keyF12                        = TB_KEY_F12
  , keyInsert                     = TB_KEY_INSERT
  , keyDelete                     = TB_KEY_DELETE
  , keyHome                       = TB_KEY_HOME
  , keyEnd                        = TB_KEY_END
  , keyPgUp                       = TB_KEY_PGUP
  , keyPgDn                       = TB_KEY_PGDN
  , keyArrowUp                    = TB_KEY_ARROW_UP
  , keyArrowDown                  = TB_KEY_ARROW_DOWN
  , keyArrowLeft                  = TB_KEY_ARROW_LEFT
  , keyArrowRight                 = TB_KEY_ARROW_RIGHT
  , keyBackTab                    = TB_KEY_BACK_TAB
  , keyMouseLeft                  = TB_KEY_MOUSE_LEFT
  , keyMouseRight                 = TB_KEY_MOUSE_RIGHT
  , keyMouseMiddle                = TB_KEY_MOUSE_MIDDLE
  , keyMouseRelease               = TB_KEY_MOUSE_RELEASE
  , keyMouseWheelUp               = TB_KEY_MOUSE_WHEEL_UP
  , keyMouseWheelDown             = TB_KEY_MOUSE_WHEEL_DOWN
}

newtype Tb2Cap = Tb2Cap CInt
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)
#{enum Tb2Cap, Tb2Cap
  , capF1                                 = TB_CAP_F1
  , capF2                                 = TB_CAP_F2
  , capF3                                 = TB_CAP_F3
  , capF4                                 = TB_CAP_F4
  , capF5                                 = TB_CAP_F5
  , capF6                                 = TB_CAP_F6
  , capF7                                 = TB_CAP_F7
  , capF8                                 = TB_CAP_F8
  , capF9                                 = TB_CAP_F9
  , capF10                                = TB_CAP_F10
  , capF11                                = TB_CAP_F11
  , capF12                                = TB_CAP_F12
  , capInsert                             = TB_CAP_INSERT
  , capDelete                             = TB_CAP_DELETE
  , capHome                               = TB_CAP_HOME
  , capEnd                                = TB_CAP_END
  , capPgUp                               = TB_CAP_PGUP
  , capPgDn                               = TB_CAP_PGDN
  , capArrowUp                            = TB_CAP_ARROW_UP
  , capArrowDown                          = TB_CAP_ARROW_DOWN
  , capArrowLeft                          = TB_CAP_ARROW_LEFT
  , capArrowRight                         = TB_CAP_ARROW_RIGHT
  , capBackTab                            = TB_CAP_BACK_TAB
  , capEnterCA                            = TB_CAP_ENTER_CA
  , capExitCA                             = TB_CAP_EXIT_CA
  , capShowCursor                         = TB_CAP_SHOW_CURSOR
  , capHideCursor                         = TB_CAP_HIDE_CURSOR
  , capClearScreen                        = TB_CAP_CLEAR_SCREEN
  , capSGR0                               = TB_CAP_SGR0
  , capUnderline                          = TB_CAP_UNDERLINE
  , capBold                               = TB_CAP_BOLD
  , capBlink                              = TB_CAP_BLINK
  , capItalic                             = TB_CAP_ITALIC
  , capReverse                            = TB_CAP_REVERSE
  , capEnterKeypad                        = TB_CAP_ENTER_KEYPAD
  , capExitKeypad                         = TB_CAP_EXIT_KEYPAD
}

newtype Tb2ColorAttr = Tb2ColorAttr CInt
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Bits)
#{enum Tb2ColorAttr, Tb2ColorAttr
  , colorBlack                = TB_BLACK
  , colorRed                  = TB_RED
  , colorGreen                = TB_GREEN
  , colorYellow               = TB_YELLOW
  , colorBlue                 = TB_BLUE
  , colorMagenta              = TB_MAGENTA
  , colorCyan                 = TB_CYAN
  , colorWhite                = TB_WHITE
  , colorDefault              = TB_DEFAULT
  , attrBold                  = TB_BOLD
  , attrUnderline             = TB_UNDERLINE
  , attrReverse               = TB_REVERSE
  , attrItalic                = TB_ITALIC
  , attrBlink                 = TB_BLINK
}

instance Semigroup Tb2ColorAttr where
  (<>) = (.|.)

newtype Tb2EventType = Tb2EventType CInt
  deriving (Eq, Ord, Num, Enum, Real, Integral, Storable)
#{enum Tb2EventType, Tb2EventType
  , eventKey = TB_EVENT_KEY
  , eventResize = TB_EVENT_RESIZE
  , eventMouse = TB_EVENT_MOUSE
}

instance Show Tb2EventType where
  show evt@(Tb2EventType raw)
    | evt == eventKey = "[Key]"
    | evt == eventResize = "[Resize]"
    | evt == eventMouse = "[Mouse]"
    | otherwise = concat ["[Unknown event type: ", show raw, "]" ]

newtype Tb2Mod = Tb2Mod CInt
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Bits, Storable)
#{enum Tb2Mod, Tb2Mod
  , modAlt = TB_MOD_ALT
  , modCtrl = TB_MOD_CTRL
  , modShift = TB_MOD_SHIFT
  , modMotion = TB_MOD_MOTION
}

instance Semigroup Tb2Mod where
  (<>) = (.|.)

newtype Tb2Input = Tb2Input CInt
  deriving (Show, Eq, Ord, Num, Enum,   Real, Integral, Bits)
#{enum Tb2Input, Tb2Input
  , inputCurrent = TB_INPUT_CURRENT
  , inputEsc = TB_INPUT_ESC
  , inputAlt = TB_INPUT_ALT
  , inputMouse = TB_INPUT_MOUSE
}

instance Semigroup Tb2Input where
  (<>) = (.|.)

newtype Tb2Output = Tb2Output CInt
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Bits)
#{enum Tb2Output, Tb2Output
  , outputCurrent = TB_OUTPUT_CURRENT
  , outputNormal = TB_OUTPUT_NORMAL
  , output256 = TB_OUTPUT_256
  , output216 = TB_OUTPUT_216
  , outputGrayscale = TB_OUTPUT_GRAYSCALE
}

instance Semigroup Tb2Output where
  (<>) = (.|.)

newtype Tb2Err = Tb2Err CInt
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)
#{enum Tb2Err, Tb2Err
  , errOk = TB_OK
  , errErr = TB_ERR
  , errNeedMore = TB_ERR_NEED_MORE
  , errInitAlready = TB_ERR_INIT_ALREADY
  , errInitOpen = TB_ERR_INIT_OPEN
  , errMem = TB_ERR_MEM
  , errNoEvent = TB_ERR_NO_EVENT
  , errNoTerm = TB_ERR_NO_TERM
  , errNotInit = TB_ERR_NOT_INIT
  , errOutOfBounds = TB_ERR_OUT_OF_BOUNDS
  , errRead = TB_ERR_READ
  , errResizeIOCTL = TB_ERR_RESIZE_IOCTL
  , errResizePipe = TB_ERR_RESIZE_PIPE
  , errResizeSigaction = TB_ERR_RESIZE_SIGACTION
  , errPoll = TB_ERR_POLL
  , errUnsupportedTerm = TB_ERR_UNSUPPORTED_TERM
  , errResizeWrite = TB_ERR_RESIZE_WRITE
  , errResizePoll = TB_ERR_RESIZE_POLL
  , errResizeRead = TB_ERR_RESIZE_READ
  , errResizeSscanf = TB_ERR_RESIZE_SSCANF
  , errCapCollision = TB_ERR_CAP_COLLISION
}

-- | Enables writing text-based user interfaces with termbox2.
newtype Termbox2 a = Termbox2 (ReaderT (Ptr Tb2Event) (ExceptT Tb2Err IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadError Tb2Err
           , MonadReader (Ptr Tb2Event) )

-- | Allocates the 'Tb2Event' struct pointer, runs the UI, and frees.
runTermbox2 :: Termbox2 a -> IO (Either Tb2Err a)
runTermbox2 (Termbox2 m) = alloca (runExceptT . runReaderT m)

-- | Must be called before anything else. The termbox2 documentation notes that
-- handling some exceptions requires calling 'shutdown' followed by 'init'
-- again, hence this is not invoked automatically by 'runTermbox2'.
init :: Termbox2 ()
init = do
  ret <- liftIO ffi_tb_init
  if errOk == Tb2Err ret
    then return ()
    else throwError (Tb2Err ret)

initFile :: String -> Termbox2 ()
initFile filePath = do
  ret <- liftIO $ withCString filePath ffi_tb_init_file
  if errOk == Tb2Err ret
    then return ()
    else throwError (Tb2Err ret)

initFd :: (Integral n) => n -> Termbox2 ()
initFd fd = do
  ret <- liftIO $ ffi_tb_init_fd (fromIntegral fd)
  if errOk == Tb2Err ret
    then return ()
    else throwError (Tb2Err ret)

initRwFd :: (Integral n, Integral o) => n -> o -> Termbox2 ()
initRwFd rfd wfd = do
  ret <- liftIO $ ffi_tb_init_rwfd (fromIntegral rfd) (fromIntegral wfd)
  if errOk == Tb2Err ret
    then return ()
    else throwError (Tb2Err ret)

-- | Call this when you're finished or your terminal will act funky after exit!
shutdown :: Termbox2 ()
shutdown = do
  ret <- liftIO ffi_tb_shutdown
  if errOk == Tb2Err ret
    then return ()
    else throwError (Tb2Err ret)

-- | Width of the drawing space in characters.
width :: Integral n => Termbox2 n
width = liftIO ffi_tb_width >>= return . fromIntegral

-- | Height of the drawing space in lines.
height :: Integral n => Termbox2 n
height = liftIO ffi_tb_height >>= return . fromIntegral

-- | Draws the buffer to the screen.
present :: Termbox2 ()
present = do
  ret <- liftIO ffi_tb_present
  if errOk == Tb2Err ret
    then return ()
    else throwError (Tb2Err ret)

-- | Clears the screen.
clear :: Termbox2 ()
clear = do
  ret <- liftIO ffi_tb_clear
  if errOk == Tb2Err ret
    then return ()
    else throwError (Tb2Err ret)

-- | Specify the foreground and background attributes to be applied when
-- 'clear'ing the buffer.
setClearAttrs :: Tb2ColorAttr -> Tb2ColorAttr -> Termbox2 ()
setClearAttrs (Tb2ColorAttr fg) (Tb2ColorAttr bg) = do
  ret <- liftIO $ ffi_tb_set_clear_attrs fg bg
  if errOk == Tb2Err ret
    then return ()
    else throwError (Tb2Err ret)

-- | Set the location of the cursor (upper-left character is origin).
setCursor :: Int -> Int -> Termbox2 ()
setCursor x y = do
  ret <- liftIO $ ffi_tb_set_cursor (fromIntegral x) (fromIntegral y)
  if errOk == Tb2Err ret
    then return ()
    else throwError (Tb2Err ret)

-- | Hide the mouse pointer.
hideCursor :: Termbox2 ()
hideCursor = do
  ret <- liftIO ffi_tb_hide_cursor
  if errOk == Tb2Err ret
    then return ()
    else throwError (Tb2Err ret)

-- | Draw a single cell on the screen.
setCell
  :: Int
  -> Int
  -> Int32          -- ^ Unicode code point
  -> Tb2ColorAttr
  -> Tb2ColorAttr
  -> Termbox2 ()
setCell x y ch (Tb2ColorAttr fg) (Tb2ColorAttr bg) = do
  ret <- liftIO $ ffi_tb_set_cell
          (fromIntegral x)
          (fromIntegral y)
          (fromIntegral ch)
          fg
          bg
  if errOk == Tb2Err ret
    then return ()
    else throwError (Tb2Err ret)

-- | NB: If the argument is 'inputCurrent' then the function acts as a query
-- and returns the current input mode.
setInputMode :: Tb2Input -> Termbox2 (Maybe Tb2Input)
setInputMode im@(Tb2Input inputMode) = do
  ret <- liftIO $ ffi_tb_set_input_mode inputMode
  if im == inputCurrent
    then return $ Just (Tb2Input ret)
    else if errOk == Tb2Err ret
      then return Nothing
      else throwError (Tb2Err ret)

-- | NB: If the argument is 'outputCurrent' then the function acts as a query
-- and returns the current output mode.
setOutputMode :: Tb2Output -> Termbox2 (Maybe Tb2Output)
setOutputMode om@(Tb2Output outputMode) = do
  ret <- liftIO $ ffi_tb_set_output_mode outputMode
  if om == outputCurrent
    then return $ Just (Tb2Output ret)
    else if errOk == Tb2Err ret
      then return Nothing
      else throwError (Tb2Err ret)

-- | Prints a string of text to the screen.
print
  :: Int
  -> Int
  -> Tb2ColorAttr
  -> Tb2ColorAttr
  -> String
  -> Termbox2 ()
print x y (Tb2ColorAttr fg) (Tb2ColorAttr bg) str = do
  ret <- liftIO $ withCString str $ ffi_tb_print
          (fromIntegral x)
          (fromIntegral y)
          fg
          bg
  if errOk == Tb2Err ret
    then return ()
    else throwError (Tb2Err ret)

-- | Blocks until an exception is thrown or an event is observed.
pollEvent :: Termbox2 Tb2Event
pollEvent = do
  ptr <- ask
  ret <- liftIO (ffi_tb_poll_event ptr)
  if errOk == Tb2Err ret
    then liftIO (peek ptr)
    else throwError (Tb2Err ret)

-- | Blocks for the specified number of MILLISECONDS until an exception is
-- thrown or an event is received; returns 'Nothing' if the timeout is reached
-- without incident or event.
peekEvent :: Int -> Termbox2 (Maybe Tb2Event)
peekEvent waitMS = do
  ptr <- ask
  ret <- liftIO $ ffi_tb_peek_event ptr (fromIntegral waitMS)
  if errOk == Tb2Err ret
    then do
      evt <- liftIO (peek ptr)
      return (Just evt)
    else return Nothing
