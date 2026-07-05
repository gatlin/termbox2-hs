module Main where

import System.IO
import Control.Monad (when)
import Data.Char (isPrint)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

-- Assuming the library provides TbEvent and TbEvent constructors
-- based on the error message and common naming conventions in such bindings.
import Termbox

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    
    -- Initialize termbox
    res <- termboxInit
    when (res /= 0) $ error "Failed to initialize termbox"
    
    -- Main loop
    loop
    
    termboxClose

loop :: IO ()
loop = do
    -- Wait for an event
    ev <- termboxPollEvent
    
    case ev of
        -- Use TbEvent constructors
        Just (TbEvent KeyEsc _) -> do
            termboxClose
            return ()
        
        Just (TbEvent (TbKey Char c) _) -> do
            -- Handle character input
            -- For a simple example, we just print the character
            -- Note: termbox usually handles output via its own API, 
            -- but for this demo we'll assume a simple interaction.
            putStrLn $ "Pressed: " ++ [c]
            loop
            
        _ -> loop
