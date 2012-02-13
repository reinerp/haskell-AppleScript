{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Quasi where

import Data.Text.Lazy(Text)
import Foreign.AppleScript(applescript)

f = [applescript|
    tell application "iTunes" 
      playpause
    end tell
    
    $return[ "x" ]$
    
    tell application $value{"iTunes" :: String}$
      stop
    end tell
    
    $callback{\t -> return ("Hello" :: Text)}$[ "s" ]$
    
    $applescript{ undefined }$|]

