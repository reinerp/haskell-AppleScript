{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as Text
import Foreign.AppleScript

-- Displays a dialog window with unicode text

main = execAppleScript dialog

dialog = Text.unlines $ 
         [
         "tell application \"System Events\"",
         "display dialog \"Viele Grüße\"",
         "end tell"
         ] 


