{-# LANGUAGE QuasiQuotes #-}
import Foreign.AppleScript.Plain
import qualified Data.Text.Lazy.IO as Text

main = Text.putStrLn =<< evalScript [applescript|
  tell application "System Events"
    display dialog "Hello World!"
    return "Viele Grüße von AppleScript!"
  end tell
 |]
