{-# LANGUAGE QuasiQuotes #-}
import Foreign.AppleScript.Plain

main = runScript [applescript|
  tell application "iTunes"
    "
  end tell
 |]
