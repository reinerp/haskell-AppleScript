{-# LANGUAGE QuasiQuotes #-}
import Foreign.AppleScript.Plain

main = runScript [applescript|
  tell application "iTunes"
    missingMessage("x")
  end tell
  |]
