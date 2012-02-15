{-# LANGUAGE QuasiQuotes #-}
import Foreign.AppleScript

main = runScript [applescript|
  tell application "System Events"
    -- we expect to be warned that we are evaluating the callback with multiple
    -- lines (since only the first one will be read in by Haskell)
    display dialog $callback{ return }$[ "hello\n goodbye" ]$
  end tell
 |]
