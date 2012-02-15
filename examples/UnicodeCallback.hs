{-# LANGUAGE QuasiQuotes #-}
import Foreign.AppleScript
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy

main = runScript [applescript|
  tell application "System Events"
    display dialog $callback{ \t -> Lazy.putStrLn t >> return (Lazy.reverse t) }$[ "Grüße" ]$
  end tell
 |]
