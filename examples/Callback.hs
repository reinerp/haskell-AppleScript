{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
import Foreign.AppleScript

import Data.Monoid((<>))
import Data.Text.Lazy(Text)

main = execAppleScript [applescript|
tell application "System Events"
  set yourName to text returned of (display dialog "What is your name?" default answer "" buttons {"OK"} default button 1)
  display dialog ($callback{\t -> return $ "Hello from Haskell, " <> t }$[ yourName ]$)
end tell
 |]
