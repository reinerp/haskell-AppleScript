{-# LANGUAGE QuasiQuotes #-}

import Foreign.AppleScript
-- open a location in the default webbrowser

main = runScript [applescript| open location $value{location}$ |]

location = "http://www.cs.nott.ac.uk/~wss"
