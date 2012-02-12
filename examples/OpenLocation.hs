import Foreign.AppleScript

-- open a location in the default webbrowser

main = do
  execAppleScript script

location = "http://www.cs.nott.ac.uk/~wss"

script = "open location " ++ show location
