import Foreign.AppleScript
import Data.Text(pack)
-- open a location in the default webbrowser

main = do
  execAppleScript (pack script)

location = "http://www.cs.nott.ac.uk/~wss"

script = "open location " ++ show location
