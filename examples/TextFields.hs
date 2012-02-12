import Data.Text(pack)
import Foreign.AppleScript

-- Opens a small dialog window with a text field

main = execAppleScript (pack dialog)

dialog = unlines $ 
         [
         "tell application \"System Events\"",
         "display dialog \"What is your name?\" default answer \"\" buttons {\"OK\"} default button 1", 
         "end tell"
         ] 


