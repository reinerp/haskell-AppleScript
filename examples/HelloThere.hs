import Data.Text(pack)
import Foreign.AppleScript

-- Asks for your name and says hello

script nm = unlines $ 
            [
            "tell application \"Finder\"",
            " delay 1",
            " set volume 4",
            " say \"Hello " ++ nm ++"\" using  \"Vicki\"",
            "end tell"
            ]
main = do
  putStrLn "What is your name?"
  nm <- getLine
  execAppleScript (pack $ script nm)
  

  