{-# LANGUAGE QuasiQuotes #-}
import Foreign.AppleScript

-- Asks for your name and says hello
main = do
  putStrLn "What is your name?"
  nm <- getLine
  runScript [applescript|
    tell application "Finder"
      delay 1
      set volume 4
      say ("Hello " & $value{nm}$) using "Vicki"
    end tell
   |]
