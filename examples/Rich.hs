{-# LANGUAGE QuasiQuotes #-}
import Foreign.AppleScript.Rich
import qualified Data.Text.Lazy    as Text
import qualified Data.Text.Lazy.IO as Text

main = Text.putStrLn =<< evalScript mainScript

mainScript = [applescript|
  tell application "System Events"
    -- Haskell value splices, and Unicode support.
    display dialog "The value of π is $value{pi :: Double}$."

    -- AppleScript can call back into Haskell.
    set yourName to text returned of (display dialog "What is your name?" default answer "")
    display dialog ("Your name in reverse is " & $callback{ \t -> return (Text.reverse t) }$[ yourName ]$)

    -- Splice other AppleScript code into here
    $applescript{ othergreeter }$

    -- Return text from AppleScript back to Haskell
    return "Hello from AppleScript!"
  end tell
 |]

othergreeter = [applescript|
  display dialog "Hello from the other greeter!"
 |]
