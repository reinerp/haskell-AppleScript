-- | A module that enables you to compile and execute AppleScript.
module Foreign.AppleScript 
  (
  appleScriptAvailable,
  execAppleScript
  ) 
  where

import System.Exit
import Foreign
import Foreign.C


type OSStatus = Int32
data AEDesc = AEDesc

foreign import ccall "RunScript.h LowRunAppleScript" 
  cLowRunAppleScript :: CString -> CLong -> Ptr AEDesc -> IO OSStatus

-- | The 'appleScriptAvailable' function checks whether or not AppleScript is
-- available on your platform.
foreign import ccall "RunScript.h AppleScriptAvailable" appleScriptAvailable :: IO Bool

-- | The 'execAppleScript' function will attempt to compile and
-- execute the AppleScript, described in the @String@ it receives as
-- its argument.  Any result of running the script is discarded. The
-- @ExitCode@ indicates whether or not any errors were encountered
-- during compilation or execution.
execAppleScript :: String -> IO ExitCode
execAppleScript script = do
  let run (cstr,len) = cLowRunAppleScript cstr (fromIntegral len) nullPtr
  osstatus <- withCStringLen script run
  return (fromOSStatus osstatus)

-- Not the world's finest functions...
noErr :: Int32
noErr = 0

fromOSStatus :: Int32 -> ExitCode
fromOSStatus n
  | n == noErr = ExitSuccess
  | otherwise  = ExitFailure (fromIntegral n)


