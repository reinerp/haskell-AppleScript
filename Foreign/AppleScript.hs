-- | A module that enables you to compile and execute AppleScript.
module Foreign.AppleScript 
  (
  appleScriptAvailable,
  execAppleScript
  ) 
  where

import Control.Applicative
import Control.Exception(bracket)
import System.Exit
import Foreign
import Foreign.C
import Data.Text(Text)
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import Data.ByteString(useAsCStringLen, packCStringLen)

type OSStatus = Int32
data AEDesc = AEDesc

foreign import ccall unsafe "RunScript.h _hs_AEDescSize"
  aeDescSize :: CSize
foreign import ccall unsafe "RunScript.h _hs_initNull"
  initNull :: Ptr AEDesc -> IO ()
foreign import ccall unsafe "RunScript.h _hs_getUTF8Size"
  getUtf8Size :: Ptr AEDesc -> IO CPtrdiff
foreign import ccall unsafe "RunScript.h _hs_getData"
  getData :: Ptr AEDesc -> Ptr a -> CSize -> IO OSStatus
foreign import ccall unsafe "RunScript.h _hs_dispose"
  dispose :: Ptr AEDesc -> IO OSStatus

withAEDesc :: (Ptr AEDesc -> IO a) -> IO a
withAEDesc f =
  allocaBytes (fromIntegral aeDescSize) $ \aeptr ->
    bracket (initNull aeptr) (\_ -> dispose aeptr) $ \_ ->
      f aeptr

extractStringData :: Ptr AEDesc -> IO (Maybe Text)
extractStringData aedesc = do
  size <- getUtf8Size aedesc
  case size of
    -1 -> return Nothing
    _ -> allocaBytes (fromIntegral size) $ \dat -> do
      err <- getData aedesc dat (fromIntegral size)
      case err == noErr of
        False -> return Nothing
        True -> (Just . decodeUtf8) <$> packCStringLen (dat, fromIntegral size)

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
execAppleScript :: Text -> IO (ExitCode, Maybe Text)
execAppleScript script = withAEDesc $ \aedesc -> do
  let run (cstr,len) = cLowRunAppleScript cstr (fromIntegral len) aedesc
  osstatus <- useAsCStringLen (encodeUtf8 script) run
  mstr <- extractStringData aedesc
  return (fromOSStatus osstatus, mstr)

-- Not the world's finest functions...
noErr :: Int32
noErr = 0

fromOSStatus :: Int32 -> ExitCode
fromOSStatus n
  | n == noErr = ExitSuccess
  | otherwise  = ExitFailure (fromIntegral n)


