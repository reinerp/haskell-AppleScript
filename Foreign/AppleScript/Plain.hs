{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
-- | Module for running plain AppleScript. For a richer interface, see "Foreign.AppleScript.Rich".
--
-- A complete example using this module:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > import Foreign.AppleScript.Plain
-- > import qualified Data.Text.Lazy.IO as Text
-- >
-- > main = Text.putStrLn =<< evalScript [applescript|
-- >   tell application "System Events"
-- >     display dialog "Hello World!"
-- >     
-- >     -- Unicode support
-- >     return "Viele Grüße von AppleScript!"
-- >   end tell
-- >  |]
module Foreign.AppleScript.Plain(
  appleScriptAvailable,
  runScript,
  evalScript,
  runScriptFull,
  applescript,
  AppleScript(..),
 ) where

import Data.String
import Language.Haskell.TH.Quote

import Control.Exception
import Control.Applicative
import System.Exit

import Foreign
import Foreign.C
import Foreign.AppleScript.Error

import          Data.Text.Encoding(decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy(Text, isPrefixOf)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.ByteString as BS

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
        True -> (Just . Text.fromStrict . decodeUtf8) <$> BS.packCStringLen (dat, fromIntegral size)

foreign import ccall "RunScript.h LowRunAppleScript" 
  cLowRunAppleScript :: CString -> CLong -> Ptr AEDesc -> IO OSStatus

-- | The 'appleScriptAvailable' function checks whether or not AppleScript is
-- available on your platform.
foreign import ccall "RunScript.h AppleScriptAvailable" appleScriptAvailable :: IO Bool

newtype AppleScript = AppleScript { unAppleScript :: Text }
  deriving(IsString)

-- | Run the script using 'runScriptFull', throwing an exception on failure.
-- Ignores the script's result value.
runScript :: AppleScript -> IO ()
runScript script = ignoreResultOrThrow (runScriptFull script)

-- | Run the script using 'runScriptFull', throwing an exception on
-- failure. Returns the script's result value, coerced to text.
--
-- See "Foreign.AppleScript.Error" for the possible exceptions.
evalScript :: AppleScript -> IO Text
evalScript script = extractResultOrThrow (runScriptFull script)

-- | The 'execAppleScript' function will attempt to compile and
-- execute the AppleScript, described in the @String@ it receives as
-- its argument.  Any result of running the script is discarded. The
-- @ExitCode@ indicates whether or not any errors were encountered
-- during compilation or execution.
runScriptFull :: AppleScript -> IO (ExitCode, Maybe Text)
runScriptFull (AppleScript script) = withAEDesc $ \aedesc -> do
  let run (cstr,len) = cLowRunAppleScript cstr (fromIntegral len) aedesc
  osstatus <- BS.useAsCStringLen (encodeUtf8 . Text.toStrict $ script) run
  mstr <- extractStringData aedesc
  return (fromOSStatus osstatus, mstr)

-- Not the world's finest functions...
noErr :: Int32
noErr = 0

fromOSStatus :: Int32 -> ExitCode
fromOSStatus n
  | n == noErr = ExitSuccess
  | otherwise  = ExitFailure (fromIntegral n)


{- | 
Unescaped 'AppleScript', useful for AppleScript programs. See the
example at the top of this module.
-}
applescript :: QuasiQuoter
applescript = QuasiQuoter{quoteExp=driver, quotePat=undefined, quoteType=undefined, quoteDec=undefined}
 where
  driver string = [| AppleScript (Text.pack string) |]
