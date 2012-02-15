{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Foreign.AppleScript.Error where

import System.Exit
import Control.Exception
import Data.Typeable
import qualified Data.Text.Lazy as Text
import Data.Text.Format

-- | AppleScript signaled an error.
data AppleScriptError =
  AppleScriptError 
    { exitCode :: !Int,
      message :: !(Maybe Text.Text)
    }
 deriving(Typeable)

instance Show AppleScriptError where
  show (AppleScriptError c mt) = case mt of
    Nothing -> Text.unpack $ format "AppleScript error code {}" (Only c)
    Just t  -> Text.unpack $ format "AppleScript error (code {}): {}" (c, t)

instance Exception AppleScriptError

-- | AppleScript didn't return a value which could be coerced to text.
data AppleScriptNoReturn = AppleScriptNoReturn
  deriving(Typeable)

instance Show AppleScriptNoReturn where
  show AppleScriptNoReturn = "AppleScript didn't return a value which could be coerced to text"

instance Exception AppleScriptNoReturn

-- | On success, returns; otherwise throws 'AppleScriptError'.
ignoreResultOrThrow :: IO (ExitCode, Maybe Text.Text) -> IO ()
ignoreResultOrThrow mv = do
  v <- mv
  case v of
    (ExitSuccess, _) -> return ()
    (ExitFailure c, mt) -> throwIO $ AppleScriptError c mt

-- | On success, extracts the message; otherwise throws 'AppleScriptError' or 'AppleScriptNoReturn', as appropriate.
extractResultOrThrow :: IO (ExitCode, Maybe Text.Text) -> IO Text.Text
extractResultOrThrow mv = do
  v <- mv
  case v of
    (ExitSuccess, Just t) -> return t
    (ExitSuccess, Nothing) -> throwIO AppleScriptNoReturn
    (ExitFailure c, mt) -> throwIO $ AppleScriptError c mt
