{-# LANGUAGE TemplateHaskell, OverloadedStrings, ExistentialQuantification, ViewPatterns, TupleSections, 
    TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | A module that enables you to compile and execute AppleScript.
module Foreign.AppleScript
  (
  appleScriptAvailable,
  execAppleScript_basic,
  execAppleScript,
  applescript,
  AppleScriptValue(..),
  AppleScript(..),
  AppleScriptElement(..),
  AppleScriptCode,
  ) 
  where

import System.Exit
import Foreign
import Foreign.C

import System.IO(openTempFile, hClose, Handle)
import System.Directory(removeFile, getTemporaryDirectory)

import Control.Applicative
import Control.Monad.Trans(lift, liftIO)
import Control.Monad.State
import Control.Monad.Writer
import Language.Haskell.Meta.Parse(parseExp)
import Control.Monad.Trans.Resource(ResourceT, runResourceT, withIO)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.List(minimumBy)
import Data.Monoid((<>))
import Data.Ord(comparing)

import Control.Exception(finally)
import Control.Concurrent(forkIO, killThread)

import Network(accept, listenOn, sClose, PortID(..))

import qualified Data.Text.Lazy.IO as Text
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy(Text, isPrefixOf)
import Data.Text.Lazy.Encoding(encodeUtf8)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Format

type OSStatus = Int32
data AEDesc = AEDesc

foreign import ccall "RunScript.h LowRunAppleScript" 
  cLowRunAppleScript :: CString -> CLong -> Ptr AEDesc -> IO OSStatus

-- | The 'appleScriptAvailable' function checks whether or not AppleScript is
-- available on your platform.
foreign import ccall "RunScript.h AppleScriptAvailable" appleScriptAvailable :: IO Bool

type AppleScriptCode = Text

-- | The 'execAppleScript' function will attempt to compile and
-- execute the AppleScript, described in the @String@ it receives as
-- its argument.  Any result of running the script is discarded. The
-- @ExitCode@ indicates whether or not any errors were encountered
-- during compilation or execution.
execAppleScript_basic :: AppleScriptCode -> IO ExitCode
execAppleScript_basic script = do
  Text.putStrLn script
  let run (cstr,len) = cLowRunAppleScript cstr (fromIntegral len) nullPtr
  osstatus <- BS.useAsCStringLen (BS.concat . LBS.toChunks . encodeUtf8 $ script) run
  return (fromOSStatus osstatus)

-- Not the world's finest functions...
noErr :: Int32
noErr = 0

fromOSStatus :: Int32 -> ExitCode
fromOSStatus n
  | n == noErr = ExitSuccess
  | otherwise  = ExitFailure (fromIntegral n)


----------------------------------------
-- Insertion and callbacks support
----------------------------------------
data AppleScript = AppleScript [AppleScriptElement]

data AppleScriptElement
 = PlainCode !AppleScriptCode
 | Callback !(Text -> IO Text) !AppleScriptCode
-- -- | Return !AppleScriptCode
 | forall a. AppleScriptValue a => Value !a
 | Nest AppleScript

class AppleScriptValue a where
  toAppleScriptCode :: a -> AppleScriptCode

instance AppleScriptValue Int where toAppleScriptCode = Text.pack . show
instance AppleScriptValue Double where toAppleScriptCode = Text.pack . show
instance AppleScriptValue String where toAppleScriptCode = Text.pack . show

type CodeGenM = WriterT Builder.Builder (ResourceT IO)

execAppleScript :: AppleScript -> IO ExitCode
execAppleScript script = runResourceT $ do
    builder <- execWriterT (proc script >> addSpecialFunctions)
    liftIO $ execAppleScript_basic (Builder.toLazyText builder)
  where
    
    tell_code t = tell (Builder.fromLazyText t)

    talk :: (Text -> IO Text) -> Handle -> IO ()
    talk handler h = do
      request <- Text.hGetLine h
      response <- handler request
      Text.hPutStrLn h response
    
    addSpecialFunctions :: CodeGenM ()
    addSpecialFunctions = tell_code $ Text.unlines [
      "",
      "on __sendHaskellMessage__(serverName, message)",
      "  set line_count to count of (paragraphs of message)",
      "  return (do shell script \"echo \" & quoted form of (line_count & return & message & return) & \" | nc localhost \" & (quoted form of serverName))",
      "end __sendHaskellMessage__"
     ]

    proc :: AppleScript -> CodeGenM ()
    proc (AppleScript els) = mapM_ proc_el els

    proc_el :: AppleScriptElement -> CodeGenM ()
    proc_el (PlainCode t) = tell_code t
    -- TODO: proc_el Return
    proc_el (Callback handler arg_code) = do
{-      -- create a temporary file
      liftIO $ putStrLn "A"
      tmpDir <- liftIO getTemporaryDirectory
      (_, (unixsocket, hndl)) <- lift $
        withIO
          (openTempFile tmpDir "callback.unixsocket")
          (\(sock, hndl) -> removeFile sock)
      liftIO $ hClose hndl
      liftIO $ putStrLn unixsocket
  -}

      let port = 123456789
      -- start the callback server
      (_, sock) <- lift $
        withIO
          (listenOn (PortNumber port))
          sClose
{-      lift $ withIO
        (forkIO $ forever $ do
           (h,_,_) <- accept sock
           forkIO (talk handler h `finally` hClose h)
        )
        killThread-}

      -- return the code
      tell_code (format "my __sendHaskellMessage__({}, {})" (Shown (show port), arg_code))
    proc_el (Value v) = tell_code (toAppleScriptCode v)
    proc_el (Nest s) = proc s

----------------------------------------
-- Parsing
----------------------------------------
-- $callback{haskell code}$[ applescript code ]$
-- $return[ applescript code ]$
-- $value{haskell code}$
-- $applescript{haskell code}$


type Parse = StateT Text Q

applescript :: QuasiQuoter
applescript = QuasiQuoter{quoteExp=driver, quotePat=undefined, quoteType=undefined, quoteDec=undefined}
 where
   driver string = [| AppleScript $(evalStateT go (Text.pack string)) |]

   callbackStart = "$callback{"
--   returnStart = "$return["
   valueStart = "$value{"
   nestStart = "$applescript{"
   starts = [callbackStart, {-returnStart, -}valueStart, nestStart]

   readTo :: Text -> Parse Text
   readTo needle = StateT $ \haystack -> case Text.breakOn needle haystack of
     (l, Text.stripPrefix needle -> Just r) -> return (l, r)
     _ -> fail $ "Missing " <> Text.unpack needle

   parseHs :: Text -> Parse ExpQ
   parseHs t = case parseExp (Text.unpack t) of
     Right r -> return (return r)
     Left msg -> fail msg

   breakOnFirst :: [Text] -> Text -> (Text, Text)
   breakOnFirst needles haystack = 
     minimumBy 
       (comparing (Text.length . fst)) 
       (map (\n -> Text.breakOn n haystack) needles)

   cons :: Q Exp -> Parse Exp -> Parse Exp
   cons x xs = StateT $ \t -> (,t) <$> [| $x : $(evalStateT xs t) |]

   go :: Parse Exp
   go = do
     t <- get
     case breakOnFirst starts t of
       (Text.null -> True, b) -> do
         put b
         go'
       (Text.unpack -> a, b) -> do
         put b
         cons [| PlainCode (Text.pack a) |] go'
   
   -- precondition: we are looking at one of the 'starts', or else we are at eof
   go' :: Parse Exp
   go' = do
     t <- get
     if Text.null t then
       lift [| [] |]
       else do
         elem <- case t of
           ((callbackStart `isPrefixOf`) -> True) -> do
             readTo callbackStart
             hask_code <- parseHs =<< readTo "}$["
             appl_code <- Text.unpack <$> readTo "]$"
             return [| Callback $hask_code (Text.pack appl_code) |]
{-           ((returnStart `isPrefixOf`) -> True) -> do
             readTo returnStart
             appl_code <- Text.unpack <$> readTo "]$"
             return [| Return (Text.pack appl_code) |]-}
           ((valueStart `isPrefixOf`) -> True) -> do
             readTo valueStart
             hask_code <- parseHs =<< readTo "}$"
             return [| Value $hask_code |]
           ((nestStart `isPrefixOf`) -> True) -> do
             readTo nestStart
             hask_code <- parseHs =<< readTo "}$"
             return [| Nest $hask_code |]
         cons elem go


