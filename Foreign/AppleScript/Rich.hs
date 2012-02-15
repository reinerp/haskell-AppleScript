{-# LANGUAGE 
   TemplateHaskell, 
   OverloadedStrings, 
   ExistentialQuantification, 
   ViewPatterns, 
   TupleSections, 
   TypeSynonymInstances, 
   FlexibleInstances 
 #-}
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

-- |
-- This module supports a \"rich\" communication with AppleScript. Specifically, this
-- module provides support for AppleScript calling back into Haskell, as well as splicing
-- Haskell values into AppleScript code.
--
-- Here is an example which demonstrates the provided features.
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > import Foreign.AppleScript.Rich
-- > import qualified Data.Text.Lazy    as Text
-- > import qualified Data.Text.Lazy.IO as Text
-- >
-- > main = Text.putStrLn =<< evalScript mainScript
-- >
-- > mainScript = [applescript|
-- >   tell application "System Events"
-- >     -- Haskell value splices, and Unicode support.
-- >     display dialog "The value of π is $value{pi :: Double}$."
-- > 
-- >     -- AppleScript can call back into Haskell.
-- >     set yourName to text returned of (display dialog "What is your name?" default answer "")
-- >     display dialog ("Your name in reverse is " & $callback{ \t -> return (Text.reverse t) }$[ yourName ]$)
-- > 
-- >     -- Splice other AppleScript code into here
-- >     $applescript{ othergreeter }$
-- > 
-- >     -- Return text from AppleScript back to Haskell
-- >     return "Hello from AppleScript!"
-- >   end tell
-- >  |]
-- >
-- > othergreeter = [applescript|
-- >   display dialog "Hello from the other greeter!"
-- >  |]
--
-- The quasiquoter is 
module Foreign.AppleScript.Rich
  (
  -- * Common-use functions
  Plain.appleScriptAvailable,
  applescript,
  runScript,
  evalScript,
  debugScript,
  runScriptFull,
  -- * Syntax tree
  AppleScript(..),
  AppleScriptElement(..),
  AppleScriptValue(..),
  -- * Configuration
  AppleScriptConfig(..),
  def,
  ) 
  where

import           Foreign.AppleScript.Error
import qualified Foreign.AppleScript.Plain as Plain

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Resource(ResourceT, runResourceT, withIO)

import Control.Exception(tryJust, finally)
import Control.Concurrent(forkIO, killThread)

import System.IO
import System.IO.Error(ioeGetErrorType)
import System.IO.Unsafe(unsafePerformIO)
import GHC.IO.Exception(IOErrorType(InvalidArgument))

import System.Exit

import Network(accept, listenOn, sClose, PortID(..), PortNumber)

import Data.List(minimumBy)
import Data.Ord(comparing)
import Data.IORef
import Data.Default

import           Data.Text.Lazy(Text, isPrefixOf)
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text as Strict
import Data.Text.Format

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse(parseExp)

----------------------------------------
-- Insertion and callbacks support
----------------------------------------
{- |
A rich apple script is a concatenation of applescript elements. See the 'applescript' quasiquoter.
-}
data AppleScript = AppleScript [AppleScriptElement]

{- |
Plain AppleScript code, with fancy additions.
-}
data AppleScriptElement
 = PlainCode !Plain.AppleScript
   -- ^ Plain AppleScript code, to be inserted verbatim into the result.
 | Callback !(Text -> IO Text) !Plain.AppleScript
   -- ^ A callback into Haskell. Represents the @$callback{...}$[...]$@ construction.
   -- An AppleScript expression @Callback f arg@ is evaluated by AppleScript roughly as follows:
   --
   --  * evaluate the AppleScript code in @arg@ to produce a string @s@
   --
   --  * run @f s@ in Haskell, producing a new string @t@
   --
   --  * return the value @t@ as the result of this AppleScript expression.
   --
   -- Callbacks are implemented internally by setting up a server on the Haskell side,
   -- and connecting to this server from AppleScript using the unix command @nc@. The
   -- main caveat of this approach is that the message produced by the AppleScript code
   -- @arg@ is required to be only one line long.
 | forall a. AppleScriptValue a => Value !a
   -- ^ A Haskell value spliced into the applescript code. The Haskell value is serialized into
   -- AppleScript code using the function 'toAppleScriptCode'.
 | Nest AppleScript
   -- ^ Utility for avoiding appending lists. For example:
   --
   -- > f = AppleScript [
   -- >   PlainCode "...",
   -- >   Nest someOtherAppleScriptCode,
   -- >   PlainCode "..."
   -- >  ]

class AppleScriptValue a where
  -- | Serialise the given Haskell value into AppleScript code.
  toAppleScriptCode :: a -> Plain.AppleScript

instance AppleScriptValue Int where toAppleScriptCode = Plain.AppleScript . Text.pack . show
instance AppleScriptValue Double where toAppleScriptCode = Plain.AppleScript . Text.pack . show
instance AppleScriptValue String where toAppleScriptCode = Plain.AppleScript . Text.pack . show
instance AppleScriptValue Text where toAppleScriptCode = Plain.AppleScript . Text.pack . show
instance AppleScriptValue Strict.Text where toAppleScriptCode = Plain.AppleScript . Text.pack . show

-- | Configuration for 'runScriptFull'. Use 'def' to get a default configuration.
data AppleScriptConfig =
  AppleScriptConfig {
    debug :: Bool,
      -- ^ If true, 'runScript' will print the generated code to stdout before
      -- running it.
    portGen :: IO PortNumber,
      -- ^ Method for generating network ports to use for 'Callback's.
    extsName :: Text
      -- ^ To implement 'Callback's, 'runScript' inserts some extra AppleScript code at the
      -- start of the AppleScript. This code is inserted as a script whose name is set by 'extsName'.
      -- In the unlikely situation of name clashes, change 'extsName' as appropriate.
  }

{-# NOINLINE portGenCounter #-}
portGenCounter :: IORef PortNumber
portGenCounter = unsafePerformIO (newIORef 57700)

instance Default AppleScriptConfig where
  def = AppleScriptConfig {
    portGen = do
       port <- readIORef portGenCounter
       writeIORef portGenCounter (succ port)
       return port,
    extsName = "__hs_exts__",
    debug = False
   }
   
type CodeGenM = WriterT Builder.Builder (ResourceT IO)

-- | Same as 'runScript', but sets 'debug' to 'True', so that the generated code is printed before running.
debugScript :: AppleScript -> IO ()
debugScript script = ignoreResultOrThrow (runScriptFull def{debug=True} script)

-- | Run the script using 'runScriptFull', throwing an exception on failure.
-- Ignores the script's result value.
runScript :: AppleScript -> IO ()
runScript script = ignoreResultOrThrow (runScriptFull def script)

-- | Run the script using 'runScriptFull', throwing an exception on
-- failure. Returns the script's result value, coerced to text.
--
-- See "Foreign.AppleScript.Error" for the possible exceptions.
evalScript :: AppleScript -> IO Text
evalScript script = extractResultOrThrow (runScriptFull def script)

-- | Run the 'AppleScript' with the given 'AppleScriptConfig'.
--
-- Returns an exit code, together with useful text when available:
--
--   * if the script had an error, returns the error message
--
--   * if the script returns a value which could be coerced to text, returns this value
runScriptFull :: AppleScriptConfig -> AppleScript -> IO (ExitCode, Maybe Text)
runScriptFull conf script = runResourceT $ do
    builder <- execWriterT (addSpecialFunctions >> proc script)
    let code = Builder.toLazyText builder
    when (debug conf) $ liftIO $ Text.putStrLn code
    liftIO $ Plain.runScriptFull (Plain.AppleScript code)
  where
    
    tell_code :: Text -> CodeGenM ()
    tell_code t = tell (Builder.fromLazyText t)

    matchInvalidArgument InvalidArgument = Just ()
    matchInvalidArgument _ = Nothing

    serverLoop handler sock = loop where
      loop = do
        res <- tryJust (matchInvalidArgument . ioeGetErrorType) (accept sock)
        case res of
          Left () -> return () -- the socket was closed, which is normal operation
          Right (h,hostName,_) -> do
            void $ forkIO $
              (when (hostName == "localhost") $ talk handler h)
                `finally` hClose h
            loop

    success_signal = "success: "

    talk :: (Text -> IO Text) -> Handle -> IO ()
    talk handler h = do
      hSetBuffering h LineBuffering
      -- AppleScript's "do shell script" uses utf8; see https://developer.apple.com/library/mac/#technotes/tn2065/_index.html
      hSetEncoding h utf8
      request <- Text.hGetLine h
      response <- handler request
      Text.hPutStrLn h (Text.append success_signal response)
    
    addSpecialFunctions :: CodeGenM ()
    addSpecialFunctions = tell_code $ 
      Text.concat [
        "script ", extsName conf, "\n",
        "  on sendHaskellMessage(serverName, message)\n",
        "    if (count of (paragraphs of message)) > 1 then\n",
        "      error (\"callback was given more than one line of text: '\" & message & \"'\")\n",
        "    else\n",
        "      set resp to (do shell script \"echo \" & quoted form of message & \" | nc localhost \" & (quoted form of serverName))\n",
        "      if resp starts with ", Text.pack (show success_signal), " then\n",
        "        return ((characters ", Text.pack $ show $ 1 + Text.length success_signal, " thru (length of resp) of resp) as text)\n",
        "      else\n",
        "        error (\"callback didn't complete successfully. Response returned: '\" & resp & \"'\")\n",
        "      end if\n",
        "    end if\n",
        "  end sendHaskellMessage\n",
        "end script\n"
      ]

    proc :: AppleScript -> CodeGenM ()
    proc (AppleScript els) = mapM_ proc_el els

    proc_el :: AppleScriptElement -> CodeGenM ()
    proc_el (PlainCode (Plain.AppleScript t)) = tell_code t
    proc_el (Callback handler arg_code) = do
      -- get the port
      port <- liftIO $ portGen conf

      -- start the callback server
      (_, sock) <- lift $
        withIO
          (listenOn (PortNumber port))
          sClose
          -- (const $ return ())
      void $ lift $ withIO
        (forkIO $ serverLoop handler sock)
        killThread

      -- return the code
      tell_code (format "({}'s sendHaskellMessage({}, {}))" (extsName conf, Shown (show port), Plain.unAppleScript arg_code))
    proc_el (Value v) = tell_code . Plain.unAppleScript . toAppleScriptCode $ v
    proc_el (Nest s) = proc s

----------------------------------------
-- Parsing
----------------------------------------
-- | Synonym for 'Plain.applescript'
applescriptplain :: QuasiQuoter
applescriptplain = Plain.applescript

type Parse = StateT Text Q

{- |
A quasiquoter for generating 'AppleScript'. This is mostly unescaped text, but with the following
special features:

 * Haskell values may be spliced directly into the code using the syntax 

     > $value{ <haskell code> }$

   See the 'Value' constructor for more details and types.

 * Callbacks into Haskell may be spliced using the syntax 

     > $callback{ <haskell code> }$[ <applescript code> ]$

   See the 'Callback' constructor for further details and types.

 * Extra AppleScript code may be inserted using the syntax

     > $applescript{ <haskell code> }

   See the 'Nest' constructor for further details and types.

See also the example at the top of this module.
-}
applescript :: QuasiQuoter
applescript = QuasiQuoter{quoteExp=driver, quotePat=undefined, quoteType=undefined, quoteDec=undefined}
 where
   driver string = [| AppleScript $(evalStateT go (Text.pack string)) |]

   callbackStart = "$callback{"
   valueStart = "$value{"
   nestStart = "$applescript{"
   starts = [callbackStart, valueStart, nestStart]

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
         cons [| PlainCode (Plain.AppleScript (Text.pack a)) |] go'
   
   -- precondition: we are looking at one of the 'starts', or else we are at eof
   go' :: Parse Exp
   go' = do
     t <- get
     if Text.null t then
       lift [| [] |]
       else do
         script_elem <- case t of
           ((callbackStart `isPrefixOf`) -> True) -> do
             void $ readTo callbackStart
             hask_code <- parseHs =<< readTo "}$["
             appl_code <- Text.unpack <$> readTo "]$"
             return [| Callback $hask_code (Plain.AppleScript (Text.pack appl_code)) |]
           ((valueStart `isPrefixOf`) -> True) -> do
             void $ readTo valueStart
             hask_code <- parseHs =<< readTo "}$"
             return [| Value $hask_code |]
           ((nestStart `isPrefixOf`) -> True) -> do
             void $ readTo nestStart
             hask_code <- parseHs =<< readTo "}$"
             return [| Nest $hask_code |]
           _ -> error "invariant violation in Foreign.AppleScript.Rich.applescript"
         cons script_elem go


