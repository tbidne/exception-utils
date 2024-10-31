{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides utilities for working with GHC's native Exception annotations.
--
-- @since 0.1
module Control.Exception.Annotation.Utils
  ( -- * Global mechanisms
    setUncaughtExceptionDisplay,
    setUncaughtExceptionDisplayInnerMatch,

    -- ** Utils
    ExceptionProxy (..),
    displayInner,
    displayInnerMatch,
    displayInnerMatchHandler,
  )
where

import Control.Monad.Catch
  ( Exception (displayException, fromException, toException),
    SomeException (SomeException),
  )
import Data.Maybe qualified as Maybe
import Data.Proxy (Proxy)
import Data.Typeable (cast)
import GHC.Conc (setUncaughtExceptionHandler)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

-- | Proxy for exception types. Used for matching multiple exception types.
--
-- @since 0.1
data ExceptionProxy = forall e. (Exception e) => MkExceptionProxy (Proxy e)

-- | Calls 'displayException' on 'Control.Exception.SomeException's _inner_
-- exception i.e. given @SomeException e@ calls @displayException e@. This
-- means we will not use SomeException's default annotation printing.
--
-- @since 0.1
displayInner :: forall e. (Exception e) => e -> String
displayInner = walkSomeEx . toException
  where
    walkSomeEx :: SomeException -> String
    walkSomeEx (SomeException innerEx1) = case cast innerEx1 of
      Just innerSomeEx@(SomeException _) -> walkSomeEx innerSomeEx
      Nothing -> displayException innerEx1

-- | @displayInnerMatch proxies e@ attempts to convert @e@ to one of the given
-- exception types. If successful, we call 'displayException' on the result.
-- Otherwise calls 'displayException' on the original parameter.
--
-- This can be useful when we do no want to use
-- 'Control.Exception.SomeException's @displayException@ when e.g. we do not
-- want to print annotations by default. For instance, we can use the following
-- to ensure we do not print callstacks for specific @Ex1@ and @Ex2@:
--
-- @
-- data Ex1 = ... deriving anyclass Exception
-- data Ex2 = ... deriving anyclass Exception
--
-- let  displayEx' :: SomeException -> String
--      displayEx' =
--        displayInnerMatch
--          [ MkExceptionProxy (Proxy \@Ex1),
--            MkExceptionProxy (Proxy \@Ex2)
--          ]
--
-- -- in main
-- setUncaughtExceptionHandler displayEx'
-- @
--
-- @since 0.1
displayInnerMatch :: forall e. (Exception e) => [ExceptionProxy] -> e -> String
displayInnerMatch proxies ex =
  case Maybe.mapMaybe toMatchedDesc proxies of
    (innerExDesc : _) -> innerExDesc
    _ -> displayException ex
  where
    se = toException ex

    toMatchedDesc :: ExceptionProxy -> Maybe String
    toMatchedDesc (MkExceptionProxy @ex _) = case fromException @ex se of
      Just e -> Just $ displayException e
      _ -> Nothing

-- | 'setUncaughtExceptionHandler' with 'displayInnerMatchHandler' i.e. calls
-- 'displayException' on any uncaught exceptions, passing the result to the
-- param handler. 'ExitSuccess' is ignored.
--
-- @since 0.1
setUncaughtExceptionDisplay :: (String -> IO ()) -> IO ()
setUncaughtExceptionDisplay = setUncaughtExceptionDisplayInnerMatch []
{-# INLINEABLE setUncaughtExceptionDisplay #-}

-- | Like 'setUncaughtExceptionDisplay', except we attempt to match the
-- given exception types.
--
-- @since 0.1
setUncaughtExceptionDisplayInnerMatch ::
  [ExceptionProxy] ->
  (String -> IO ()) ->
  IO ()
setUncaughtExceptionDisplayInnerMatch proxies =
  setUncaughtExceptionHandler . displayInnerMatchHandler proxies
{-# INLINEABLE setUncaughtExceptionDisplayInnerMatch #-}

-- | Calls 'displayInnerMatch' on the exception, passing the result to the
-- param handler. 'ExitSuccess' is ignored.
--
-- @since 0.1
displayInnerMatchHandler ::
  forall f.
  (Applicative f) =>
  -- | Proxies to attempt to match in 'displayInnerMatch'.
  [ExceptionProxy] ->
  -- | The handler.
  (String -> f ()) ->
  -- | The exception.
  SomeException ->
  -- | The result of the handler, or @pure ()@, if we encountered
  -- 'ExitSuccess'.
  f ()
displayInnerMatchHandler
  proxies
  handler
  ex = case fromException ex of
    -- Need to handle ExitSuccess, which this does.
    Just ExitSuccess -> pure ()
    Just (ExitFailure _) -> handler $ displayInnerMatch proxies ex
    Nothing -> handler $ displayInnerMatch proxies ex
{-# INLINEABLE displayInnerMatchHandler #-}
