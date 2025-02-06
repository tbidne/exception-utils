{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides utilities for working with GHC's native Exception annotations.
--
-- @since 0.1
module Control.Exception.Annotation.Utils
  ( -- * Exception matching
    ExceptionProxy (..),
    matchesException,

    -- * Ignoring known callstacks
    setIgnoreKnownCallStackHandler,
    ignoreKnownCallStackHandler,
    ignoreCallStackHandler,
  )
where

import Control.Monad.Catch
  ( Exception (displayException, fromException, toException),
    SomeException,
  )
import Data.List qualified as L
import Data.Maybe qualified as Maybe
import GHC.Conc (getUncaughtExceptionHandler, setUncaughtExceptionHandler)

-- | Proxy for exception types. Used for matching multiple exception types.
--
-- @since 0.1
data ExceptionProxy = forall e. (Exception e) => MkExceptionProxy

-- | Augments the current global exception handler with the logic to ignore
-- callstacks on known exceptions, per the parameter proxy list. In other
-- words, merely calls 'displayException' on known exceptions. Note that
-- on older GHCs (< 9.12), this may still print a callstack as the callstack
-- is part of the 'SomeException' type, prior to the exceptions redesign.
--
-- @since 0.1
setIgnoreKnownCallStackHandler :: [ExceptionProxy] -> IO ()
setIgnoreKnownCallStackHandler proxies =
  getUncaughtExceptionHandler
    >>= setUncaughtExceptionHandler . ignoreKnownCallStackHandler proxies

-- | Augments the parameter handler with logic to avoid callstacks when
-- the exception matches one of the proxies.
--
-- @since 0.1
ignoreKnownCallStackHandler ::
  -- | Exception proxies to match.
  [ExceptionProxy] ->
  -- | Previous handler, to use when we do not have a match.
  (SomeException -> IO ()) ->
  -- | Augmented handler.
  SomeException ->
  IO ()
ignoreKnownCallStackHandler proxies prevHandler ex =
  if matchesException proxies ex
    then ignoreCallStackHandler ex
    else prevHandler ex

-- | Exception handler that merely calls 'displayException', thereby avoiding
-- callstacks for GHC >= 9.12.
--
-- @since 0.1
ignoreCallStackHandler :: SomeException -> IO ()
ignoreCallStackHandler = putStrLn . displayException

-- | Returns true iff e matches some ExceptionProxy.
--
-- @since 0.1
matchesException :: (Exception e) => [ExceptionProxy] -> e -> Bool
matchesException proxies ex = L.any isMatch proxies
  where
    se = toException ex
    isMatch (MkExceptionProxy @ex) = Maybe.isJust (fromException @ex se)
