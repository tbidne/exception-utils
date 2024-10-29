{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This is essentially an addition to the @exceptions@ API. Note we do
-- __not__ export that API. This is merely some useful additional utilities.
--
-- @since 0.1
module Control.Exception.Utils
  ( -- * Throwing

    -- ** Text exception
    TextException (..),
    throwText,
    throwString,

    -- * Catching
    catchDeep,
    catchDeepSync,
    catchSync,
    catchIf,
    handleDeep,
    handleDeepSync,
    handleSync,
    handleIf,
    tryDeep,
    tryDeepSync,
    trySync,
    tryIf,

    -- * Cleanup
    onSyncException,

    -- * Exiting
    exitFailure,
    exitSuccess,
    exitWith,

    -- * Misc
    isSyncException,
    isAsyncException,
  )
where

import Control.DeepSeq (NFData, force)
import Control.Exception
  ( Exception (fromException, toException),
    SomeAsyncException (SomeAsyncException),
    SomeException,
  )
import Control.Exception qualified as E
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import Control.Monad.Catch qualified as C
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.IO.Exception (IOErrorType (InvalidArgument), IOException (IOError))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

-- | Exception that contains a text description.
--
-- @since 0.1
newtype TextException = MkTextException Text
  deriving stock (Eq, Show)

instance Exception TextException where
  displayException (MkTextException t) = T.unpack t

-- | Throws an exception with the 'Text' description.
--
-- @since 0.1
throwText :: forall m a. (HasCallStack, MonadThrow m) => Text -> m a
throwText = throwM . MkTextException
{-# INLINEABLE throwText #-}

-- | Throws an exception with the 'String' description.
--
-- @since 0.1
throwString :: forall m a. (HasCallStack, MonadThrow m) => String -> m a
throwString = throwText . T.pack
{-# INLINEABLE throwString #-}

-- | Like 'C.catch', except it fully evaluates the result to find impure
-- exceptions.
--
-- @since 0.1
catchDeep ::
  forall m e a.
  ( Exception e,
    HasCallStack,
    MonadCatch m,
    MonadIO m,
    NFData a
  ) =>
  m a ->
  -- | The exception handler.
  (e -> m a) ->
  m a
catchDeep action = C.catch (evaluateDeep =<< action)
{-# INLINEABLE catchDeep #-}

-- | 'catchDeep' specialized to synchronous 'SomeException'.
--
-- @since 0.1
catchDeepSync ::
  forall m a.
  ( HasCallStack,
    MonadCatch m,
    MonadIO m,
    NFData a
  ) =>
  m a ->
  -- | The exception handler.
  (SomeException -> m a) ->
  m a
catchDeepSync action = catchSync (evaluateDeep =<< action)
{-# INLINEABLE catchDeepSync #-}

-- | 'C.catch' specialized to catch all synchronous 'SomeException's.
catchSync ::
  forall m a.
  (HasCallStack, MonadCatch m) =>
  m a ->
  -- | The exception handler.
  (SomeException -> m a) ->
  m a
catchSync = catchIf @_ @SomeException isSyncException
{-# INLINEABLE catchSync #-}

-- | Catch an exception only if it satisfies a specific predicate.
catchIf ::
  forall m e a.
  (Exception e, HasCallStack, MonadCatch m) =>
  -- | The predicate.
  (e -> Bool) ->
  m a ->
  -- | The exception handler.
  (e -> m a) ->
  m a
catchIf p = C.catchJust (\e -> if p e then Just e else Nothing)
{-# INLINEABLE catchIf #-}

-- | Like 'C.try', except it fully evaluates the result to find impure
-- exceptions.
--
-- @since 0.1
tryDeep ::
  forall m e a.
  ( E.Exception e,
    MonadCatch m,
    MonadIO m,
    NFData a
  ) =>
  -- | The action.
  m a ->
  m (Either e a)
tryDeep action = C.try (evaluateDeep =<< action)
{-# INLINEABLE tryDeep #-}

-- | 'tryDeep' specialized to synchronous 'SomeException'.
--
-- @since 0.1
tryDeepSync ::
  forall m a.
  ( MonadCatch m,
    MonadIO m,
    NFData a
  ) =>
  -- | The action.
  m a ->
  m (Either SomeException a)
tryDeepSync action = trySync (evaluateDeep =<< action)
{-# INLINEABLE tryDeepSync #-}

-- | 'C.try' specialized to catch all synchronous 'SomeException's.
trySync ::
  forall m a.
  (HasCallStack, MonadCatch m) =>
  m a ->
  m (Either SomeException a)
trySync = tryIf @_ @SomeException isSyncException
{-# INLINEABLE trySync #-}

-- | Catch an exception only if it satisfies a specific predicate.
tryIf ::
  forall m e a.
  (Exception e, HasCallStack, MonadCatch m) =>
  -- | The predicate.
  (e -> Bool) ->
  m a ->
  m (Either e a)
tryIf p = C.tryJust (\e -> if p e then Just e else Nothing)
{-# INLINEABLE tryIf #-}

-- | Flipped 'catchDeep'.
--
-- @since 0.1
handleDeep ::
  forall m e a.
  ( Exception e,
    MonadCatch m,
    MonadIO m,
    NFData a
  ) =>
  -- | The exception handler.
  (e -> m a) ->
  m a ->
  m a
handleDeep = flip catchDeep
{-# INLINEABLE handleDeep #-}

-- | Flipped 'catchDeepSync'.
--
-- @since 0.1
handleDeepSync ::
  forall m a.
  (MonadCatch m, MonadIO m, NFData a) =>
  -- | The exception handler.
  (SomeException -> m a) ->
  m a ->
  m a
handleDeepSync = flip catchDeepSync
{-# INLINEABLE handleDeepSync #-}

-- | Flipped 'catchSync'.
--
-- @since 0.1
handleSync ::
  forall m a.
  (HasCallStack, MonadCatch m) =>
  (SomeException -> m a) ->
  m a ->
  m a
handleSync = flip catchSync
{-# INLINEABLE handleSync #-}

-- | Flipped 'catchIf'.
--
-- @since 0.1
handleIf ::
  forall m e a.
  (Exception e, HasCallStack, MonadCatch m) =>
  -- | The predicate.
  (e -> Bool) ->
  -- | The exception handler.
  (e -> m a) ->
  m a ->
  m a
handleIf p = flip (catchIf p)
{-# INLINEABLE handleIf #-}

-- | Like 'C.onException', except it does not catch asynchronous exception.
--
-- @since 0.1
onSyncException ::
  forall m a b.
  (HasCallStack, MonadCatch m) =>
  m a ->
  m b ->
  m a
onSyncException action handler =
  withFrozenCallStack catchSync action (\e -> handler >> throwM e)
{-# INLINEABLE onSyncException #-}

-- | The computation 'exitFailure' is equivalent to
-- 'exitWith' @(@'ExitFailure' /exitfail/@)@,
-- where /exitfail/ is implementation-dependent.
--
-- @since 0.1
exitFailure :: forall m a. (HasCallStack, MonadThrow m) => m a
exitFailure = exitWith (ExitFailure 1)
{-# INLINEABLE exitFailure #-}

-- | The computation 'exitSuccess' is equivalent to
-- 'exitWith' 'ExitSuccess', It terminates the program
-- successfully.
--
-- @since 0.1
exitSuccess :: forall m a. (HasCallStack, MonadThrow m) => m a
exitSuccess = exitWith ExitSuccess
{-# INLINEABLE exitSuccess #-}

-- | Lifted 'System.Exit.exitWith'.
--
-- @since 0.1
exitWith :: forall m a. (HasCallStack, MonadThrow m) => ExitCode -> m a
exitWith ExitSuccess = throwM ExitSuccess
exitWith code@(ExitFailure n)
  | n /= 0 = throwM code
  | otherwise =
      throwM
        ( IOError
            Nothing
            InvalidArgument
            "exitWith"
            "ExitFailure 0"
            Nothing
            Nothing
        )
{-# INLINEABLE exitWith #-}

-- | Returns 'True' iff the exception is not a subtype of 'SomeAsyncException'.
--
-- @since 0.1
isSyncException :: forall e. (Exception e) => e -> Bool
isSyncException e = case fromException (toException e) of
  Just SomeAsyncException {} -> False
  Nothing -> True

-- | Negation of 'isSyncException'.
--
-- @since 0.1
isAsyncException :: forall e. (Exception e) => e -> Bool
isAsyncException = not . isSyncException

-- | Evaluates a value deeply.
--
-- @since 0.1
evaluateDeep :: (MonadIO m, NFData a) => a -> m a
evaluateDeep = liftIO . E.evaluate . force
{-# INLINEABLE evaluateDeep #-}
