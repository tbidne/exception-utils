{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4, 20, 0)

module Unit.Control.Exception.Annotation.Utils (tests) where

import Control.Monad.Catch qualified as C
import Control.Exception ( SomeException (SomeException), addExceptionContext)
import Control.Exception.Backtrace (collectBacktraces)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as T
import Control.Exception (Exception (toException))
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Control.Exception.Utils (trySync)
import Control.Exception.Annotation.Utils
  ( ExceptionProxy (MkExceptionProxy),
    displayInner,
    displayInnerMatch,
    displayInnerMatchHandler,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCase, (@=?))
import TestUtils qualified

tests :: TestTree
tests =
  testGroup
    "Control.Exception.Annotation.Utils"
    [ displayInnerTests,
      displayInnerMatchTests,
      displayNoCSIfMatchHandlerTests
    ]

data Ex = MkEx
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

displayInnerTests :: TestTree
displayInnerTests =
  testGroup
    "displayInner"
    [ displaysInner,
      displaysInnerNested
    ]

displaysInner :: (HasCallStack) => TestTree
displaysInner = testCase "Displays inner exception" $ do
  trySync (C.throwM MkEx) >>= \case
    Left e -> "MkEx" @=? displayInner e
    Right _ -> assertFailure "Error: did not catch expected exception."

displaysInnerNested :: (HasCallStack) => TestTree
displaysInnerNested = testCase "Displays inner exception when nested" $ do
  trySync (C.throwM nestedEx) >>= \case
    Left e -> "MkEx" @=? displayInner e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    nestedEx =
      SomeException $
        SomeException $
          SomeException $
            SomeException MkEx

data ExA = MkExA
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data ExB = MkExB
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data ExC = MkExC
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

displayInnerMatchTests :: TestTree
displayInnerMatchTests =
  testGroup
    "displayInnerMatch"
    [ displaysInnerForSingleMatch,
      displaysInnerForLaterMatch,
      displaysInnerForMultiMatch,
      displaysOuterForNoSingleMatch,
      displaysOuterForNoMultiMatch
    ]

displaysInnerForSingleMatch :: (HasCallStack) => TestTree
displaysInnerForSingleMatch = testCase "Does not display callstack for single match" $ do
  trySync (C.throwM MkExB) >>= \case
    Left e -> "MkExB" @=? displayInnerMatch matches e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches = [MkExceptionProxy (Proxy @ExB)]

displaysInnerForLaterMatch :: (HasCallStack) => TestTree
displaysInnerForLaterMatch = testCase "Does not display callstack for later match" $ do
  trySync (C.throwM MkExC) >>= \case
    Left e -> "MkExC" @=? displayInnerMatch matches e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExA),
        MkExceptionProxy (Proxy @ExC)
      ]

displaysInnerForMultiMatch :: (HasCallStack) => TestTree
displaysInnerForMultiMatch = testCase "Does not display callstack for match" $ do
  trySync (C.throwM MkExC) >>= \case
    Left e -> "MkExC" @=? displayInnerMatch matches e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExC),
        MkExceptionProxy (Proxy @ExC)
      ]

displaysOuterForNoSingleMatch :: (HasCallStack) => TestTree
displaysOuterForNoSingleMatch = testCase "Displays callstack for no single match" $ do
  trySync (C.throwM MkExC) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 5 expected (T.pack $ displayInnerMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches = [MkExceptionProxy (Proxy @ExB)]
    expected =
      [ "MkExC",
        "HasCallStack backtrace:",
        "  throwM, called at",
        "  displaysOuterForNoSingleMatch, called at"
      ]

displaysOuterForNoMultiMatch :: (HasCallStack) => TestTree
displaysOuterForNoMultiMatch = testCase "Displays callstack for no multi match" $ do
  trySync (C.throwM MkExB) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 5 expected (T.pack $ displayInnerMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExA),
        MkExceptionProxy (Proxy @ExC)
      ]
    expected =
      [ "MkExB",
        "HasCallStack backtrace:",
        "  throwM, called at",
        "  displaysOuterForNoMultiMatch, called at"
      ]

displayNoCSIfMatchHandlerTests :: TestTree
displayNoCSIfMatchHandlerTests =
  testGroup
    "displayInnerMatchHandler"
    [ displayNoCSIfMatchHandlerDefault,
      displayNoCSIfMatchHandlerNoMatches,
      displayNoCSIfMatchHandlerSkipsMatch,
      displayNoCSIfMatchHandlerSkipsCSMatch,
      displayNoCSIfMatchHandlerExitFailure,
      displayNoCSIfMatchHandlerNoExitSuccess
    ]

displayNoCSIfMatchHandlerDefault :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerDefault = testCase "Displays callstack by default" $ do
  backtraces <- collectBacktraces
  let ex = addExceptionContext backtraces (toException MkExA)
  str <- runDisplayNoCSIfMatchHandler [] ex
  TestUtils.assertContainsMinLines 5 expected (T.pack str)
  where
    expected =
      [ "MkExA",
        "HasCallStack backtrace:",
        "  collectBacktraces, called at",
        "  displayNoCSIfMatchHandlerDefault, called at",
        ""
      ]

displayNoCSIfMatchHandlerNoMatches :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerNoMatches = testCase "Displays callstack by no proxy matches" $ do
  backtraces <- collectBacktraces
  let ex = addExceptionContext backtraces (toException MkExA)
  str <- runDisplayNoCSIfMatchHandler proxies ex
  TestUtils.assertContainsMinLines 5 expected (T.pack str)
  where
    expected =
      [ "MkExA",
        "HasCallStack backtrace:",
        "  collectBacktraces, called at",
        "  displayNoCSIfMatchHandlerNoMatches, called at"
      ]
    proxies =
      [ MkExceptionProxy $ Proxy @ExB,
        MkExceptionProxy $ Proxy @ExC
      ]

displayNoCSIfMatchHandlerSkipsMatch :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerSkipsMatch = testCase "Does not display callstack for match" $ do
  backtraces <- collectBacktraces
  let ex = addExceptionContext backtraces (toException MkExB)
  str <- runDisplayNoCSIfMatchHandler [MkExceptionProxy $ Proxy @ExB] ex
  "MkExB" @=? str

displayNoCSIfMatchHandlerSkipsCSMatch :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerSkipsCSMatch = testCase "Does not display callstack for cs match" $ do
  backtraces <- collectBacktraces
  let ex = addExceptionContext backtraces (toException MkExB)
  str <- runDisplayNoCSIfMatchHandler proxies ex
  "MkExB" @=? str
  where
    proxies =
      [ MkExceptionProxy $ Proxy @ExB,
        MkExceptionProxy $ Proxy @ExC
      ]

displayNoCSIfMatchHandlerExitFailure :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerExitFailure = testCase "Displays callstack for ExitFailure" $ do
  backtraces <- collectBacktraces
  let ex = addExceptionContext backtraces (toException (ExitFailure 1))
  str <- runDisplayNoCSIfMatchHandler [] ex
  TestUtils.assertContainsMinLines 5 expected (T.pack str)
  where
    expected =
      [ "ExitFailure 1",
        "HasCallStack backtrace:",
        "  collectBacktraces, called at",
        "  displayNoCSIfMatchHandlerExitFailure, called at"
      ]

displayNoCSIfMatchHandlerNoExitSuccess :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerNoExitSuccess = testCase desc $ do
  backtraces <- collectBacktraces
  let ex = addExceptionContext backtraces (toException ExitSuccess)
  str <- runDisplayNoCSIfMatchHandler [] ex
  "" @=? str
  where
    desc = "Does not display callstack for ExitSuccess"

runDisplayNoCSIfMatchHandler :: [ExceptionProxy] -> SomeException -> IO String
runDisplayNoCSIfMatchHandler proxies = runTestIO . testIO
  where
    testIO :: SomeException -> TestIO ()
    testIO = displayInnerMatchHandler proxies testHandler

    runTestIO :: TestIO a -> IO String
    runTestIO m = do
      ref <- newIORef ""
      _ <- unTestIO m ref
      readIORef ref

    testHandler :: String -> TestIO ()
    testHandler str = do
      ref <- MkTestIO ask
      liftIO $ writeIORef ref str

newtype TestIO a = MkTestIO (ReaderT (IORef String) IO a)
  deriving (Applicative, Functor, Monad, MonadIO) via ReaderT (IORef String) IO

unTestIO :: TestIO a -> IORef String -> IO a
unTestIO (MkTestIO io) = runReaderT io

#else

module Unit.Control.Exception.Annotation.Utils (tests) where

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Control.Exception.Annotation.Utils" []

#endif
