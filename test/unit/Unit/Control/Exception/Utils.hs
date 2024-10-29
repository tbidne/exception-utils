{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4, 20, 0)

module Unit.Control.Exception.Utils (tests) where

import Control.Monad.Catch qualified as C
import Control.Exception ( ExceptionWithContext)
import Control.Exception (Exception)
import System.Exit (exitSuccess)
import Control.Exception.Utils (exitFailure, trySync)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCase)
import TestUtils qualified

tests :: TestTree
tests =
  testGroup
    "Control.Exception.Utils"
    [ throwsTests,
      catchTests
    ]

data Ex = MkEx
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

throwsTests :: TestTree
throwsTests =
  testGroup
    "Throwing"
    [ throwsExitFailure,
      throwsExitSuccess
    ]

throwsExitFailure :: TestTree
throwsExitFailure = testCase "Calls exitFailure" $ do
  trySync exitFailure >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 6 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "ExitFailure 1",
        "HasCallStack backtrace:",
        "  throwM, called at",
        "  exitWith, called at",
        "  exitFailure"
      ]

throwsExitSuccess :: TestTree
throwsExitSuccess =
  testCase "Calls exitSuccess" $
    trySync exitSuccess >>= \case
      Left e ->
        TestUtils.assertContainsMinLines 6 expected (TestUtils.displayExceptiont e)
      Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "ExitSuccess",
        "HasCallStack backtrace:",
        "  collectBacktraces, called at",
        "  toExceptionWithBacktrace, called at",
        "  throwIO, called at"
      ]

catchTests :: (HasCallStack) => TestTree
catchTests =
  testGroup
    "Catching"
    [ catchesContext,
      catchesOriginal,
      catchesGetsContext
    ]

catchesContext :: (HasCallStack) => TestTree
catchesContext = testCase "catches exception with context" $ do
  C.try @_ @(ExceptionWithContext Ex) (C.throwM MkEx) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 6 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "MkEx",
        "HasCallStack backtrace:",
        "  throwM, called at",
        "  catchesContext, called at",
        "  catchTests, called at"
      ]

catchesOriginal :: (HasCallStack) => TestTree
catchesOriginal = testCase "catches exception without stacktrace" $ do
  C.try @_ @Ex (C.throwM MkEx) >>= \case
    Left e -> TestUtils.assertContainsMinLines 1 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected = ["MkEx"]

-- Notice this does not include MkEx in the callstack :-(
catchesGetsContext :: (HasCallStack) => TestTree
catchesGetsContext = testCase "catches exception and gets context" $ do
  trySync (C.throwM MkEx) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 6 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "HasCallStack backtrace:",
        "  throwIO, called at",
        "  catchesGetsContext, called at",
        "  catchTests, called at"
      ]

#else

{-# OPTIONS_GHC -Wno-unused-imports #-}

-- Including these libs so we don't trip -Wunused-packages.
-- Remove once we drop support for base < 4.20.

module Unit.Control.Exception.Utils (tests) where

import Control.Exception.Utils ()
import Control.Monad.Catch ()
import Control.Monad.Trans.Reader ()
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Control.Exception.Utils" []

#endif
