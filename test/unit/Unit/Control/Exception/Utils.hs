{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4, 20, 0)

module Unit.Control.Exception.Utils (tests) where

import Control.Exception (Exception, ExceptionWithContext)
import Control.Exception qualified as E
import Control.Exception.Context qualified as Ctx
import Control.Exception.Utils (exitFailure)
import Control.Monad.Catch qualified as C
import Data.Text qualified as T
import System.Exit (ExitCode, exitSuccess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCase)
import TestUtils qualified

-- NOTE: The tests in this module have low value. They're not really testing
-- any logic in this lib, but rather the built-in callstack functionality.
-- Thus the only real purpose is to demonstrate how to get callstacks. Since
-- the callstack functionality has been a bit of a moving target, this has
-- some value, but it is limited.
--
-- Once the callstack functionality is stable, we can probably remove them.

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
  C.try @_ @(ExceptionWithContext ExitCode) exitFailure >>= \case
    Left (E.ExceptionWithContext ctx _) ->
      TestUtils.assertContainsMinLines 6 expected (T.pack $ Ctx.displayExceptionContext ctx)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "HasCallStack backtrace:",
        "  throwM, called at",
        "  exitWith, called at",
        "  exitFailure"
      ]

throwsExitSuccess :: TestTree
throwsExitSuccess =
  testCase "Calls exitSuccess" $
    C.try @_ @(ExceptionWithContext ExitCode) exitSuccess >>= \case
      Left (E.ExceptionWithContext ctx _) ->
        TestUtils.assertContainsMinLines 3 expected (T.pack $ Ctx.displayExceptionContext ctx)
      Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "HasCallStack backtrace:",
        "  throwIO, called at"
      ]

catchTests :: (HasCallStack) => TestTree
catchTests =
  testGroup
    "Catching"
    [ catchesContext,
      catchesOriginal
    ]

catchesContext :: (HasCallStack) => TestTree
catchesContext = testCase "catches exception with context" $ do
  C.try @_ @(ExceptionWithContext Ex) (C.throwM MkEx) >>= \case
    Left (E.ExceptionWithContext ctx _) ->
      TestUtils.assertContainsMinLines 6 expected (T.pack $ Ctx.displayExceptionContext ctx)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "HasCallStack backtrace:",
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

#else

{-# OPTIONS_GHC -Wno-unused-imports #-}

-- Including these libs so we don't trip -Wunused-packages.
-- Remove once we drop support for base < 4.20.

module Unit.Control.Exception.Utils (tests) where

import Control.Exception.Utils ()
import Control.Monad.Catch ()
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Control.Exception.Utils" []

#endif
