{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4, 20, 0)

module Unit.Control.Exception.Utils (tests) where

import Control.Exception
  ( AsyncException (ThreadKilled),
    Exception,
    ExceptionWithContext,
    SomeException
  )
import Control.Exception qualified as E
import Control.Exception.Context qualified as Ctx
import Control.Exception.Utils (exitFailure)
import Control.Exception.Utils qualified as Utils
import Control.Monad.Catch (Handler(Handler))
import Control.Monad.Catch qualified as C
import Data.Functor (($>))
import Data.Text qualified as T
import System.Exit (ExitCode, exitSuccess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCase, (@=?))
import TestUtils qualified

-- NOTE: Most of the tests in this module merely test the built-in callstack
-- functionality i.e. have little value. Their only real purpose is to
-- demonstrate how to get callstacks. Since the callstack functionality has
-- been a bit of a moving target, this has some value, but it is limited.
--
-- Once the callstack functionality is stable, consider removing them.

tests :: TestTree
tests =
  testGroup
    "Control.Exception.Utils"
    [ throwsTests,
      catchTests,
      handlerTests
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

handlerTests :: TestTree
handlerTests =
  testGroup
    "Handler"
    [ testCatchesSpecific,
      testCatchesSync,
      testCatchesAsync
    ]

data Ex2 = MkEx2
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- Tests that exceptions are caught by the expected handler. Note that we
-- need to differential the handler, hence each handler:
--
--   - Specific Handler
--   - SomeException
--   - Fallback (catches uncaught exceptions so they don't kill the test)
--
-- has a unique string prefix.

testCatchesSpecific :: TestTree
testCatchesSpecific = testCase desc $ do
  result <- Utils.catchesSync (C.throwM MkEx $> "not caught") onSync handlers
  "handlers: MkEx" @=? result
  where
    desc = "catchesSync catches specific exception"

testCatchesSync :: TestTree
testCatchesSync = testCase desc $ do
  result <- Utils.catchesSync (C.throwM MkEx2 $> "not caught") onSync handlers
  "onSync: MkEx2" @=? result
  where
    desc = "catchesSync catches specific exception"

testCatchesAsync :: TestTree
testCatchesAsync = testCase desc $ do
  result <- (Utils.catchesSync (C.throwM ThreadKilled $> "not caught") onSync handlers)
    -- This catch proves that catchesSync did not catch the async exception,
    -- as desired. To actually teset this, the handler here needs to be
    -- different 
    `C.catch` fallback
  "fallback: thread killed" @=? result
  where
    desc = "catchesSync does not catch async exception"

    fallback :: SomeException -> IO String
    fallback = pure . ("fallback: " <> ) . show

-- NOTE: Show over displayException so we do not get callstacks.
-- (GHC 9.10).

onSync :: SomeException -> IO String
onSync = pure . ("onSync: " <>) . show

handlers :: [Handler IO String]
handlers =
  [ Handler $ \(ex :: Ex) -> pure $ "handlers: " <> show ex
  ]

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
