{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4, 20, 0)

module Unit.Control.Exception.Annotation.Utils (tests) where

import Control.Exception (Exception)
import Control.Exception.Annotation.Utils
  ( ExceptionProxy (MkExceptionProxy),
  )
import Control.Exception.Annotation.Utils qualified as AnnUtils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (HasCallStack, assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "Control.Exception.Annotation.Utils"
    [ testNoMatchNone,
      testMatchOne,
      testMatchMulti,
      testMatchLater,
      testNoMatchMulti
    ]

testNoMatchNone :: (HasCallStack) => TestTree
testNoMatchNone = testCase "Should not match empty expected" $ do
  assertBool "Expected False" (not $ AnnUtils.matchesException [] MkExA)

testMatchOne :: (HasCallStack) => TestTree
testMatchOne = testCase "Should match one" $ do
  assertBool "Expected True" (AnnUtils.matchesException matches MkExA)
  where
    matches = [MkExceptionProxy @ExA]

testMatchMulti :: TestTree
testMatchMulti = testCase "Should match multiple" $ do
  assertBool "Expected True" (AnnUtils.matchesException matches MkExA)
  where
    matches = [MkExceptionProxy @ExA, MkExceptionProxy @ExA]

testMatchLater :: TestTree
testMatchLater = testCase "Should match later" $ do
  assertBool "Expected True" (AnnUtils.matchesException matches MkExB)
  where
    matches = [MkExceptionProxy @ExA, MkExceptionProxy @ExB]

testNoMatchMulti :: TestTree
testNoMatchMulti = testCase "Should not match multiple wrong types" $ do
  assertBool "Expected False" (not $ AnnUtils.matchesException matches MkExA)
  where
    matches = [MkExceptionProxy @ExB, MkExceptionProxy @ExC]

data ExA = MkExA
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data ExB = MkExB
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data ExC = MkExC
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

#else

module Unit.Control.Exception.Annotation.Utils (tests) where

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Control.Exception.Annotation.Utils" []

#endif
