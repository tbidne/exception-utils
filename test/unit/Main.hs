module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Unit.Control.Exception.Annotation.Utils qualified
import Unit.Control.Exception.Utils qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit"
      [ Unit.Control.Exception.Annotation.Utils.tests,
        Unit.Control.Exception.Utils.tests
      ]
