module Main
    ( main
    ) where

import qualified Spec.Test01TokenIsBought
import qualified Spec.Test02NoBuyerTokenReturns
import qualified Test.Tasty

main :: Prelude.IO ()
main = Test.Tasty.defaultMain tests

tests :: Test.Tasty.TestTree
tests = Test.Tasty.testGroup "RAD sale on chains test"
    [ 
        Spec.Test01TokenIsBought.tests,
        Spec.Test02NoBuyerTokenReturns.tests
    ]
