module Main
    ( main
    ) where

import qualified Spec.Test01TokenIsBought
import qualified Spec.Test02NoBuyerTokenReturns
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "RAD sale on chains test"
    [ 
        Spec.Test01TokenIsBought.tests,
        Spec.Test02NoBuyerTokenReturns.tests
    ]
