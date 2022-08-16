module Main
  ( main,
  )
where

import qualified Data.String
import qualified Relude
import qualified SerializeToCardanoApi
import qualified System.Environment

main :: Relude.IO ()
main = do
  [tn'] <- System.Environment.getArgs
  let tn = Data.String.fromString tn'
  Relude.putStrLn Relude.$
    SerializeToCardanoApi.unsafeTokenNameToHex tn