module Main
  ( main,
  )
where

import qualified Data.String
import qualified SerializeToCardanoApi
import qualified System.Environment

main :: Prelude.IO ()
main = do
  [tn'] <- System.Environment.getArgs
  let tn = Data.String.fromString tn'
  Prelude.putStrLn Prelude.$
    SerializeToCardanoApi.unsafeTokenNameToHex tn