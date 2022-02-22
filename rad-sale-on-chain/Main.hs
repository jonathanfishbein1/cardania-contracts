{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Cardano.Api
import qualified Data.ByteString.Short
import qualified Data.String
import qualified Plutus.V1.Ledger.Api
import qualified PlutusTx.Builtins.Class
import qualified PlutusTx.Prelude
import qualified Script
import qualified System.Environment
import qualified Prelude

main :: Prelude.IO ()
main =
  do
    result <- Cardano.Api.writeFileTextEnvelope "./transactions/result.plutus" Prelude.Nothing Script.radSaleOnChainSerialised
    case result of
      Prelude.Left err -> Prelude.print Prelude.$ Cardano.Api.displayError err
      Prelude.Right () -> Prelude.return ()