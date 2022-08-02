{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Cardano.Api
import qualified Cardano.Api.Shelley
import qualified Cardano.Crypto.Hash.Class
import qualified Cardano.Ledger.Credential
import qualified Cardano.Ledger.Crypto
import qualified Cardano.Ledger.Hashes
import qualified Cardano.Ledger.Keys
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Data.String
import qualified Data.Text
import qualified Ledger
import qualified Ledger.Address
import qualified Plutus.V1.Ledger.Address
import qualified Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Crypto
import qualified Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.Builtins
import qualified PlutusTx.Builtins.Class
import qualified PlutusTx.Prelude
import qualified Script
import qualified System.Environment
import qualified Prelude
import qualified SerializeToCardanoApi

main :: Prelude.IO ()
main =
    do
    args <- System.Environment.getArgs
    let [addy] = args
        sellerAddress = SerializeToCardanoApi.unsafeReadAddress addy
        sellerPaymentPubKeyHash = SerializeToCardanoApi.unsafePaymentPubKeyHash sellerAddress
    result <-
      Cardano.Api.writeFileTextEnvelope
        "./transactions/result.plutus"
        PlutusTx.Prelude.Nothing
        (Script.mintSerialised sellerPaymentPubKeyHash)
    case result of
      PlutusTx.Prelude.Left err -> Prelude.print PlutusTx.Prelude.$ Cardano.Api.displayError err
      PlutusTx.Prelude.Right () -> Prelude.return ()