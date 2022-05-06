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
import qualified SerializeToCardanoApi
import qualified System.Environment
import qualified Prelude

main :: Prelude.IO ()
main =
  do
    args <- System.Environment.getArgs
    let [tokenCost, currencySymbol, tokenName, addy] = args
    let sellerAddress = SerializeToCardanoApi.unsafeReadAddress addy
    let (Plutus.V1.Ledger.Api.PubKeyCredential pubKeyCredential) =
          (Plutus.V1.Ledger.Api.addressCredential sellerAddress)
    let tokenSaleParam =
          Script.TokenSaleParam
            { Script.tokenCost = Prelude.read tokenCost,
              Script.currencySymbol = Data.String.fromString currencySymbol,
              Script.tokenName = Data.String.fromString tokenName,
              Script.sellerPubKeyHash = pubKeyCredential
            }
    writeRedeemer <- SerializeToCardanoApi.writeJSON "/home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/redeemer.json" Script.Close

    result <-
      Cardano.Api.writeFileTextEnvelope
        "/home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus"
        PlutusTx.Prelude.Nothing
        (Script.radSaleOnChainSerialised tokenSaleParam)
    case result of
      PlutusTx.Prelude.Left err -> Prelude.print PlutusTx.Prelude.$ Cardano.Api.displayError err
      PlutusTx.Prelude.Right () -> Prelude.return ()