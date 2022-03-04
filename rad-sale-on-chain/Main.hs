{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Cardano.Api
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Ledger
import qualified Ledger.Address
import qualified Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.Builtins.Class
import qualified PlutusTx.Prelude
import qualified Script
import qualified System.Environment
import qualified Prelude

-- tokenSaleParam :: Script.TokenSaleParam
-- tokenSaleParam =
--   Script.TokenSaleParam
--     { Script.tokenCost = 10000000,
--       Script.assetClass =
--         Plutus.V1.Ledger.Value.assetClass
--           (Plutus.V1.Ledger.Api.CurrencySymbol "3846efdf61ca59c8d79ec77d4ae71dece719c3db7aadc5c302980f02")
--           (Plutus.V1.Ledger.Api.TokenName "436c617373696342616279426c75653034"),
--       Script.tokenSellerPublicKeyHash =
--         Ledger.Address.PaymentPubKeyHash "eefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd52"
--     }

dataToScriptData :: PlutusTx.Data -> Cardano.Api.ScriptData
dataToScriptData (PlutusTx.Constr n xs) = Cardano.Api.ScriptDataConstructor n PlutusTx.Prelude.$ dataToScriptData PlutusTx.Prelude.<$> xs
dataToScriptData (PlutusTx.Map xs) = Cardano.Api.ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (PlutusTx.List xs) = Cardano.Api.ScriptDataList PlutusTx.Prelude.$ dataToScriptData PlutusTx.Prelude.<$> xs
dataToScriptData (PlutusTx.I n) = Cardano.Api.ScriptDataNumber n
dataToScriptData (PlutusTx.B bs) = Cardano.Api.ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => Prelude.FilePath -> a -> Prelude.IO ()
writeJSON file =
  Data.ByteString.Lazy.writeFile file PlutusTx.Prelude.. Data.Aeson.encode
    PlutusTx.Prelude.. Cardano.Api.scriptDataToJson Cardano.Api.ScriptDataJsonDetailedSchema
    PlutusTx.Prelude.. dataToScriptData
    PlutusTx.Prelude.. PlutusTx.toData

writeDatum :: Prelude.IO ()
writeDatum = writeJSON "/home/jonathan/Documents/rad-sale-on-chain/transactions/datum.json" tokenSellerPublicKeyHash

tokenSellerPublicKeyHash =
  Ledger.Address.PaymentPubKeyHash "eefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd52"

main :: Prelude.IO ()
main =
  do
    args <- System.Environment.getArgs
    dat <- writeDatum
    let [tokenCost, currencySymbol, tokenName] = args
    let tokenSaleParam =
          Script.TokenSaleParam
            { Script.tokenCost = Prelude.read tokenCost,
              Script.currencySymbol = (currencySymbol),
              Script.tokenName = Plutus.V1.Ledger.Api.TokenName (PlutusTx.Builtins.Class.stringToBuiltinByteString tokenName)
            }
    result <- Cardano.Api.writeFileTextEnvelope "./transactions/result.plutus" PlutusTx.Prelude.Nothing (Script.radSaleOnChainSerialised tokenSaleParam)
    case result of
      PlutusTx.Prelude.Left err -> Prelude.print PlutusTx.Prelude.$ Cardano.Api.displayError err
      PlutusTx.Prelude.Right () -> Prelude.return ()