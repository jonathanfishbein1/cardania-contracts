module Main where

import qualified Cardano.Api
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified PlutusTx
import qualified PlutusTx.Prelude
import qualified Script
import qualified Prelude

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
writeDatum = writeJSON "/home/jonathan/Documents/rad-sale-on-chain/transactions/datum.json" Script.tokenSale

main :: Prelude.IO ()
main =
  do
    result <- Cardano.Api.writeFileTextEnvelope "./transactions/result.plutus" PlutusTx.Prelude.Nothing Script.radSaleOnChainSerialised
    dat <- writeDatum
    case result of
      PlutusTx.Prelude.Left err -> Prelude.print PlutusTx.Prelude.$ Cardano.Api.displayError err
      PlutusTx.Prelude.Right () -> Prelude.return ()