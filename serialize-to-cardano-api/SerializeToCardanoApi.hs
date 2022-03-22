{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SerializeToCardanoApi
  ( dataToScriptData,
    writeJSON,
    writeRedeemer,
  )
where

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
import qualified System.Environment
import qualified Prelude

dataToScriptData :: PlutusTx.Data -> Cardano.Api.Shelley.ScriptData
dataToScriptData (PlutusTx.Constr n xs) = Cardano.Api.Shelley.ScriptDataConstructor n PlutusTx.Prelude.$ dataToScriptData PlutusTx.Prelude.<$> xs
dataToScriptData (PlutusTx.Map xs) = Cardano.Api.Shelley.ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (PlutusTx.List xs) = Cardano.Api.Shelley.ScriptDataList PlutusTx.Prelude.$ dataToScriptData PlutusTx.Prelude.<$> xs
dataToScriptData (PlutusTx.I n) = Cardano.Api.Shelley.ScriptDataNumber n
dataToScriptData (PlutusTx.B bs) = Cardano.Api.Shelley.ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => Prelude.FilePath -> a -> Prelude.IO ()
writeJSON file =
  Data.ByteString.Lazy.writeFile file
    PlutusTx.Prelude.. Data.Aeson.encode
    PlutusTx.Prelude.. Cardano.Api.Shelley.scriptDataToJson Cardano.Api.Shelley.ScriptDataJsonDetailedSchema
    PlutusTx.Prelude.. dataToScriptData
    PlutusTx.Prelude.. PlutusTx.toData

writeRedeemer :: Prelude.IO ()
writeRedeemer = writeJSON "transactions/redeemer.json" ()