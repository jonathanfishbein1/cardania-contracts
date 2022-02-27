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
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.String
import qualified Plutus.V1.Ledger.Api
import qualified PlutusTx
import qualified PlutusTx.Builtins.Class
import qualified PlutusTx.Prelude
import qualified Script
import qualified System.Environment
import qualified Prelude

dataToScriptData :: PlutusTx.Data -> Cardano.Api.ScriptData
dataToScriptData (PlutusTx.Constr n xs) = Cardano.Api.ScriptDataConstructor n PlutusTx.Prelude.$ dataToScriptData PlutusTx.Prelude.<$> xs
dataToScriptData (PlutusTx.Map xs) = Cardano.Api.ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (PlutusTx.List xs) = Cardano.Api.ScriptDataList PlutusTx.Prelude.$ dataToScriptData PlutusTx.Prelude.<$> xs
dataToScriptData (PlutusTx.I n) = Cardano.Api.ScriptDataNumber n
dataToScriptData (PlutusTx.B bs) = Cardano.Api.ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => Prelude.FilePath -> a -> Prelude.IO ()
writeJSON file = Data.ByteString.Lazy.writeFile file Prelude.. Data.Aeson.encode Prelude.. Cardano.Api.scriptDataToJson Cardano.Api.ScriptDataJsonDetailedSchema PlutusTx.Prelude.. dataToScriptData PlutusTx.Prelude.. PlutusTx.toData

writeDatum :: Prelude.IO ()
writeDatum = writeJSON "/home/jonathan/Documents/rad-sale-on-chain/transactions/datum.json" Script.tokenSale

main :: Prelude.IO ()
main =
  do
    result <- Cardano.Api.writeFileTextEnvelope "./transactions/result.plutus" Prelude.Nothing Script.radSaleOnChainSerialised
    dat <- writeDatum
    case result of
      Prelude.Left err -> Prelude.print Prelude.$ Cardano.Api.displayError err
      Prelude.Right () -> Prelude.return ()