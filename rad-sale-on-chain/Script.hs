{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Script
  ( radSaleOnChainSerialised,
  )
where

import qualified Cardano.Api
import qualified Cardano.Api.Shelley
import qualified Codec.Serialise
import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Ledger
import qualified Ledger.Ada
import qualified Ledger.Typed.Scripts
import qualified Plutus.V1.Ledger.Ada
import qualified Plutus.V1.Ledger.Address
import qualified Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Crypto
import qualified Plutus.V1.Ledger.Scripts
import qualified PlutusTx
import qualified PlutusTx.Lift.Class
import qualified PlutusTx.Prelude
import qualified Prelude

data RadSaleOnChain

instance Ledger.Typed.Scripts.ValidatorTypes RadSaleOnChain where
  type RedeemerType RadSaleOnChain = ()
  type DatumType RadSaleOnChain = ()

{-# INLINEABLE mkRadSaleOnChainValidator #-}
mkRadSaleOnChainValidator :: () -> () -> Plutus.V1.Ledger.Contexts.ScriptContext -> PlutusTx.Prelude.Bool
mkRadSaleOnChainValidator _ _ context
  | contextCostCheck currentTxOutputs = PlutusTx.Prelude.True
  | PlutusTx.Prelude.otherwise = PlutusTx.Prelude.traceIfFalse "Incorrect Tx To Script Address" PlutusTx.Prelude.False
  where
    info :: Plutus.V1.Ledger.Contexts.TxInfo
    info = Plutus.V1.Ledger.Contexts.scriptContextTxInfo context

    currentTxOutputs :: [Plutus.V1.Ledger.Contexts.TxOut]
    currentTxOutputs = Plutus.V1.Ledger.Contexts.txInfoOutputs info

    tokenCost :: PlutusTx.Prelude.Integer
    tokenCost = 10000000

    sellerAddress :: Plutus.V1.Ledger.Api.Address
    sellerAddress = Plutus.V1.Ledger.Address.pubKeyHashAddress (Plutus.V1.Ledger.Crypto.PubKeyHash "eefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd52")

    contextCostCheck [] = PlutusTx.Prelude.traceIfFalse "Incorrect Amount Of ADA Sent To Script Address" PlutusTx.Prelude.False
    contextCostCheck (x : xs)
      | (Plutus.V1.Ledger.Contexts.txOutAddress x PlutusTx.Prelude.== sellerAddress)
          PlutusTx.Prelude.&& Plutus.V1.Ledger.Contexts.txOutValue x PlutusTx.Prelude.== Ledger.Ada.lovelaceValueOf tokenCost =
        PlutusTx.Prelude.True
      | PlutusTx.Prelude.otherwise = contextCostCheck xs

typedValidator :: Ledger.Typed.Scripts.TypedValidator RadSaleOnChain
typedValidator =
  Ledger.Typed.Scripts.mkTypedValidator @RadSaleOnChain
    $$(PlutusTx.compile [||mkRadSaleOnChainValidator||])
    $$(PlutusTx.compile [||Ledger.Typed.Scripts.wrapValidator @() @()||])

validator :: Plutus.V1.Ledger.Scripts.Validator
validator = Ledger.Typed.Scripts.validatorScript typedValidator

radSaleOnChainScript :: Plutus.V1.Ledger.Scripts.Script
radSaleOnChainScript = Plutus.V1.Ledger.Scripts.unValidatorScript validator

radSaleOnChainSBS :: Data.ByteString.Short.ShortByteString
radSaleOnChainSBS = Data.ByteString.Short.toShort PlutusTx.Prelude.. Data.ByteString.Lazy.toStrict PlutusTx.Prelude.$ Codec.Serialise.serialise radSaleOnChainScript

radSaleOnChainSerialised :: Cardano.Api.PlutusScript Cardano.Api.PlutusScriptV1
radSaleOnChainSerialised = Cardano.Api.Shelley.PlutusScriptSerialised radSaleOnChainSBS
