{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Script where

import qualified Data.ByteString
import qualified Flat
import qualified Plutus.V1.Ledger.Ada
import qualified Plutus.V1.Ledger.Address
import qualified Plutus.V1.Ledger.Address as Plutus.V1.Ledger
import qualified Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Scripts
import Plutus.V2.Ledger.Api (ScriptContext (ScriptContext))
import qualified PlutusTx
import qualified PlutusTx.Prelude

data TokenSaleParams = TokenSaleParams
  { tsSellerAddress :: !Plutus.V1.Ledger.Address.Address,
    tsCost :: !PlutusTx.Prelude.Integer
  }

PlutusTx.makeLift ''TokenSaleParams

{-# INLINEABLE mkTokenSaleValidator #-}
mkTokenSaleValidator :: TokenSaleParams -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkTokenSaleValidator ts _ _ context = PlutusTx.Prelude.traceError "BURNT!"
  where
    valCtx :: Plutus.V1.Ledger.Contexts.ScriptContext
    valCtx = PlutusTx.unsafeFromBuiltinData context

    info :: Plutus.V1.Ledger.Contexts.TxInfo
    info = Plutus.V1.Ledger.Contexts.scriptContextTxInfo valCtx

    currentTxOutputs :: [Plutus.V1.Ledger.Contexts.TxOut]
    currentTxOutputs = Plutus.V1.Ledger.Contexts.txInfoOutputs info

    tokenCost :: PlutusTx.Prelude.Integer
    tokenCost = tsCost ts

    sellerAddress :: Plutus.V1.Ledger.Address.Address

    sellerAddress = tsSellerAddress ts

    contextCostCheck :: [Plutus.V1.Ledger.Contexts.TxOut] -> PlutusTx.Prelude.Bool
    contextCostCheck [] = PlutusTx.Prelude.traceIfFalse "Incorrect Amount Of ADA Sent To Script Address" PlutusTx.Prelude.False
    contextCostCheck (x : xs)
      | (Plutus.V1.Ledger.Contexts.txOutAddress x PlutusTx.Prelude.== sellerAddress) PlutusTx.Prelude.&& (Plutus.V1.Ledger.Contexts.txOutValue x PlutusTx.Prelude.== Plutus.V1.Ledger.Ada.lovelaceValueOf tokenCost) = PlutusTx.Prelude.True
      | PlutusTx.Prelude.otherwise = contextCostCheck xs

tokenSaleCompiled :: TokenSaleParams -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
tokenSaleCompiled ts = $$(PlutusTx.compile [||mkTokenSaleValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode ts

validator :: Plutus.V1.Ledger.Scripts.Validator
validator = Plutus.V1.Ledger.Scripts.mkValidatorScript (tokenSaleCompiled ts)
  where
    ts =
      TokenSaleParams
        { tsSellerAddress = Plutus.V1.Ledger.pubKeyHashAddress "PUBLIC_KEY_HASH_ADDRESS",
          tsCost = 10
        }

serializedCompiledCode :: TokenSaleParams -> Data.ByteString.ByteString
serializedCompiledCode ts = Flat.flat (tokenSaleCompiled ts)

-- validatorCodeFromFile :: PlutusTx.CompiledCode (() -> () -> Plutus.V1.Ledger.Contexts.ScriptContext -> PlutusTx.Prelude.Bool)
-- validatorCodeFromFile = $$(PlutusTx.loadFromFile "plutus/howtos/myscript.uplc")