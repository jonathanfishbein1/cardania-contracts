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

import qualified Plutus.V1.Ledger.Ada
import qualified Plutus.V1.Ledger.Address
import qualified Plutus.V1.Ledger.Contexts
import Plutus.V1.Ledger.Scripts
import qualified PlutusTx
import qualified PlutusTx.Prelude

data TokenSaleParams = TokenSaleParams
  { tsSellerAddress :: !Plutus.V1.Ledger.Address.Address,
    tsCost :: !PlutusTx.Prelude.Integer
  }

PlutusTx.makeLift ''TokenSaleParams

{-# INLINEABLE mkValidator #-}
mkValidator :: TokenSaleParams -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> Plutus.V1.Ledger.Contexts.ScriptContext -> ()
mkValidator ts _ _ context = PlutusTx.Prelude.traceError "BURNT!"
  where
    info :: Plutus.V1.Ledger.Contexts.TxInfo
    info = Plutus.V1.Ledger.Contexts.scriptContextTxInfo context

    currentTxOutputs :: [Plutus.V1.Ledger.Contexts.TxOut]
    currentTxOutputs = Plutus.V1.Ledger.Contexts.txInfoOutputs info

    tokenCost :: PlutusTx.Prelude.Integer
    tokenCost = tsCost ts

    sellerAddress :: Plutus.V1.Ledger.Address.Address

    sellerAddress = tsSellerAddress ts

    contextCostCheck :: [Plutus.V1.Ledger.Contexts.TxOut] -> PlutusTx.Prelude.Bool
    contextCostCheck [] = PlutusTx.Prelude.traceIfFalse "Incorrect Amount Of ADA Sent To Script Address" PlutusTx.Prelude.$ PlutusTx.Prelude.False
    contextCostCheck (x : xs)
      | ((Plutus.V1.Ledger.Contexts.txOutAddress x) PlutusTx.Prelude.== sellerAddress) PlutusTx.Prelude.&& ((Plutus.V1.Ledger.Contexts.txOutValue x) PlutusTx.Prelude.== (Plutus.V1.Ledger.Ada.lovelaceValueOf tokenCost)) = PlutusTx.Prelude.True
      | PlutusTx.Prelude.otherwise = contextCostCheck xs

-- data Typed
-- instance Plutus.V1.Ledger.Scripts.Script Typed where
--     type instance DatumType    Typed = PlutusTx.BuiltinData
--     type instance RedeemerType Typed = PlutusTx.BuiltinData
