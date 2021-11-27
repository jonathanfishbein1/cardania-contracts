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
{-# LANGUAGE NoImplicitPrelude     #-}

module Script
  ( 
    tokenSaledSerialised,
    tokenSaleSBS,
  )
where

import qualified Cardano.Api
import qualified Cardano.Api.Shelley
import qualified Codec.Serialise
import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Plutus.V1.Ledger.Ada


import qualified Plutus.V1.Ledger.Address
import qualified Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Scripts
import qualified Plutus.V1.Ledger.Api
import qualified PlutusTx
import qualified PlutusTx.Prelude
import qualified Ledger.Typed.Scripts 
import qualified Prelude

data TokenSaleParams = TokenSaleParams
  { tsSellerAddress :: !Plutus.V1.Ledger.Address.Address,
    tsCost :: !PlutusTx.Prelude.Integer
  }

PlutusTx.makeLift ''TokenSaleParams

data TokenSale

instance Ledger.Typed.Scripts.ValidatorTypes TokenSale where
    type instance RedeemerType TokenSale = ()
    type instance DatumType TokenSale = ()


{-# INLINEABLE mkTokenSaleValidator #-}
mkTokenSaleValidator :: TokenSaleParams -> () -> () -> Plutus.V1.Ledger.Contexts.ScriptContext -> PlutusTx.Prelude.Bool
mkTokenSaleValidator ts () () context = PlutusTx.Prelude.True
  -- where

  --   info :: Plutus.V1.Ledger.Contexts.TxInfo
  --   info = Plutus.V1.Ledger.Contexts.scriptContextTxInfo context

  --   currentTxOutputs :: [Plutus.V1.Ledger.Contexts.TxOut]
  --   currentTxOutputs = Plutus.V1.Ledger.Contexts.txInfoOutputs info

  --   tokenCost :: PlutusTx.Prelude.Integer
  --   tokenCost = tsCost ts

  --   sellerAddress :: Plutus.V1.Ledger.Address.Address
  --   sellerAddress = tsSellerAddress ts

  --   contextCostCheck :: [Plutus.V1.Ledger.Contexts.TxOut] -> PlutusTx.Prelude.Bool
  --   contextCostCheck [] = PlutusTx.Prelude.traceIfFalse "Incorrect Amount Of ADA Sent To Script Address" PlutusTx.Prelude.False
  --   contextCostCheck (x : xs)
  --     | (Plutus.V1.Ledger.Contexts.txOutAddress x PlutusTx.Prelude.== sellerAddress) PlutusTx.Prelude.&& (Plutus.V1.Ledger.Contexts.txOutValue x PlutusTx.Prelude.== Plutus.V1.Ledger.Ada.lovelaceValueOf tokenCost) = PlutusTx.Prelude.True
  --     | PlutusTx.Prelude.otherwise = contextCostCheck xs



typedValidator :: TokenSaleParams -> Ledger.Typed.Scripts.TypedValidator TokenSale
typedValidator ts = Ledger.Typed.Scripts.mkTypedValidator @TokenSale
    ($$(PlutusTx.compile [||mkTokenSaleValidator||])  `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| Ledger.Typed.Scripts.wrapValidator @() @() ||])
    where
        wrap = Ledger.Typed.Scripts.wrapValidator @() @()


validator :: TokenSaleParams ->  Plutus.V1.Ledger.Scripts.Validator
validator = Ledger.Typed.Scripts.validatorScript PlutusTx.Prelude.. typedValidator


tokenSaleScript :: Plutus.V1.Ledger.Scripts.Script
tokenSaleScript = Plutus.V1.Ledger.Scripts.unValidatorScript (validator ts)
  where
    ts =
      TokenSaleParams
        { tsSellerAddress = Plutus.V1.Ledger.Address.pubKeyHashAddress "eefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd52",
          tsCost = 10000000
        }

tokenSaleSBS :: Data.ByteString.Short.ShortByteString
tokenSaleSBS = Data.ByteString.Short.toShort PlutusTx.Prelude.. Data.ByteString.Lazy.toStrict PlutusTx.Prelude.$ Codec.Serialise.serialise tokenSaleScript

tokenSaledSerialised :: Cardano.Api.PlutusScript Cardano.Api.PlutusScriptV1
tokenSaledSerialised = Cardano.Api.Shelley.PlutusScriptSerialised tokenSaleSBS
