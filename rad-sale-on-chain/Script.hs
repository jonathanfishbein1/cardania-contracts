{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Script
  ( radSaleOnChainSerialised,
    tokenSale,
  )
where

import qualified Cardano.Api
import qualified Cardano.Api.Shelley
import qualified Codec.Serialise
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Ledger
import qualified Ledger.Ada
import qualified Ledger.Typed.Scripts
import qualified Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Crypto
import qualified Plutus.V1.Ledger.Scripts
import qualified Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.Applicative
import qualified PlutusTx.Builtins.Internal
import qualified PlutusTx.Either
import qualified PlutusTx.Prelude

data TokenSale = TokenSale
  { tokenCost :: !PlutusTx.Prelude.Integer,
    assetClass :: !Plutus.V1.Ledger.Value.AssetClass
  }

PlutusTx.unstableMakeIsData ''TokenSale

tokenSale :: TokenSale
tokenSale =
  TokenSale
    { tokenCost = 10000000,
      assetClass =
        Plutus.V1.Ledger.Value.assetClass
          (Plutus.V1.Ledger.Api.CurrencySymbol "f2319ead26195a78dc3eb1fff35b98966617864ee12d1e433f78b68a")
          (Plutus.V1.Ledger.Api.TokenName "434c4153534943424c55453231")
    }

data TokenSaleParams = TokenSaleParams
  { tokenSellerPublicKeyHash :: Plutus.V1.Ledger.Crypto.PubKeyHash,
    tokenBuyerPublicKeyHash :: Plutus.V1.Ledger.Crypto.PubKeyHash
  }

PlutusTx.makeLift ''TokenSaleParams

minLovelace :: PlutusTx.Prelude.Integer
minLovelace = 2000000

tokenSaleParams :: TokenSaleParams
tokenSaleParams =
  TokenSaleParams
    { tokenSellerPublicKeyHash =
        "eefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd52",
      tokenBuyerPublicKeyHash =
        "eefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd52"
    }

data RadSaleOnChain

instance Ledger.Typed.Scripts.ValidatorTypes RadSaleOnChain where
  type RedeemerType RadSaleOnChain = ()
  type DatumType RadSaleOnChain = TokenSale

{-# INLINEABLE isValid #-}
isValid :: PlutusTx.Prelude.Bool -> PlutusTx.Prelude.Bool -> PlutusTx.Prelude.Bool
isValid txToSeller txToBuyer = PlutusTx.Prelude.True

{-# INLINEABLE mkRadSaleOnChainValidator #-}
mkRadSaleOnChainValidator :: TokenSaleParams -> TokenSale -> () -> Plutus.V1.Ledger.Contexts.ScriptContext -> PlutusTx.Prelude.Bool
mkRadSaleOnChainValidator tokenSaleParams datum _ context
  | (PlutusTx.Applicative.pure isValid PlutusTx.Applicative.<*> isTxToSeller PlutusTx.Applicative.<*> isTxToBuyer)
      PlutusTx.Prelude.== PlutusTx.Either.Right PlutusTx.Prelude.True =
    PlutusTx.Prelude.True
  | PlutusTx.Prelude.otherwise = PlutusTx.Prelude.traceIfFalse "Incorrect output to seller" PlutusTx.Prelude.False
  where
    info :: Plutus.V1.Ledger.Contexts.TxInfo
    info = Plutus.V1.Ledger.Contexts.scriptContextTxInfo context

    isTxToSeller :: PlutusTx.Either.Either PlutusTx.Builtins.Internal.BuiltinString PlutusTx.Prelude.Bool
    isTxToSeller =
      if ( Plutus.V1.Ledger.Contexts.valuePaidTo
             info
             ( tokenSellerPublicKeyHash tokenSaleParams
             )
             PlutusTx.Prelude.== Ledger.Ada.lovelaceValueOf (tokenCost datum)
         )
        PlutusTx.Prelude.== PlutusTx.Prelude.True
        then PlutusTx.Either.Right PlutusTx.Prelude.True
        else PlutusTx.Either.Left "Incorrect Tx to seller"

    tokenValue :: Plutus.V1.Ledger.Api.Value
    tokenValue =
      Plutus.V1.Ledger.Api.singleton
        (Plutus.V1.Ledger.Api.CurrencySymbol "f2319ead26195a78dc3eb1fff35b98966617864ee12d1e433f78b68a")
        (Plutus.V1.Ledger.Api.TokenName "434c4153534943424c55453231")
        1

    isTxToBuyer :: PlutusTx.Either.Either PlutusTx.Builtins.Internal.BuiltinString PlutusTx.Prelude.Bool
    isTxToBuyer =
      if ( Plutus.V1.Ledger.Contexts.valuePaidTo
             info
             ( tokenBuyerPublicKeyHash tokenSaleParams
             )
             PlutusTx.Prelude.== tokenValue PlutusTx.Prelude.<> Ledger.Ada.lovelaceValueOf minLovelace
         )
        PlutusTx.Prelude.== PlutusTx.Prelude.True
        then PlutusTx.Either.Right PlutusTx.Prelude.True
        else PlutusTx.Either.Left "Incorrect Tx to buyer"

typedValidator :: TokenSaleParams -> Ledger.Typed.Scripts.TypedValidator RadSaleOnChain
typedValidator tokenSaleParams =
  Ledger.Typed.Scripts.mkTypedValidator @RadSaleOnChain
    ($$(PlutusTx.compile [||mkRadSaleOnChainValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode tokenSaleParams)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Ledger.Typed.Scripts.wrapValidator @TokenSale @()

validator :: Plutus.V1.Ledger.Scripts.Validator
validator = Ledger.Typed.Scripts.validatorScript (typedValidator tokenSaleParams)

radSaleOnChainScript :: Plutus.V1.Ledger.Scripts.Script
radSaleOnChainScript = Plutus.V1.Ledger.Scripts.unValidatorScript validator

radSaleOnChainSBS :: Data.ByteString.Short.ShortByteString
radSaleOnChainSBS =
  Data.ByteString.Short.toShort
    PlutusTx.Prelude.. Data.ByteString.Lazy.toStrict
    PlutusTx.Prelude.$ Codec.Serialise.serialise radSaleOnChainScript

radSaleOnChainSerialised :: Cardano.Api.PlutusScript Cardano.Api.PlutusScriptV1
radSaleOnChainSerialised = Cardano.Api.Shelley.PlutusScriptSerialised radSaleOnChainSBS
