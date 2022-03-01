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
import qualified Ledger.Address
import qualified Ledger.Typed.Scripts
import qualified Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Crypto
import qualified Plutus.V1.Ledger.Scripts
import qualified Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.Applicative
import qualified PlutusTx.Builtins.Class
import qualified PlutusTx.Builtins.Internal
import qualified PlutusTx.Either
import qualified PlutusTx.Prelude
import qualified Prelude

data TokenSale = TokenSale
  { tokenCost :: !PlutusTx.Prelude.Integer,
    assetClass :: !Plutus.V1.Ledger.Value.AssetClass,
    tokenSellerPublicKeyHash :: Ledger.Address.PaymentPubKeyHash
  }

PlutusTx.unstableMakeIsData ''TokenSale

tokenSale :: TokenSale
tokenSale =
  TokenSale
    { tokenCost = 10000000,
      assetClass =
        Plutus.V1.Ledger.Value.assetClass
          (Plutus.V1.Ledger.Api.CurrencySymbol "3846efdf61ca59c8d79ec77d4ae71dece719c3db7aadc5c302980f02")
          (Plutus.V1.Ledger.Api.TokenName "436c617373696342616279426c75653033"),
      tokenSellerPublicKeyHash =
        Ledger.Address.PaymentPubKeyHash "eefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd52"
    }

minLovelace :: PlutusTx.Prelude.Integer
minLovelace = 2000000

data RadSaleOnChain

instance Ledger.Typed.Scripts.ValidatorTypes RadSaleOnChain where
  type RedeemerType RadSaleOnChain = ()
  type DatumType RadSaleOnChain = TokenSale

{-# INLINEABLE isValid #-}
isValid :: PlutusTx.Prelude.Bool -> PlutusTx.Prelude.Bool -> PlutusTx.Prelude.Bool
isValid
  txToSeller
  txToBuyer =
    txToSeller
      PlutusTx.Prelude.&& txToBuyer
      PlutusTx.Prelude.== PlutusTx.Prelude.True

{-# INLINEABLE mkRadSaleOnChainValidator #-}
mkRadSaleOnChainValidator :: TokenSale -> () -> Plutus.V1.Ledger.Contexts.ScriptContext -> PlutusTx.Prelude.Bool
mkRadSaleOnChainValidator datum _ context
  | Ledger.txSignedBy info (Ledger.Address.unPaymentPubKeyHash (tokenSellerPublicKeyHash datum)) = PlutusTx.Prelude.True
  | PlutusTx.Prelude.True = case ( PlutusTx.Applicative.pure isValid PlutusTx.Applicative.<*> isTxToSeller
                                     PlutusTx.Applicative.<*> isTxToBuyer
                                 ) of
    PlutusTx.Either.Right PlutusTx.Prelude.True -> PlutusTx.Prelude.True
    PlutusTx.Either.Left error -> PlutusTx.Prelude.traceIfFalse error PlutusTx.Prelude.False
  where
    info :: Plutus.V1.Ledger.Contexts.TxInfo
    info = Plutus.V1.Ledger.Contexts.scriptContextTxInfo context

    isTxToSeller :: PlutusTx.Either.Either PlutusTx.Builtins.Internal.BuiltinString PlutusTx.Prelude.Bool
    isTxToSeller =
      if ( Plutus.V1.Ledger.Contexts.valuePaidTo
             info
             ( Ledger.Address.unPaymentPubKeyHash
                 ( tokenSellerPublicKeyHash datum
                 )
             )
             PlutusTx.Prelude.== Ledger.Ada.lovelaceValueOf (tokenCost datum)
         )
        PlutusTx.Prelude.== PlutusTx.Prelude.True
        then PlutusTx.Either.Right PlutusTx.Prelude.True
        else PlutusTx.Either.Left "Incorrect Tx to seller"

    (currencySymbol, tokenName) = Plutus.V1.Ledger.Value.unAssetClass (assetClass datum)

    tokenValue :: Plutus.V1.Ledger.Api.Value
    tokenValue =
      Plutus.V1.Ledger.Api.singleton
        (currencySymbol)
        (tokenName)
        1

    tokenBuyerPaymentPubKeyHash :: Ledger.Address.PaymentPubKeyHash
    tokenBuyerPaymentPubKeyHash =
      Ledger.Address.PaymentPubKeyHash PlutusTx.Prelude.$
        PlutusTx.Prelude.head (Plutus.V1.Ledger.Contexts.txInfoSignatories info)

    getsValue :: Ledger.Address.PaymentPubKeyHash -> Plutus.V1.Ledger.Api.Value -> PlutusTx.Prelude.Bool
    getsValue h v =
      let [o] =
            [ o'
              | o' <- Plutus.V1.Ledger.Contexts.txInfoOutputs info,
                Plutus.V1.Ledger.Contexts.txOutValue o' PlutusTx.Prelude.== v
            ]
       in Plutus.V1.Ledger.Contexts.txOutAddress o
            PlutusTx.Prelude.== Ledger.Address.pubKeyHashAddress h PlutusTx.Prelude.Nothing

    isTxToBuyer :: PlutusTx.Either.Either PlutusTx.Builtins.Internal.BuiltinString PlutusTx.Prelude.Bool
    isTxToBuyer =
      if ( getsValue tokenBuyerPaymentPubKeyHash PlutusTx.Prelude.$
             tokenValue PlutusTx.Prelude.<> Ledger.Ada.lovelaceValueOf minLovelace
         )
        PlutusTx.Prelude.== PlutusTx.Prelude.True
        then PlutusTx.Either.Right PlutusTx.Prelude.True
        else PlutusTx.Either.Left "Incorrect Tx to buyer"

typedValidator :: Ledger.Typed.Scripts.TypedValidator RadSaleOnChain
typedValidator =
  Ledger.Typed.Scripts.mkTypedValidator @RadSaleOnChain
    ($$(PlutusTx.compile [||mkRadSaleOnChainValidator||]))
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Ledger.Typed.Scripts.wrapValidator @TokenSale @()

validator :: Plutus.V1.Ledger.Scripts.Validator
validator = Ledger.Typed.Scripts.validatorScript (typedValidator)

radSaleOnChainScript :: Plutus.V1.Ledger.Scripts.Script
radSaleOnChainScript = Plutus.V1.Ledger.Scripts.unValidatorScript validator

radSaleOnChainSBS :: Data.ByteString.Short.ShortByteString
radSaleOnChainSBS =
  Data.ByteString.Short.toShort
    PlutusTx.Prelude.. Data.ByteString.Lazy.toStrict
    PlutusTx.Prelude.$ Codec.Serialise.serialise radSaleOnChainScript

radSaleOnChainSerialised :: Cardano.Api.PlutusScript Cardano.Api.PlutusScriptV1
radSaleOnChainSerialised = Cardano.Api.Shelley.PlutusScriptSerialised radSaleOnChainSBS
