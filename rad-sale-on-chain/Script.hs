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
    TokenSaleParam (TokenSaleParam, tokenCost, currencySymbol, tokenName),
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

data TokenSaleParam = TokenSaleParam
  { tokenCost :: !PlutusTx.Prelude.Integer,
    currencySymbol :: !Plutus.V1.Ledger.Api.CurrencySymbol,
    tokenName :: Plutus.V1.Ledger.Api.TokenName
  }

PlutusTx.unstableMakeIsData ''TokenSaleParam
PlutusTx.makeLift ''TokenSaleParam

minLovelace :: PlutusTx.Prelude.Integer
minLovelace = 2000000

data RadSaleOnChain

instance Ledger.Typed.Scripts.ValidatorTypes RadSaleOnChain where
  type RedeemerType RadSaleOnChain = ()
  type DatumType RadSaleOnChain = Ledger.Address.PaymentPubKeyHash

{-# INLINEABLE isValid #-}
isValid :: PlutusTx.Prelude.Bool -> PlutusTx.Prelude.Bool -> PlutusTx.Prelude.Bool
isValid
  txToSeller
  txToBuyer =
    txToSeller
      PlutusTx.Prelude.&& txToBuyer
      PlutusTx.Prelude.== PlutusTx.Prelude.True

{-# INLINEABLE mkRadSaleOnChainValidator #-}
mkRadSaleOnChainValidator :: TokenSaleParam -> Ledger.Address.PaymentPubKeyHash -> () -> Plutus.V1.Ledger.Contexts.ScriptContext -> PlutusTx.Prelude.Bool
mkRadSaleOnChainValidator tkSaleParam tokenSellerPublicKeyHash () context
  | Ledger.txSignedBy info (Ledger.Address.unPaymentPubKeyHash (tokenSellerPublicKeyHash)) = PlutusTx.Prelude.True
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
                 ( tokenSellerPublicKeyHash
                 )
             )
             PlutusTx.Prelude.== Ledger.Ada.lovelaceValueOf (tokenCost tkSaleParam)
         )
        PlutusTx.Prelude.== PlutusTx.Prelude.True
        then PlutusTx.Either.Right PlutusTx.Prelude.True
        else PlutusTx.Either.Left "Incorrect Tx to seller"

    tokenBuyerPaymentPubKeyHashEither :: PlutusTx.Either.Either PlutusTx.Builtins.Internal.BuiltinString Ledger.Address.PaymentPubKeyHash
    tokenBuyerPaymentPubKeyHashEither =
      if PlutusTx.Prelude.length (Plutus.V1.Ledger.Contexts.txInfoSignatories info) PlutusTx.Prelude.> 0
        then
          PlutusTx.Either.Right PlutusTx.Prelude.$
            Ledger.Address.PaymentPubKeyHash PlutusTx.Prelude.$
              PlutusTx.Prelude.head (Plutus.V1.Ledger.Contexts.txInfoSignatories info)
        else PlutusTx.Either.Left "No signer"

    isTxToBuyer :: PlutusTx.Either.Either PlutusTx.Builtins.Internal.BuiltinString PlutusTx.Prelude.Bool
    isTxToBuyer =
      tokenBuyerPaymentPubKeyHashEither
        PlutusTx.Prelude.>>= ( \tokenBuyerPaymentPubKeyHash ->
                                 let buyerTxOut =
                                       PlutusTx.Prelude.filter
                                         ( \o ->
                                             Plutus.V1.Ledger.Value.assetClassValueOf
                                               (Plutus.V1.Ledger.Contexts.txOutValue o)
                                               ( Plutus.V1.Ledger.Value.assetClass
                                                   (currencySymbol tkSaleParam)
                                                   (tokenName tkSaleParam)
                                               )
                                               PlutusTx.Prelude.== 1
                                         )
                                         (Plutus.V1.Ledger.Contexts.txInfoOutputs info)
                                  in case buyerTxOut of
                                       [] ->
                                         PlutusTx.Either.Left "No output to buyer"
                                       [o] ->
                                         if ( Plutus.V1.Ledger.Contexts.txOutAddress o
                                                PlutusTx.Prelude.== Ledger.Address.pubKeyHashAddress tokenBuyerPaymentPubKeyHash PlutusTx.Prelude.Nothing
                                            )
                                           then PlutusTx.Either.Right PlutusTx.Prelude.True
                                           else PlutusTx.Either.Left "Wrong buyer address"
                                       x : xs ->
                                         PlutusTx.Either.Left "Too many outputs to buyer"
                             )

typedValidator :: TokenSaleParam -> Ledger.Typed.Scripts.TypedValidator RadSaleOnChain
typedValidator p =
  Ledger.Typed.Scripts.mkTypedValidator @RadSaleOnChain
    ($$(PlutusTx.compile [||mkRadSaleOnChainValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Ledger.Typed.Scripts.wrapValidator @Ledger.Address.PaymentPubKeyHash @()

validator :: TokenSaleParam -> Plutus.V1.Ledger.Scripts.Validator
validator = Ledger.Typed.Scripts.validatorScript PlutusTx.Prelude.. typedValidator

radSaleOnChainScript :: TokenSaleParam -> Plutus.V1.Ledger.Scripts.Script
radSaleOnChainScript = Plutus.V1.Ledger.Scripts.unValidatorScript PlutusTx.Prelude.. validator

radSaleOnChainSBS :: TokenSaleParam -> Data.ByteString.Short.ShortByteString
radSaleOnChainSBS p =
  Data.ByteString.Short.toShort
    PlutusTx.Prelude.. Data.ByteString.Lazy.toStrict
    PlutusTx.Prelude.$ Codec.Serialise.serialise (radSaleOnChainScript p)

radSaleOnChainSerialised :: TokenSaleParam -> Cardano.Api.PlutusScript Cardano.Api.PlutusScriptV1
radSaleOnChainSerialised p = Cardano.Api.Shelley.PlutusScriptSerialised (radSaleOnChainSBS p)
