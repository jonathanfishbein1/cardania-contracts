{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Script
  ( radSaleOnChainSerialised,
    TokenSaleParam (TokenSaleParam, tokenCost, currencySymbol, tokenName, sellerPubKeyHash),
  )
where

import qualified Cardano.Api
import qualified Cardano.Api.Shelley
import qualified Codec.Serialise
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Functor
import qualified GHC.Generics
import qualified Ledger
import qualified Ledger.Ada
import qualified Ledger.Address
import qualified Ledger.Constraints
import qualified Ledger.Constraints.TxConstraints
import qualified Ledger.Typed.Scripts
import qualified Playground.Contract
import qualified Plutus.Contract
import qualified Plutus.Contract.Error
import qualified Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Crypto
import qualified Plutus.V1.Ledger.Scripts
import qualified Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.Applicative
import qualified PlutusTx.Base
import qualified PlutusTx.Builtins.Class
import qualified PlutusTx.Builtins.Internal
import qualified PlutusTx.Either
import qualified PlutusTx.Prelude
import qualified Schema
import qualified Text.Printf
import qualified Prelude

data TokenSaleParam = TokenSaleParam
  { tokenCost :: !PlutusTx.Prelude.Integer,
    currencySymbol :: !Plutus.V1.Ledger.Api.CurrencySymbol,
    tokenName :: Plutus.V1.Ledger.Api.TokenName,
    sellerPubKeyHash :: Ledger.PubKeyHash
  }

PlutusTx.unstableMakeIsData ''TokenSaleParam
PlutusTx.makeLift ''TokenSaleParam

minLovelace :: PlutusTx.Prelude.Integer
minLovelace = 2_000_000

data RadSaleOnChain

instance Ledger.Typed.Scripts.ValidatorTypes RadSaleOnChain where
  type RedeemerType RadSaleOnChain = ()
  type DatumType RadSaleOnChain = ()

{-# INLINEABLE isValid #-}
isValid :: PlutusTx.Prelude.Bool -> PlutusTx.Prelude.Bool -> PlutusTx.Prelude.Bool -> PlutusTx.Prelude.Bool -> PlutusTx.Prelude.Bool
isValid
  txToSeller
  txToBuyer
  outputDatum
  scriptOutputValue =
    txToSeller
      PlutusTx.Prelude.&& txToBuyer
      PlutusTx.Prelude.&& outputDatum
      PlutusTx.Prelude.&& scriptOutputValue
      PlutusTx.Prelude.== PlutusTx.Prelude.True

{-# INLINEABLE mkRadSaleOnChainValidator #-}
mkRadSaleOnChainValidator :: TokenSaleParam -> () -> () -> Plutus.V1.Ledger.Contexts.ScriptContext -> PlutusTx.Prelude.Bool
mkRadSaleOnChainValidator tkSaleParam () () context
  | Ledger.txSignedBy info ((sellerPubKeyHash tkSaleParam)) = PlutusTx.Prelude.True
  | PlutusTx.Prelude.True = case ( PlutusTx.Applicative.pure isValid PlutusTx.Applicative.<*> isTxToSeller
                                     PlutusTx.Applicative.<*> isTxToBuyer
                                     PlutusTx.Applicative.<*> correctOutputDatum
                                     PlutusTx.Applicative.<*> correctScriptOutputValue
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
             ( sellerPubKeyHash tkSaleParam
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
                                               PlutusTx.Prelude.&& ( Plutus.V1.Ledger.Contexts.txOutAddress o
                                                                       PlutusTx.Prelude.== Ledger.Address.pubKeyHashAddress tokenBuyerPaymentPubKeyHash PlutusTx.Prelude.Nothing
                                                                   )
                                         )
                                         (Plutus.V1.Ledger.Contexts.txInfoOutputs info)
                                  in case buyerTxOut of
                                       [] ->
                                         PlutusTx.Either.Left "No output to buyer"
                                       [o] ->
                                         PlutusTx.Either.Right PlutusTx.Prelude.True
                                       x : xs ->
                                         PlutusTx.Either.Left "Too many outputs to buyer"
                             )

    correctOutputDatum :: PlutusTx.Either.Either PlutusTx.Builtins.Internal.BuiltinString PlutusTx.Prelude.Bool
    correctOutputDatum = case Plutus.V1.Ledger.Contexts.getContinuingOutputs context of
      [o] ->
        let cOutDat =
              Ledger.txOutDatum o
                PlutusTx.Prelude.>>= ( \datumHash ->
                                         Ledger.findDatum datumHash info
                                           PlutusTx.Prelude.>>= ( \(Plutus.V1.Ledger.Scripts.Datum d) ->
                                                                    (PlutusTx.fromBuiltinData d :: PlutusTx.Prelude.Maybe ())
                                                                      PlutusTx.Prelude.>>= (\datValue -> PlutusTx.Prelude.Just PlutusTx.Prelude.True)
                                                                )
                                     )
         in case cOutDat of
              PlutusTx.Prelude.Nothing -> PlutusTx.Either.Left "Error converting txOutDatum to unit"
              PlutusTx.Prelude.Just val -> PlutusTx.Either.Right val
      _ -> PlutusTx.Either.Left "No continuing output"

    scriptInputValue :: PlutusTx.Prelude.Maybe Plutus.V1.Ledger.Value.Value
    scriptInputValue =
      Plutus.V1.Ledger.Contexts.findOwnInput context
        PlutusTx.Prelude.>>= ( \txInInfo ->
                                 PlutusTx.Prelude.Just
                                   ( Plutus.V1.Ledger.Contexts.txOutValue
                                       (Plutus.V1.Ledger.Contexts.txInInfoResolved txInInfo)
                                   )
                             )

    flippedAssetClassValueOf :: Plutus.V1.Ledger.Value.AssetClass -> Plutus.V1.Ledger.Value.Value -> PlutusTx.Prelude.Integer
    flippedAssetClassValueOf = PlutusTx.Base.flip Plutus.V1.Ledger.Value.assetClassValueOf

    quantityOfNativeTokenBeforeMaybe :: PlutusTx.Prelude.Maybe PlutusTx.Prelude.Integer
    quantityOfNativeTokenBeforeMaybe =
      PlutusTx.Prelude.fmap
        ( flippedAssetClassValueOf
            ( Plutus.V1.Ledger.Value.assetClass
                (currencySymbol tkSaleParam)
                (tokenName tkSaleParam)
            )
        )
        scriptInputValue

    quantityOfNativeTokenAfterMaybe :: PlutusTx.Prelude.Maybe Plutus.V1.Ledger.Value.Value
    quantityOfNativeTokenAfterMaybe =
      PlutusTx.Prelude.fmap
        ( \quantityOfNativeTokenBefore ->
            Plutus.V1.Ledger.Api.singleton
              (currencySymbol tkSaleParam)
              (tokenName tkSaleParam)
              (quantityOfNativeTokenBefore PlutusTx.Prelude.- 1)
        )
        quantityOfNativeTokenBeforeMaybe

    quantityOfNativeTokenAfter :: PlutusTx.Either.Either PlutusTx.Builtins.Internal.BuiltinString Plutus.V1.Ledger.Value.Value
    quantityOfNativeTokenAfter = case quantityOfNativeTokenAfterMaybe of
      PlutusTx.Prelude.Nothing -> PlutusTx.Prelude.Left "Error getting native token from input"
      PlutusTx.Prelude.Just value -> PlutusTx.Prelude.Right value

    correctScriptOutputValue :: PlutusTx.Either.Either PlutusTx.Builtins.Internal.BuiltinString PlutusTx.Prelude.Bool
    correctScriptOutputValue =
      quantityOfNativeTokenAfter
        PlutusTx.Prelude.>>= ( \nativeTokenValue ->
                                 if PlutusTx.Prelude.any
                                   (\output -> Plutus.V1.Ledger.Contexts.txOutValue output PlutusTx.Prelude.== nativeTokenValue PlutusTx.Prelude.<> Ledger.Ada.lovelaceValueOf minLovelace)
                                   (Plutus.V1.Ledger.Contexts.getContinuingOutputs context)
                                   PlutusTx.Prelude.== PlutusTx.Prelude.True
                                   then PlutusTx.Prelude.Right PlutusTx.Prelude.True
                                   else PlutusTx.Prelude.Left "No output to script"
                             )

typedValidator :: TokenSaleParam -> Ledger.Typed.Scripts.TypedValidator RadSaleOnChain
typedValidator p =
  Ledger.Typed.Scripts.mkTypedValidator @RadSaleOnChain
    ($$(PlutusTx.compile [||mkRadSaleOnChainValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Ledger.Typed.Scripts.wrapValidator @() @()

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

start :: Plutus.Contract.Error.AsContractError e => TokenSaleParam -> Plutus.Contract.Contract w s e ()
start tkSaleParam = do
  pkh <- Playground.Contract.ownPaymentPubKeyHash
  let v =
        Plutus.V1.Ledger.Api.singleton (currencySymbol tkSaleParam) (tokenName tkSaleParam) 1
          PlutusTx.Prelude.<> Ledger.Ada.lovelaceValueOf minLovelace
  let tx = Ledger.Constraints.TxConstraints.mustPayToTheScript () v
  ledgerTx <- Plutus.Contract.submitTxConstraints (typedValidator tkSaleParam) tx
  Data.Functor.void PlutusTx.Prelude.$
    Plutus.Contract.awaitTxConfirmed PlutusTx.Prelude.$
      Ledger.getCardanoTxId ledgerTx
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "started auction for token %s" (Prelude.show v)
