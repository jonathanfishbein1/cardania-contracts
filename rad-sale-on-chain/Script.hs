{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Script
  ( radSaleOnChainSerialised,
    TokenSaleParam
      ( TokenSaleParam,
        tokenCost,
        currencySymbol,
        tokenName,
        sellerPubKeyHash
      ),
    minLovelace,
    endpoints,
    SaleSchema,
    start,
    buy,
    close,
    isValid,
    isTxToSeller,
    tokenBuyerPaymentPubKeyHashEither,
    isTxToBuyer,
    correctOutputDatum,
    scriptInputValue,
    quantityOfNativeTokenBeforeMaybe,
    quantityOfNativeTokenAfterMaybe,
    quantityOfNativeTokenAfter,
    correctScriptOutputValue,
    mkRadSaleOnChainValidator,
  )
where

import qualified Cardano.Api
import qualified Cardano.Api.Shelley
import qualified Codec.Serialise
import qualified Control.Monad.Freer.Extras
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Monoid
import qualified Data.OpenApi
import qualified Data.Semigroup
import qualified Data.Text
import qualified Data.Void
import qualified GHC.Generics
import qualified Ledger
import qualified Ledger.Ada
import qualified Ledger.Address
import qualified Ledger.Constraints
import qualified Ledger.Constraints.OffChain
import qualified Ledger.Constraints.TxConstraints
import qualified Ledger.Typed.Scripts
import qualified Playground.Contract
import qualified Plutus.Contract
import qualified Plutus.Contract.Typed.Tx
import qualified Plutus.Trace
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
import qualified Wallet.Emulator.Wallet
import qualified Prelude

data SaleAction = Buy | Close
  deriving (Prelude.Show)

PlutusTx.unstableMakeIsData ''SaleAction
PlutusTx.makeLift ''SaleAction

data TokenSaleParam = TokenSaleParam
  { tokenCost :: !PlutusTx.Prelude.Integer,
    currencySymbol :: !Plutus.V1.Ledger.Api.CurrencySymbol,
    tokenName :: !Plutus.V1.Ledger.Api.TokenName,
    sellerPubKeyHash :: !Ledger.PubKeyHash
  }
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Schema.ToSchema)
  deriving anyclass (Data.Aeson.FromJSON, Data.Aeson.ToJSON, Data.OpenApi.ToSchema)

PlutusTx.unstableMakeIsData ''TokenSaleParam
PlutusTx.makeLift ''TokenSaleParam

minLovelace :: PlutusTx.Prelude.Integer
minLovelace = 2_000_000

data RadSaleOnChain

instance Ledger.Typed.Scripts.ValidatorTypes RadSaleOnChain where
  type RedeemerType RadSaleOnChain = SaleAction
  type DatumType RadSaleOnChain = ()

{-# INLINEABLE isValid #-}
isValid ::
  PlutusTx.Prelude.Bool ->
  PlutusTx.Prelude.Bool ->
  PlutusTx.Prelude.Bool ->
  PlutusTx.Prelude.Bool ->
  PlutusTx.Prelude.Bool
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

isTxToSeller ::
  TokenSaleParam ->
  Plutus.V1.Ledger.Contexts.TxInfo ->
  PlutusTx.Either.Either
    PlutusTx.Builtins.Internal.BuiltinString
    PlutusTx.Prelude.Bool
isTxToSeller tkSaleParam info =
  if ( Plutus.V1.Ledger.Contexts.valuePaidTo
         info
         ( sellerPubKeyHash tkSaleParam
         )
         PlutusTx.Prelude.== Ledger.Ada.lovelaceValueOf
           (tokenCost tkSaleParam)
     )
    PlutusTx.Prelude.== PlutusTx.Prelude.True
    then PlutusTx.Either.Right PlutusTx.Prelude.True
    else PlutusTx.Either.Left "Incorrect Tx to seller"

tokenBuyerPaymentPubKeyHashEither ::
  Plutus.V1.Ledger.Contexts.TxInfo ->
  PlutusTx.Either.Either
    PlutusTx.Builtins.Internal.BuiltinString
    Ledger.Address.PaymentPubKeyHash
tokenBuyerPaymentPubKeyHashEither info =
  if PlutusTx.Prelude.length
    (Plutus.V1.Ledger.Contexts.txInfoSignatories info)
    PlutusTx.Prelude.> 0
    then
      PlutusTx.Either.Right PlutusTx.Prelude.$
        Ledger.Address.PaymentPubKeyHash PlutusTx.Prelude.$
          PlutusTx.Prelude.head
            (Plutus.V1.Ledger.Contexts.txInfoSignatories info)
    else PlutusTx.Either.Left "No signer"

isTxToBuyer ::
  TokenSaleParam ->
  Plutus.V1.Ledger.Contexts.TxInfo ->
  PlutusTx.Either.Either
    PlutusTx.Builtins.Internal.BuiltinString
    PlutusTx.Prelude.Bool
isTxToBuyer tkSaleParam info =
  tokenBuyerPaymentPubKeyHashEither info
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
                                                                   PlutusTx.Prelude.== Ledger.Address.pubKeyHashAddress
                                                                     tokenBuyerPaymentPubKeyHash
                                                                     PlutusTx.Prelude.Nothing
                                                               )
                                     )
                                     (Plutus.V1.Ledger.Contexts.txInfoOutputs info)
                              in case buyerTxOut of
                                   [] ->
                                     PlutusTx.Either.Left "No output to buyer"
                                   [tokenOutput] ->
                                     PlutusTx.Either.Right PlutusTx.Prelude.True
                                   x : xs ->
                                     PlutusTx.Either.Left "Too many outputs to buyer"
                         )

correctOutputDatum ::
  Plutus.V1.Ledger.Contexts.ScriptContext ->
  PlutusTx.Either.Either
    PlutusTx.Builtins.Internal.BuiltinString
    PlutusTx.Prelude.Bool
correctOutputDatum context = case Plutus.V1.Ledger.Contexts.getContinuingOutputs context of
  [o] ->
    let cOutDat =
          Ledger.txOutDatum o
            PlutusTx.Prelude.>>= ( \datumHash ->
                                     Ledger.findDatum datumHash (Plutus.V1.Ledger.Contexts.scriptContextTxInfo context)
                                       PlutusTx.Prelude.>>= ( \(Plutus.V1.Ledger.Scripts.Datum d) ->
                                                                (PlutusTx.fromBuiltinData d :: PlutusTx.Prelude.Maybe ())
                                                                  PlutusTx.Prelude.>>= (\datValue -> PlutusTx.Prelude.Just PlutusTx.Prelude.True)
                                                            )
                                 )
     in case cOutDat of
          PlutusTx.Prelude.Nothing ->
            PlutusTx.Either.Left "Error converting txOutDatum to unit"
          PlutusTx.Prelude.Just val ->
            PlutusTx.Either.Right val
  _ -> PlutusTx.Either.Left "No continuing output"

scriptInputValue ::
  Plutus.V1.Ledger.Contexts.ScriptContext ->
  PlutusTx.Prelude.Maybe Plutus.V1.Ledger.Value.Value
scriptInputValue context =
  Plutus.V1.Ledger.Contexts.findOwnInput context
    PlutusTx.Prelude.>>= ( PlutusTx.Prelude.Just
                             PlutusTx.Prelude.. Plutus.V1.Ledger.Contexts.txOutValue
                             PlutusTx.Prelude.. Plutus.V1.Ledger.Contexts.txInInfoResolved
                         )

flippedAssetClassValueOf ::
  Plutus.V1.Ledger.Value.AssetClass ->
  Plutus.V1.Ledger.Value.Value ->
  PlutusTx.Prelude.Integer
flippedAssetClassValueOf =
  PlutusTx.Base.flip
    Plutus.V1.Ledger.Value.assetClassValueOf

quantityOfNativeTokenBeforeMaybe ::
  TokenSaleParam ->
  Plutus.V1.Ledger.Contexts.ScriptContext ->
  PlutusTx.Prelude.Maybe
    PlutusTx.Prelude.Integer
quantityOfNativeTokenBeforeMaybe tkSaleParam context =
  PlutusTx.Prelude.fmap
    ( flippedAssetClassValueOf
        ( Plutus.V1.Ledger.Value.assetClass
            (currencySymbol tkSaleParam)
            (tokenName tkSaleParam)
        )
    )
    (scriptInputValue context)

quantityOfNativeTokenAfterMaybe ::
  TokenSaleParam ->
  Plutus.V1.Ledger.Contexts.ScriptContext ->
  PlutusTx.Prelude.Maybe
    Plutus.V1.Ledger.Value.Value
quantityOfNativeTokenAfterMaybe tkSaleParam context =
  PlutusTx.Prelude.fmap
    ( \quantityOfNativeTokenBefore ->
        Plutus.V1.Ledger.Api.singleton
          (currencySymbol tkSaleParam)
          (tokenName tkSaleParam)
          (quantityOfNativeTokenBefore PlutusTx.Prelude.- 1)
    )
    (quantityOfNativeTokenBeforeMaybe tkSaleParam context)

quantityOfNativeTokenAfter ::
  TokenSaleParam ->
  Plutus.V1.Ledger.Contexts.ScriptContext ->
  PlutusTx.Either.Either
    PlutusTx.Builtins.Internal.BuiltinString
    Plutus.V1.Ledger.Value.Value
quantityOfNativeTokenAfter tkSaleParam context = case quantityOfNativeTokenAfterMaybe tkSaleParam context of
  PlutusTx.Prelude.Nothing ->
    PlutusTx.Prelude.Left "Error getting native token from input"
  PlutusTx.Prelude.Just value ->
    PlutusTx.Prelude.Right value

correctScriptOutputValue ::
  TokenSaleParam ->
  Plutus.V1.Ledger.Contexts.ScriptContext ->
  PlutusTx.Either.Either
    PlutusTx.Builtins.Internal.BuiltinString
    PlutusTx.Prelude.Bool
correctScriptOutputValue tkSaleParam context =
  quantityOfNativeTokenAfter tkSaleParam context
    PlutusTx.Prelude.>>= ( \nativeTokenValue ->
                             if PlutusTx.Prelude.any
                               ( \output ->
                                   let info = Plutus.V1.Ledger.Contexts.scriptContextTxInfo context
                                    in Plutus.V1.Ledger.Contexts.txOutValue output
                                         PlutusTx.Prelude.== Ledger.valueLockedBy info (Ledger.ownHash context)
                               )
                               (Plutus.V1.Ledger.Contexts.getContinuingOutputs context)
                               PlutusTx.Prelude.== PlutusTx.Prelude.True
                               then PlutusTx.Prelude.Right PlutusTx.Prelude.True
                               else PlutusTx.Prelude.Left "No output to script"
                         )

{-# INLINEABLE mkRadSaleOnChainValidator #-}
mkRadSaleOnChainValidator ::
  TokenSaleParam ->
  () ->
  SaleAction ->
  Plutus.V1.Ledger.Contexts.ScriptContext ->
  PlutusTx.Prelude.Bool
mkRadSaleOnChainValidator tkSaleParam () redeemer context =
  let info :: Plutus.V1.Ledger.Contexts.TxInfo
      info = Plutus.V1.Ledger.Contexts.scriptContextTxInfo context
   in case redeemer of
        Close ->
          Ledger.txSignedBy
            info
            (sellerPubKeyHash tkSaleParam)
        Buy ->
          case PlutusTx.Applicative.pure isValid
            PlutusTx.Applicative.<*> isTxToSeller tkSaleParam info
            PlutusTx.Applicative.<*> isTxToBuyer tkSaleParam info
            PlutusTx.Applicative.<*> correctOutputDatum context
            PlutusTx.Applicative.<*> correctScriptOutputValue tkSaleParam context of
            PlutusTx.Either.Right PlutusTx.Prelude.True -> PlutusTx.Prelude.True
            PlutusTx.Either.Left error ->
              PlutusTx.Prelude.traceIfFalse error PlutusTx.Prelude.False

typedValidator ::
  TokenSaleParam ->
  Ledger.Typed.Scripts.TypedValidator RadSaleOnChain
typedValidator p =
  Ledger.Typed.Scripts.mkTypedValidator @RadSaleOnChain
    ( $$(PlutusTx.compile [||mkRadSaleOnChainValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode p
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Ledger.Typed.Scripts.wrapValidator @() @SaleAction

validator :: TokenSaleParam -> Plutus.V1.Ledger.Scripts.Validator
validator =
  Ledger.Typed.Scripts.validatorScript
    PlutusTx.Prelude.. typedValidator

type SaleSchema =
  Plutus.Contract.Endpoint "start" TokenSaleParam
    Plutus.Contract..\/ Plutus.Contract.Endpoint "buy" TokenSaleParam
    Plutus.Contract..\/ Plutus.Contract.Endpoint "close" TokenSaleParam

start ::
  TokenSaleParam ->
  Plutus.Contract.Contract () SaleSchema Data.Text.Text ()
start tokenSaleParam = do
  let v =
        Plutus.V1.Ledger.Api.singleton
          (currencySymbol tokenSaleParam)
          (tokenName tokenSaleParam)
          1
          PlutusTx.Prelude.<> Ledger.Ada.lovelaceValueOf 2_000_000

  let tx = Ledger.Constraints.TxConstraints.mustPayToTheScript () v
  ledgerTx <-
    Plutus.Contract.submitTxConstraints
      (typedValidator tokenSaleParam)
      tx
  Data.Functor.void PlutusTx.Prelude.$
    Plutus.Contract.awaitTxConfirmed PlutusTx.Prelude.$
      Ledger.getCardanoTxId ledgerTx
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "started auction for token %s at end of function" (Prelude.show v)

radSaleHash :: TokenSaleParam -> Ledger.ValidatorHash
radSaleHash tokenSaleParam =
  Ledger.Typed.Scripts.validatorHash
    (typedValidator tokenSaleParam)

scrAddress :: TokenSaleParam -> Ledger.Address
scrAddress tokenSaleParam =
  Ledger.Address.scriptAddress
    (validator tokenSaleParam)

buy ::
  TokenSaleParam ->
  Plutus.Contract.Contract () SaleSchema Data.Text.Text ()
buy tokenSaleParam = do
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "started buy for token"
  pkh <- Plutus.Contract.ownPaymentPubKeyHash
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "pkh %s" (Prelude.show pkh)
  scriptUtxos <-
    -- Data.Map.filter isSuitable
    --   PlutusTx.Prelude.<$>
    Plutus.Contract.utxosAt (scrAddress tokenSaleParam)
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "scriptUtxos %s" (Prelude.show scriptUtxos)
  let utxosList = Data.Map.toList scriptUtxos
      utxoOref = PlutusTx.Prelude.fst (PlutusTx.Prelude.head utxosList)
      remainingUtxosList = PlutusTx.Prelude.tail utxosList
      totalValue =
        PlutusTx.Prelude.foldl
          ( \w (oref, o) ->
              w
                PlutusTx.Prelude.<> Ledger._ciTxOutValue o
          )
          PlutusTx.Prelude.mempty
          remainingUtxosList
      totalValueOfToken = Plutus.V1.Ledger.Value.valueOf totalValue (currencySymbol tokenSaleParam) (tokenName tokenSaleParam)
      valueBackToScript =
        Plutus.V1.Ledger.Api.singleton
          (currencySymbol tokenSaleParam)
          (tokenName tokenSaleParam)
          (totalValueOfToken PlutusTx.Prelude.- 1)
      redeemer =
        Plutus.V1.Ledger.Scripts.Redeemer PlutusTx.Prelude.$
          PlutusTx.toBuiltinData Buy
      lookups =
        Data.Monoid.mconcat
          [ Ledger.Constraints.typedValidatorLookups (typedValidator tokenSaleParam),
            Ledger.Constraints.unspentOutputs scriptUtxos,
            Ledger.Constraints.otherData (Plutus.V1.Ledger.Api.Datum (Plutus.V1.Ledger.Api.toBuiltinData ()))
          ]

      tx =
        PlutusTx.Prelude.mconcat
          [ Plutus.Contract.Typed.Tx.collectFromScript scriptUtxos Buy,
            Ledger.Constraints.TxConstraints.mustBeSignedBy pkh,
            Ledger.Constraints.TxConstraints.mustPayToTheScript
              ()
              (Ledger.Ada.lovelaceValueOf minLovelace PlutusTx.Prelude.<> (totalValue PlutusTx.Prelude.<> valueBackToScript)),
            Ledger.Constraints.TxConstraints.mustPayToPubKey
              (Ledger.Address.PaymentPubKeyHash (sellerPubKeyHash tokenSaleParam))
              (Ledger.Ada.lovelaceValueOf (tokenCost tokenSaleParam)),
            Ledger.Constraints.TxConstraints.mustPayToPubKey
              pkh
              v
          ]

  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "lookups %s" (Prelude.show lookups)

  ledgerTx <-
    Plutus.Contract.submitTxConstraintsWith lookups tx

  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "ledgerTx %s" (Prelude.show ledgerTx)

  Data.Functor.void PlutusTx.Prelude.$
    Plutus.Contract.awaitTxConfirmed PlutusTx.Prelude.$ Ledger.getCardanoTxId ledgerTx
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf
      "made lovelace in auction %s for token (%s, %s)"
      (Prelude.show (tokenCost tokenSaleParam))
      (Prelude.show (currencySymbol tokenSaleParam))
      (Prelude.show (tokenName tokenSaleParam))
  where
    v =
      Plutus.V1.Ledger.Api.singleton
        (currencySymbol tokenSaleParam)
        (tokenName tokenSaleParam)
        1
        PlutusTx.Prelude.<> Ledger.Ada.lovelaceValueOf minLovelace

    isSuitable :: Ledger.ChainIndexTxOut -> PlutusTx.Prelude.Bool
    isSuitable o =
      Plutus.V1.Ledger.Value.valueOf
        (Ledger._ciTxOutValue o)
        (currencySymbol tokenSaleParam)
        (tokenName tokenSaleParam)
        PlutusTx.Prelude.>= 1

close ::
  TokenSaleParam ->
  Plutus.Contract.Contract () SaleSchema Data.Text.Text ()
close tokenSaleParam = do
  pkh <- Plutus.Contract.ownPaymentPubKeyHash
  scriptUtxos <- Plutus.Contract.utxosAt (scrAddress tokenSaleParam)
  let utxosList = Data.Map.toList scriptUtxos
      totalValue =
        Data.Map.foldl'
          ( \w o ->
              w
                PlutusTx.Prelude.<> Ledger._ciTxOutValue o
          )
          PlutusTx.Prelude.mempty
          scriptUtxos
      redeemer =
        Plutus.V1.Ledger.Scripts.Redeemer PlutusTx.Prelude.$
          PlutusTx.toBuiltinData Close
      lookups =
        Data.Monoid.mconcat
          [ Ledger.Constraints.typedValidatorLookups (typedValidator tokenSaleParam),
            Ledger.Constraints.otherScript (validator tokenSaleParam),
            Ledger.Constraints.unspentOutputs scriptUtxos
          ]
      tx =
        PlutusTx.Prelude.mconcat
          [ PlutusTx.Prelude.mconcat
              ( PlutusTx.Prelude.map
                  ( \(oref, o) ->
                      Ledger.Constraints.TxConstraints.mustSpendScriptOutput
                        oref
                        redeemer
                  )
                  utxosList
              ),
            Ledger.Constraints.mustPayToPubKey
              pkh
              totalValue
          ]
  ledgerTx <-
    Plutus.Contract.submitTxConstraintsWith lookups tx
  Data.Functor.void PlutusTx.Prelude.$
    Plutus.Contract.awaitTxConfirmed PlutusTx.Prelude.$ Ledger.getCardanoTxId ledgerTx
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf
      "closed sale for token (%s, %s), withdraw %s"
      (Prelude.show (currencySymbol tokenSaleParam))
      (Prelude.show (tokenName tokenSaleParam))
      (Prelude.show totalValue)

endpoints :: Plutus.Contract.Contract () SaleSchema Data.Text.Text ()
endpoints =
  Plutus.Contract.awaitPromise
    ( start'
        `Plutus.Contract.select` buy'
        `Plutus.Contract.select` close'
    )
    PlutusTx.Prelude.>> endpoints
  where
    start' = Plutus.Contract.endpoint @"start" start
    buy' = Plutus.Contract.endpoint @"buy" buy
    close' = Plutus.Contract.endpoint @"close" close

-- Playground.Contract.mkSchemaDefinitions ''SaleSchema

myToken :: Playground.Contract.KnownCurrency
myToken =
  Playground.Contract.KnownCurrency
    (Plutus.V1.Ledger.Scripts.ValidatorHash "641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e")
    "Token"
    (Plutus.V1.Ledger.Value.TokenName "CardaniaFounderWhite" Playground.Contract.:| [])

--Playground.Contract.mkKnownCurrencies ['myToken]

radSaleOnChainScript :: TokenSaleParam -> Plutus.V1.Ledger.Scripts.Script
radSaleOnChainScript =
  Plutus.V1.Ledger.Scripts.unValidatorScript
    PlutusTx.Prelude.. validator

radSaleOnChainSBS :: TokenSaleParam -> Data.ByteString.Short.ShortByteString
radSaleOnChainSBS p =
  Data.ByteString.Short.toShort
    PlutusTx.Prelude.. Data.ByteString.Lazy.toStrict
    PlutusTx.Prelude.$ Codec.Serialise.serialise (radSaleOnChainScript p)

radSaleOnChainSerialised ::
  TokenSaleParam ->
  Cardano.Api.PlutusScript Cardano.Api.PlutusScriptV1
radSaleOnChainSerialised p = Cardano.Api.Shelley.PlutusScriptSerialised (radSaleOnChainSBS p)
