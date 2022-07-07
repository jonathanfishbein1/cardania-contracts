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

module OffChain where

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
import qualified Ledger.Value
import qualified Playground.Contract
import qualified Plutus.Contract
import qualified Plutus.Contract.Typed.Tx
import qualified Plutus.Trace
import qualified Plutus.V1.Ledger.Ada
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
import qualified Script
import qualified Text.Printf
import qualified Wallet.Emulator.Wallet
import qualified Prelude

type SaleSchema =
  Plutus.Contract.Endpoint "start" Script.TokenSaleParam
    Plutus.Contract..\/ Plutus.Contract.Endpoint "buy" Script.TokenSaleParam
    Plutus.Contract..\/ Plutus.Contract.Endpoint "close" Script.TokenSaleParam

start ::
  Script.TokenSaleParam ->
  Plutus.Contract.Contract () SaleSchema Data.Text.Text ()
start tokenSaleParam = do
  let v =
        Plutus.V1.Ledger.Api.singleton
          (Script.currencySymbol tokenSaleParam)
          (Script.tokenName tokenSaleParam)
          1
          PlutusTx.Prelude.<> Ledger.Ada.lovelaceValueOf 6_000_000

  let tx = Ledger.Constraints.TxConstraints.mustPayToTheScript () v
  ledgerTx <-
    Plutus.Contract.submitTxConstraints
      (Script.typedValidator tokenSaleParam)
      tx
  Data.Functor.void PlutusTx.Prelude.$
    Plutus.Contract.awaitTxConfirmed PlutusTx.Prelude.$
      Ledger.getCardanoTxId ledgerTx
  scriptUtxos <-
    Plutus.Contract.utxosAt (Script.scrAddress tokenSaleParam)
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "(Script.scrAddress tokenSaleParam) %s" (Prelude.show (Script.scrAddress tokenSaleParam))
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "scriptUtxos %s" (Prelude.show scriptUtxos)
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "started auction for token %s at end of function" (Prelude.show v)

buy ::
  Script.TokenSaleParam ->
  Plutus.Contract.Contract () SaleSchema Data.Text.Text ()
buy tokenSaleParam = do
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "started buy for token"
  pkh <- Plutus.Contract.ownPaymentPubKeyHash
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "pkh %s" (Prelude.show pkh)
  scriptUtxos <-
    Plutus.Contract.utxosAt (Script.scrAddress tokenSaleParam)
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "(Script.scrAddress tokenSaleParam) %s" (Prelude.show (Script.scrAddress tokenSaleParam))
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "scriptUtxos %s" (Prelude.show scriptUtxos)
  let utxosList = Data.Map.toList scriptUtxos
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "utxosList %s" (Prelude.show utxosList)
  let totalValue =
        PlutusTx.Prelude.foldl
          ( \w (oref, o) ->
              w
                PlutusTx.Prelude.<> Ledger._ciTxOutValue o
          )
          PlutusTx.Prelude.mempty
          utxosList
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf "totalValue %s" (Prelude.show totalValue)
  let totalValueOfAda = Ledger.Value.adaOnlyValue totalValue
      totalValueOfToken = Plutus.V1.Ledger.Value.valueOf totalValue (Script.currencySymbol tokenSaleParam) (Script.tokenName tokenSaleParam)
      valueBackToScript =
        totalValueOfAda
          PlutusTx.Prelude.<> Plutus.V1.Ledger.Api.singleton
            (Script.currencySymbol tokenSaleParam)
            (Script.tokenName tokenSaleParam)
            (totalValueOfToken PlutusTx.Prelude.- 1)
      redeemer =
        Plutus.V1.Ledger.Scripts.Redeemer PlutusTx.Prelude.$
          PlutusTx.toBuiltinData Script.Buy
      lookups =
        Data.Monoid.mconcat
          [ Ledger.Constraints.typedValidatorLookups (Script.typedValidator tokenSaleParam),
            Ledger.Constraints.unspentOutputs scriptUtxos,
            Ledger.Constraints.otherData
              ( Plutus.V1.Ledger.Api.Datum
                  (Plutus.V1.Ledger.Api.toBuiltinData ())
              )
          ]
      v =
        Plutus.V1.Ledger.Api.singleton
          (Script.currencySymbol tokenSaleParam)
          (Script.tokenName tokenSaleParam)
          1
          PlutusTx.Prelude.<> Ledger.Ada.lovelaceValueOf Script.minLovelace
      tx =
        PlutusTx.Prelude.mconcat
          [ Plutus.Contract.Typed.Tx.collectFromScript scriptUtxos Script.Buy,
            Ledger.Constraints.TxConstraints.mustBeSignedBy pkh,
            Ledger.Constraints.TxConstraints.mustPayToTheScript
              ()
              valueBackToScript,
            Ledger.Constraints.TxConstraints.mustPayToPubKey
              (Ledger.Address.PaymentPubKeyHash (Script.sellerPubKeyHash tokenSaleParam))
              (Ledger.Ada.lovelaceValueOf (Script.tokenCost tokenSaleParam)),
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
      (Prelude.show (Script.tokenCost tokenSaleParam))
      (Prelude.show (Script.currencySymbol tokenSaleParam))
      (Prelude.show (Script.tokenName tokenSaleParam))

close ::
  Script.TokenSaleParam ->
  Plutus.Contract.Contract () SaleSchema Data.Text.Text ()
close tokenSaleParam = do
  pkh <- Plutus.Contract.ownPaymentPubKeyHash
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf
      "pkh %s"
      (Prelude.show pkh)
  scriptUtxos <- Plutus.Contract.utxosAt (Script.scrAddress tokenSaleParam)
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf
      "scriptUtxos %s"
      (Prelude.show scriptUtxos)
  let orefs = PlutusTx.Prelude.fst PlutusTx.Prelude.<$> Data.Map.toList scriptUtxos
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf
      "orefs %s"
      (Prelude.show orefs)
  let redeemer =
        Plutus.V1.Ledger.Scripts.Redeemer PlutusTx.Prelude.$
          PlutusTx.toBuiltinData Script.Close
      lookups =
        Data.Monoid.mconcat
          [ Ledger.Constraints.otherScript (Script.validator tokenSaleParam),
            Ledger.Constraints.unspentOutputs scriptUtxos
          ]

      tx =
        PlutusTx.Prelude.mconcat
          [ PlutusTx.Prelude.mconcat
              [Ledger.Constraints.TxConstraints.mustSpendScriptOutput oref redeemer | oref <- orefs],
            Ledger.Constraints.TxConstraints.mustBeSignedBy pkh
          ]
  ledgerTx <-
    Plutus.Contract.submitTxConstraintsWith @Data.Void.Void lookups tx
  Data.Functor.void PlutusTx.Prelude.$
    Plutus.Contract.awaitTxConfirmed PlutusTx.Prelude.$ Ledger.getCardanoTxId ledgerTx
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$
    Text.Printf.printf
      "closed sale for token (%s, %s)"
      (Prelude.show (Script.currencySymbol tokenSaleParam))
      (Prelude.show (Script.tokenName tokenSaleParam))

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
