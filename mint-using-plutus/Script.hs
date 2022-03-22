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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Script
  ( endpoints,
    MintParams
      ( MintParams,
        mpAmount,
        mpTokenName
      ),
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
import qualified Data.Text
import qualified Data.Void
import qualified GHC.Generics
import qualified Ledger
import qualified Ledger.Ada
import qualified Ledger.Address
import qualified Ledger.Constraints
import qualified Ledger.Constraints.TxConstraints
import qualified Ledger.Typed.Scripts
import qualified Playground.Contract
import qualified Plutus.Contract
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

{-# INLINEABLE mkPolicy #-}
mkPolicy ::
  Ledger.Address.PaymentPubKeyHash ->
  () ->
  Plutus.V1.Ledger.Contexts.ScriptContext ->
  PlutusTx.Prelude.Bool
mkPolicy paymentPubKeyHash () context =
  Plutus.V1.Ledger.Contexts.txSignedBy (Plutus.V1.Ledger.Contexts.scriptContextTxInfo context) PlutusTx.Prelude.$
    Ledger.Address.unPaymentPubKeyHash paymentPubKeyHash

policy :: Ledger.Address.PaymentPubKeyHash -> Ledger.Typed.Scripts.MintingPolicy
policy pkh =
  Plutus.V1.Ledger.Scripts.mkMintingPolicyScript PlutusTx.Prelude.$
    $$(PlutusTx.compile [||Ledger.Typed.Scripts.wrapMintingPolicy PlutusTx.Prelude.. mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode pkh

curSymbol :: Ledger.Address.PaymentPubKeyHash -> Plutus.V1.Ledger.Value.CurrencySymbol
curSymbol = Ledger.scriptCurrencySymbol PlutusTx.Prelude.. policy

data MintParams = MintParams
  { mpTokenName :: !Plutus.V1.Ledger.Value.TokenName,
    mpAmount :: !PlutusTx.Prelude.Integer
  }
  deriving
    ( GHC.Generics.Generic,
      Data.Aeson.ToJSON,
      Data.Aeson.FromJSON,
      Schema.ToSchema
    )

type FreeSchema = Plutus.Contract.Endpoint "mint" MintParams

mint :: MintParams -> Plutus.Contract.Contract w FreeSchema Data.Text.Text ()
mint mp = do
  pkh <- Plutus.Contract.ownPaymentPubKeyHash
  let val = Plutus.V1.Ledger.Api.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
      lookups = Ledger.Constraints.mintingPolicy PlutusTx.Prelude.$ policy pkh
      tx = Ledger.Constraints.mustMintValue val
  ledgerTx <- Plutus.Contract.submitTxConstraintsWith @Data.Void.Void lookups tx
  Data.Functor.void PlutusTx.Prelude.$
    Plutus.Contract.awaitTxConfirmed PlutusTx.Prelude.$
      Ledger.getCardanoTxId ledgerTx
  Plutus.Contract.logInfo @Prelude.String PlutusTx.Prelude.$ Text.Printf.printf "forged %s" (Prelude.show val)

endpoints :: Plutus.Contract.Contract () FreeSchema Data.Text.Text ()
endpoints = mint' PlutusTx.Prelude.>> endpoints
  where
    mint' =
      Plutus.Contract.awaitPromise PlutusTx.Prelude.$
        Plutus.Contract.endpoint @"mint" mint

Playground.Contract.mkSchemaDefinitions ''FreeSchema

-- Playground.Contract.mkKnownCurrencies []