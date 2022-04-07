{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DemoContract
  ( DemoContract (..),
  )
where

import qualified Data.Aeson
import qualified Data.OpenApi
import qualified Data.Void
import qualified GHC.Generics
import qualified Language.PureScript.Bridge
import qualified Ledger
import qualified Ledger.Constraints
import qualified Playground.Types
import qualified Plutus.Contract
import qualified Plutus.PAB.Effects.Contract.Builtin
import qualified Plutus.PAB.Run.PSGenerator
import qualified Prettyprinter
import qualified Schema
import qualified Script
import qualified Prelude

data DemoContract = DemoContract
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic)
  deriving anyclass (Data.Aeson.FromJSON, Data.Aeson.ToJSON, Data.OpenApi.ToSchema)

instance Prettyprinter.Pretty DemoContract where
  pretty = Prettyprinter.viaShow

instance Plutus.PAB.Run.PSGenerator.HasPSTypes DemoContract where
  psTypes =
    [ Language.PureScript.Bridge.order
        Prelude.. Language.PureScript.Bridge.equal
        Prelude.. Language.PureScript.Bridge.genericShow
        Prelude.. Language.PureScript.Bridge.argonaut
        Prelude.$ Language.PureScript.Bridge.mkSumType @DemoContract
    ]

instance Plutus.PAB.Effects.Contract.Builtin.HasDefinitions DemoContract where
  getDefinitions =
    [ DemoContract
    ]
  getContract = getDemoContract
  getSchema = getDemoContractSchema

getDemoContractSchema :: DemoContract -> [Playground.Types.FunctionSchema Schema.FormSchema]
getDemoContractSchema = \case
  DemoContract -> Plutus.PAB.Effects.Contract.Builtin.endpointsToSchemas @PayToWalletSchema

getDemoContract :: DemoContract -> Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin
getDemoContract = \case
  DemoContract -> Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin payToWallet

data PayToWalletParams = PayToWalletParams
  { amount :: Ledger.Value,
    pkh :: Ledger.PaymentPubKeyHash,
    skh :: Ledger.StakePubKeyHash
  }
  deriving stock (Prelude.Eq, Prelude.Show, GHC.Generics.Generic)
  deriving anyclass (Data.Aeson.ToJSON, Data.Aeson.FromJSON, Schema.ToSchema)

type PayToWalletSchema = Plutus.Contract.Endpoint "PayToWallet" PayToWalletParams

payToWallet :: Plutus.Contract.Promise () PayToWalletSchema Plutus.Contract.ContractError ()
payToWallet = Plutus.Contract.endpoint @"PayToWallet" Prelude.$ \PayToWalletParams {amount, pkh, skh} -> do
  utx <-
    Plutus.Contract.mkTxConstraints @Data.Void.Void
      Prelude.mempty
      (Ledger.Constraints.mustPayToPubKeyAddress pkh skh amount)
  Plutus.Contract.logInfo @Prelude.String Prelude.$ Prelude.show utx
  Plutus.Contract.yieldUnbalancedTx Prelude.$ Ledger.Constraints.adjustUnbalancedTx utx
