{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RadSaleContracts
  ( RadSaleContracts (..),
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
import qualified Plutus.V1.Ledger.Api
import qualified Prettyprinter
import qualified Schema
import qualified Script
import qualified Prelude

data RadSaleContracts
  = Start Script.TokenSaleParam
  | Buy Script.TokenSaleParam
  | Close Script.TokenSaleParam
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic)
  deriving anyclass (Data.Aeson.FromJSON, Data.Aeson.ToJSON, Data.OpenApi.ToSchema)

instance Prettyprinter.Pretty RadSaleContracts where
  pretty = Prettyprinter.viaShow

instance Plutus.PAB.Run.PSGenerator.HasPSTypes RadSaleContracts where
  psTypes =
    [ Language.PureScript.Bridge.order
        Prelude.. Language.PureScript.Bridge.equal
        Prelude.. Language.PureScript.Bridge.genericShow
        Prelude.. Language.PureScript.Bridge.argonaut
        Prelude.$ Language.PureScript.Bridge.mkSumType @RadSaleContracts
    ]

tokenSaleParam =
  Script.TokenSaleParam
    { Script.tokenCost = 100,
      Script.currencySymbol = Plutus.V1.Ledger.Api.CurrencySymbol "",
      Script.tokenName = Plutus.V1.Ledger.Api.TokenName "",
      Script.sellerPubKeyHash = Ledger.PubKeyHash ""
    }

instance Plutus.PAB.Effects.Contract.Builtin.HasDefinitions RadSaleContracts where
  getDefinitions =
    [ Start tokenSaleParam,
      Buy tokenSaleParam,
      Close tokenSaleParam
    ]
  getContract = getRadSaleContract
  getSchema = getRadSaleContractSchema

getRadSaleContractSchema :: RadSaleContracts -> [Playground.Types.FunctionSchema Schema.FormSchema]
getRadSaleContractSchema contract =
  case contract of
    Start param -> Plutus.PAB.Effects.Contract.Builtin.endpointsToSchemas @Script.SaleSchema
    Buy param -> Plutus.PAB.Effects.Contract.Builtin.endpointsToSchemas @Script.SaleSchema
    Close param -> Plutus.PAB.Effects.Contract.Builtin.endpointsToSchemas @Script.SaleSchema

getRadSaleContract :: RadSaleContracts -> Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin
getRadSaleContract contract =
  case contract of
    Start param -> Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin Prelude.$ Script.start param
    Buy param -> Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin Prelude.$ Script.buy param
    Close param -> Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin Prelude.$ Script.close param
