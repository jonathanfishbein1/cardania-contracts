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

import Data.Aeson qualified
import Data.OpenApi qualified
import Data.String qualified
import Data.Void qualified
import GHC.Generics qualified
import Language.PureScript.Bridge qualified
import Ledger qualified
import Ledger.Constraints qualified
import Playground.Types qualified
import Plutus.Contract qualified
import Plutus.PAB.Effects.Contract.Builtin qualified
import Plutus.PAB.Run.PSGenerator qualified
import Plutus.V1.Ledger.Api qualified
import PlutusTx.Builtins.Class qualified
import Prettyprinter qualified
import Schema qualified
import Script qualified
import Prelude qualified

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

tokenSaleParam :: Script.TokenSaleParam
tokenSaleParam =
  Script.TokenSaleParam
    { Script.tokenCost = 100,
      Script.currencySymbol = "",
      Script.tokenName = "",
      Script.sellerPubKeyHash = ""
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
    Start paramPAB ->
      Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin Prelude.$ Script.start paramPAB
    Buy paramPAB ->
      Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin Prelude.$ Script.buy paramPAB
    Close paramPAB ->
      Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin Prelude.$ Script.close paramPAB
