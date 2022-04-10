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
import qualified PlutusTx.Builtins.Class
import qualified Prettyprinter
import qualified Schema
import qualified Script
import qualified Prelude

data TokenSaleParamPAB = TokenSaleParamPAB
  { tokenCost :: !Prelude.Integer,
    currencySymbol :: !Prelude.String,
    tokenName :: !Prelude.String,
    sellerPubKeyHash :: !Prelude.String
  }
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Schema.ToSchema)
  deriving anyclass (Data.Aeson.FromJSON, Data.Aeson.ToJSON, Data.OpenApi.ToSchema)

data RadSaleContracts
  = Start TokenSaleParamPAB
  | Buy TokenSaleParamPAB
  | Close TokenSaleParamPAB
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

tokenSaleParamPAB :: TokenSaleParamPAB
tokenSaleParamPAB =
  TokenSaleParamPAB
    { tokenCost = 100,
      currencySymbol = "",
      tokenName = "",
      sellerPubKeyHash = ""
    }

instance Plutus.PAB.Effects.Contract.Builtin.HasDefinitions RadSaleContracts where
  getDefinitions =
    [ Start tokenSaleParamPAB,
      Buy tokenSaleParamPAB,
      Close tokenSaleParamPAB
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
      let param =
            Script.TokenSaleParam
              { Script.tokenCost = 100,
                Script.currencySymbol =
                  Plutus.V1.Ledger.Api.CurrencySymbol
                    (PlutusTx.Builtins.Class.stringToBuiltinByteString (currencySymbol paramPAB)),
                Script.tokenName =
                  Plutus.V1.Ledger.Api.TokenName
                    ( PlutusTx.Builtins.Class.stringToBuiltinByteString (tokenName paramPAB)
                    ),
                Script.sellerPubKeyHash =
                  Ledger.PubKeyHash
                    (PlutusTx.Builtins.Class.stringToBuiltinByteString (sellerPubKeyHash paramPAB))
              }
       in Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin Prelude.$ Script.start param
    Buy paramPAB ->
      let param =
            Script.TokenSaleParam
              { Script.tokenCost = 100,
                Script.currencySymbol =
                  Plutus.V1.Ledger.Api.CurrencySymbol
                    (PlutusTx.Builtins.Class.stringToBuiltinByteString (currencySymbol paramPAB)),
                Script.tokenName =
                  Plutus.V1.Ledger.Api.TokenName
                    ( PlutusTx.Builtins.Class.stringToBuiltinByteString (tokenName paramPAB)
                    ),
                Script.sellerPubKeyHash =
                  Ledger.PubKeyHash
                    (PlutusTx.Builtins.Class.stringToBuiltinByteString (sellerPubKeyHash paramPAB))
              }
       in Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin Prelude.$ Script.buy param
    Close paramPAB ->
      let param =
            Script.TokenSaleParam
              { Script.tokenCost = 100,
                Script.currencySymbol =
                  Plutus.V1.Ledger.Api.CurrencySymbol
                    (PlutusTx.Builtins.Class.stringToBuiltinByteString (currencySymbol paramPAB)),
                Script.tokenName =
                  Plutus.V1.Ledger.Api.TokenName
                    ( PlutusTx.Builtins.Class.stringToBuiltinByteString (tokenName paramPAB)
                    ),
                Script.sellerPubKeyHash =
                  Ledger.PubKeyHash
                    (PlutusTx.Builtins.Class.stringToBuiltinByteString (sellerPubKeyHash paramPAB))
              }
       in Plutus.PAB.Effects.Contract.Builtin.SomeBuiltin Prelude.$ Script.close param
