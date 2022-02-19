{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Script
  ( tokenSaledSerialised,
    tokenSaleSBS,
  )
where

import qualified Cardano.Api
import qualified Cardano.Api.Shelley
import qualified Codec.Serialise
import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Ledger.Typed.Scripts
import qualified Plutus.V1.Ledger.Ada
import qualified Plutus.V1.Ledger.Address
import qualified Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Scripts
import qualified PlutusTx
import qualified PlutusTx.Lift.Class
import qualified PlutusTx.Prelude
import qualified Prelude

data TokenSale

instance Ledger.Typed.Scripts.ValidatorTypes TokenSale where
  type RedeemerType TokenSale = PlutusTx.Prelude.Integer
  type DatumType TokenSale = PlutusTx.Prelude.Integer

{-# INLINEABLE mkTokenSaleValidator #-}
mkTokenSaleValidator :: PlutusTx.Prelude.Integer -> PlutusTx.Prelude.Integer -> Plutus.V1.Ledger.Contexts.ScriptContext -> PlutusTx.Prelude.Bool
mkTokenSaleValidator _ _ _ = PlutusTx.Prelude.False


typedValidator :: Ledger.Typed.Scripts.TypedValidator TokenSale
typedValidator =
  Ledger.Typed.Scripts.mkTypedValidator @TokenSale
    $$(PlutusTx.compile [||mkTokenSaleValidator||])
    $$(PlutusTx.compile [||Ledger.Typed.Scripts.wrapValidator @PlutusTx.Prelude.Integer @PlutusTx.Prelude.Integer||])

validator :: Plutus.V1.Ledger.Scripts.Validator
validator = Ledger.Typed.Scripts.validatorScript typedValidator

tokenSaleScript :: Plutus.V1.Ledger.Scripts.Script
tokenSaleScript = Plutus.V1.Ledger.Scripts.unValidatorScript validator

tokenSaleSBS :: Data.ByteString.Short.ShortByteString
tokenSaleSBS = Data.ByteString.Short.toShort PlutusTx.Prelude.. Data.ByteString.Lazy.toStrict PlutusTx.Prelude.$ Codec.Serialise.serialise tokenSaleScript

tokenSaledSerialised :: Cardano.Api.PlutusScript Cardano.Api.PlutusScriptV1
tokenSaledSerialised = Cardano.Api.Shelley.PlutusScriptSerialised tokenSaleSBS
