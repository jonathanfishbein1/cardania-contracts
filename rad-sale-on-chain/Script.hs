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
  ( rad-sale-on-chainSerialised,
    rad-sale-on-chainSBS,
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

data rad-sale-on-chain

instance Ledger.Typed.Scripts.ValidatorTypes rad-sale-on-chain where
  type RedeemerType rad-sale-on-chain = ()
  type DatumType rad-sale-on-chain = ()

{-# INLINEABLE mkrad-sale-on-chainValidator #-}
mkrad-sale-on-chainValidator :: () -> () -> Plutus.V1.Ledger.Contexts.ScriptContext -> PlutusTx.Prelude.Bool
mkrad-sale-on-chainValidator _ _ _ = PlutusTx.Prelude.traceIfFalse "rad-sale-on-chaint" PlutusTx.Prelude.False

typedValidator :: Ledger.Typed.Scripts.TypedValidator rad-sale-on-chain
typedValidator =
  Ledger.Typed.Scripts.mkTypedValidator @rad-sale-on-chain
    $$(PlutusTx.compile [||mkrad-sale-on-chainValidator||])
    $$(PlutusTx.compile [||Ledger.Typed.Scripts.wrapValidator @() @()||])

validator :: Plutus.V1.Ledger.Scripts.Validator
validator = Ledger.Typed.Scripts.validatorScript typedValidator

rad-sale-on-chainScript :: Plutus.V1.Ledger.Scripts.Script
rad-sale-on-chainScript = Plutus.V1.Ledger.Scripts.unValidatorScript validator

rad-sale-on-chainSBS :: Data.ByteString.Short.ShortByteString
rad-sale-on-chainSBS = Data.ByteString.Short.toShort PlutusTx.Prelude.. Data.ByteString.Lazy.toStrict PlutusTx.Prelude.$ Codec.Serialise.serialise rad-sale-on-chainScript

rad-sale-on-chainSerialised :: Cardano.Api.PlutusScript Cardano.Api.PlutusScriptV1
rad-sale-on-chainSerialised = Cardano.Api.Shelley.PlutusScriptSerialised rad-sale-on-chainSBS
