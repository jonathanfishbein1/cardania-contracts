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

module Main (main) where

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
import qualified Script
import qualified Text.Printf
import qualified Wallet.Emulator.Wallet
import qualified Prelude

myTrace :: Plutus.Trace.EmulatorTrace ()
myTrace = do
  h1 <-
    Plutus.Trace.activateContractWallet
      (Wallet.Emulator.Wallet.knownWallet 1)
      Script.endpoints
  h2 <-
    Plutus.Trace.activateContractWallet
      (Wallet.Emulator.Wallet.knownWallet 2)
      Script.endpoints
  Plutus.Trace.callEndpoint @"start" h1 PlutusTx.Prelude.$
    Script.TokenSaleParam
      { Script.tokenCost = 10000000,
        Script.currencySymbol = "641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e",
        Script.tokenName = "CardaniaFounderWhite",
        Script.sellerPubKeyHash =
          Ledger.Address.unPaymentPubKeyHash PlutusTx.Prelude.$
            Wallet.Emulator.Wallet.mockWalletPaymentPubKeyHash PlutusTx.Prelude.$
              Wallet.Emulator.Wallet.knownWallet 1
      }

test :: Prelude.IO ()
test = Plutus.Trace.runEmulatorTraceIO myTrace

main :: Prelude.IO ()
main = test

-- Data.Functor.void PlutusTx.Prelude.$ waitUntilSlot 20
-- callEndpoint @"grab" h2 ()
-- s <- waitNSlots 2
-- Control.Monad.Freer.Extras.logInfo PlutusTx.Prelude.$ "reached " ++ Prelude.show s