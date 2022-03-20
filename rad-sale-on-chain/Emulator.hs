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

import qualified Control.Monad.Freer.Extras
import qualified Data.Default
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Void
import qualified Ledger
import qualified Ledger.Ada
import qualified Ledger.Address
import qualified Ledger.Constraints
import qualified Ledger.Constraints.TxConstraints
import qualified Ledger.Typed.Scripts
import qualified Plutus.Trace
import qualified Plutus.Trace.Emulator
import qualified Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Crypto
import qualified Plutus.V1.Ledger.Scripts
import qualified Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.Prelude
import qualified Script
import qualified Text.Printf
import qualified Wallet.Emulator.Wallet
import qualified Prelude

tokenSaleParam :: Script.TokenSaleParam
tokenSaleParam =
  Script.TokenSaleParam
    { Script.tokenCost = 10_000_000,
      Script.currencySymbol = "641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e",
      Script.tokenName = "CardaniaFounderWhite",
      Script.sellerPubKeyHash =
        Ledger.Address.unPaymentPubKeyHash PlutusTx.Prelude.$
          Wallet.Emulator.Wallet.mockWalletPaymentPubKeyHash PlutusTx.Prelude.$
            Wallet.Emulator.Wallet.knownWallet 1
    }

tokenValue :: Plutus.V1.Ledger.Value.Value
tokenValue =
  Plutus.V1.Ledger.Api.singleton
    "641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e"
    "CardaniaFounderWhite"
    1
    PlutusTx.Prelude.<> Ledger.Ada.lovelaceValueOf 10000000

walletValue :: Plutus.V1.Ledger.Value.Value
walletValue =
  Ledger.Ada.lovelaceValueOf 100_000_000

emulatorConfig :: Plutus.Trace.Emulator.EmulatorConfig
emulatorConfig =
  Plutus.Trace.EmulatorConfig
    { Plutus.Trace._initialChainState =
        PlutusTx.Prelude.Left PlutusTx.Prelude.$
          Data.Map.fromList
            [ ((Wallet.Emulator.Wallet.knownWallet 1), tokenValue),
              ((Wallet.Emulator.Wallet.knownWallet 2), walletValue)
            ],
      Plutus.Trace._slotConfig = Data.Default.def,
      Plutus.Trace._feeConfig = Data.Default.def
    }

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
    tokenSaleParam
  s <- Plutus.Trace.Emulator.waitNSlots 2
  Plutus.Trace.callEndpoint @"buy" h2 PlutusTx.Prelude.$
    tokenSaleParam
  s2 <- Plutus.Trace.Emulator.waitNSlots 2
  Control.Monad.Freer.Extras.logInfo PlutusTx.Prelude.$
    "reached " PlutusTx.Prelude.++ Prelude.show 1

test :: Prelude.IO ()
test = Plutus.Trace.runEmulatorTraceIO' Data.Default.def emulatorConfig myTrace

main :: Prelude.IO ()
main = test

-- Data.Functor.void PlutusTx.Prelude.$ waitUntilSlot 20
-- callEndpoint @"grab" h2 ()
-- s <- waitNSlots 2
-- Control.Monad.Freer.Extras.logInfo PlutusTx.Prelude.$ "reached " ++ Prelude.show s