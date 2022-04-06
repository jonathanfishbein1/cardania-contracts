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

mintParam :: Script.MintParams
mintParam =
  Script.MintParams
    { Script.mpTokenName = "CardaniaFounderWhite",
      Script.mpAmount = 100_000_000
    }

emulatorConfig :: Plutus.Trace.Emulator.EmulatorConfig
emulatorConfig =
  Plutus.Trace.EmulatorConfig
    { Plutus.Trace._initialChainState =
        PlutusTx.Prelude.Left PlutusTx.Prelude.$
          Data.Map.fromList
            [ ((Wallet.Emulator.Wallet.knownWallet 1), (Ledger.Ada.lovelaceValueOf 100_000_000))
            ],
      Plutus.Trace._slotConfig = Data.Default.def
    }

myTrace :: Plutus.Trace.EmulatorTrace ()
myTrace = do
  h1 <-
    Plutus.Trace.activateContractWallet
      (Wallet.Emulator.Wallet.knownWallet 1)
      Script.endpoints
  Plutus.Trace.callEndpoint @"mint" h1 PlutusTx.Prelude.$
    mintParam
  s2 <- Plutus.Trace.Emulator.waitNSlots 2
  Control.Monad.Freer.Extras.logInfo PlutusTx.Prelude.$
    "reached " PlutusTx.Prelude.++ Prelude.show 1

test :: Prelude.IO ()
test = Plutus.Trace.runEmulatorTraceIO' Data.Default.def emulatorConfig myTrace

main :: Prelude.IO ()
main = test