{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  ( main,
  )
where

import qualified Control.Lens
import qualified Control.Monad
import qualified Control.Monad.Freer.Extras
import qualified Data.Default
import qualified Data.Map
import qualified Ledger
import qualified Ledger.Ada
import qualified Ledger.Value
import qualified Plutus.Contract.Test
import qualified Plutus.Trace.Emulator
import qualified PlutusTx.Prelude
import qualified Script
import qualified Test.Tasty
import qualified Prelude

main :: Prelude.IO ()
main = Test.Tasty.defaultMain tests

tests :: Test.Tasty.TestTree
tests =
  Test.Tasty.testGroup
    "RAD sale on chains test"
    [ testBuy,
      testsClose
    ]

testBuy :: Test.Tasty.TestTree
testBuy =
  Plutus.Contract.Test.checkPredicateOptions
    options
    "token is bought successful"
    buyPredicate
    buyTrace

options :: Plutus.Contract.Test.CheckOptions
options =
  Plutus.Contract.Test.defaultCheckOptions
    Control.Lens.& Plutus.Contract.Test.emulatorConfig Control.Lens..~ buyEmulatorConfig

buyPredicate :: Plutus.Contract.Test.TracePredicate
buyPredicate =
  Plutus.Contract.Test.walletFundsChange
    Plutus.Contract.Test.w1
    (Ledger.Ada.lovelaceValueOf 10_000_000 Prelude.<> Ledger.Value.assetClassValue token (-1))
    Plutus.Contract.Test..&&. Plutus.Contract.Test.walletFundsChange
      Plutus.Contract.Test.w2
      (Ledger.Ada.lovelaceValueOf (-10_000_000) Prelude.<> Ledger.Value.assetClassValue token 1)

runBuyTrace :: Prelude.IO ()
runBuyTrace = Plutus.Trace.Emulator.runEmulatorTraceIO' Data.Default.def buyEmulatorConfig buyTrace

buyEmulatorConfig :: Plutus.Trace.Emulator.EmulatorConfig
buyEmulatorConfig = Plutus.Trace.Emulator.EmulatorConfig (Prelude.Left initialDistribution) Data.Default.def

currencySymbol :: Ledger.Value.CurrencySymbol
currencySymbol = "641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e"

tokenName :: Ledger.Value.TokenName
tokenName = "CardaniaFounderWhite"

token :: Ledger.Value.AssetClass
token = Ledger.Value.AssetClass (currencySymbol, tokenName)

initialDistribution :: Plutus.Contract.Test.InitialDistribution
initialDistribution =
  Data.Map.fromList
    [ (Plutus.Contract.Test.w1, v1),
      (Plutus.Contract.Test.w2, v2)
    ]
  where
    v1 :: Ledger.Value.Value
    v1 =
      Ledger.Ada.lovelaceValueOf 100_000_000
        Prelude.<> Ledger.Value.singleton currencySymbol tokenName 4

    v2 :: Ledger.Value.Value
    v2 = Ledger.Ada.lovelaceValueOf 100_000_000

tokenSaleParam :: Script.TokenSaleParam
tokenSaleParam =
  Script.TokenSaleParam
    { Script.tokenCost = 10_000_000,
      Script.currencySymbol = "641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e",
      Script.tokenName = "CardaniaFounderWhite",
      Script.sellerPubKeyHash = Ledger.unPaymentPubKeyHash Prelude.$ Plutus.Contract.Test.mockWalletPaymentPubKeyHash Prelude.$ Plutus.Contract.Test.knownWallet 1
    }

buyTrace :: Plutus.Trace.Emulator.EmulatorTrace ()
buyTrace = do
  h1 <- Plutus.Trace.Emulator.activateContractWallet Plutus.Contract.Test.w1 Script.endpoints
  h2 <- Plutus.Trace.Emulator.activateContractWallet Plutus.Contract.Test.w2 Script.endpoints
  Plutus.Trace.Emulator.callEndpoint @"start" h1 tokenSaleParam
  Control.Monad.void Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
  Plutus.Trace.Emulator.callEndpoint @"start" h1 tokenSaleParam
  Control.Monad.void Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
  Plutus.Trace.Emulator.callEndpoint @"buy" h2 tokenSaleParam
  Control.Monad.void Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
  -- Plutus.Trace.Emulator.callEndpoint @"close" h1 tokenSaleParam
  Control.Monad.void Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
  Control.Monad.void Prelude.$ Control.Monad.Freer.Extras.logInfo @Prelude.String "Trace finished"

initialDistributionClose :: Plutus.Contract.Test.InitialDistribution
initialDistributionClose =
  Data.Map.fromList
    [ (Plutus.Contract.Test.w1, v1)
    ]
  where
    v1 :: Ledger.Value.Value
    v1 =
      Ledger.Ada.lovelaceValueOf 100_000_000
        PlutusTx.Prelude.<> Ledger.Value.singleton currencySymbol tokenName 1

closeTrace :: Plutus.Trace.Emulator.EmulatorTrace ()
closeTrace = do
  h1 <- Plutus.Trace.Emulator.activateContractWallet Plutus.Contract.Test.w1 Script.endpoints
  Plutus.Trace.Emulator.callEndpoint @"start" h1 tokenSaleParam
  Control.Monad.void PlutusTx.Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
  Plutus.Trace.Emulator.callEndpoint @"close" h1 tokenSaleParam
  Control.Monad.void PlutusTx.Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
  Control.Monad.void PlutusTx.Prelude.$ Control.Monad.Freer.Extras.logInfo @Prelude.String "Trace finished"

testsClose :: Test.Tasty.TestTree
testsClose =
  Plutus.Contract.Test.checkPredicateOptions
    options
    "The contract is open but nobody buy the token"
    closePredicate
    closeTrace

closePredicate :: Plutus.Contract.Test.TracePredicate
closePredicate =
  Plutus.Contract.Test.walletFundsChange Plutus.Contract.Test.w1 (Ledger.Ada.lovelaceValueOf 0 PlutusTx.Prelude.<> Ledger.Value.assetClassValue token 0)

closeEmulatorConfig :: Plutus.Trace.Emulator.EmulatorConfig
closeEmulatorConfig = Plutus.Trace.Emulator.EmulatorConfig (Prelude.Left initialDistributionClose) Data.Default.def

runCloseTrace :: Prelude.IO ()
runCloseTrace = Plutus.Trace.Emulator.runEmulatorTraceIO' Data.Default.def closeEmulatorConfig closeTrace
