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
    runCloseTrace,
    runBuyTrace,
  )
where

import qualified Control.Lens
import qualified Control.Monad
import qualified Control.Monad.Freer.Extras
import qualified Data.Default
import qualified Data.Map
import qualified Data.Monoid
import qualified Ledger
import qualified Ledger.Ada
import qualified Ledger.Value
import qualified OffChain
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
    [ testStart,
      testBuy,
      testsClose,
      testBuyTwo
    ]

initialDistributionStart :: Plutus.Contract.Test.InitialDistribution
initialDistributionStart =
  Data.Map.fromList
    [ (Plutus.Contract.Test.w1, v1)
    ]
  where
    v1 :: Ledger.Value.Value
    v1 =
      Ledger.Ada.lovelaceValueOf 100_000_000
        PlutusTx.Prelude.<> Ledger.Value.singleton currencySymbol tokenName 2

startEmulatorConfig :: Plutus.Trace.Emulator.EmulatorConfig
startEmulatorConfig = Plutus.Trace.Emulator.EmulatorConfig (Prelude.Left initialDistributionStart) Data.Default.def

startOptions :: Plutus.Contract.Test.CheckOptions
startOptions =
  Plutus.Contract.Test.defaultCheckOptions
    Control.Lens.& Plutus.Contract.Test.emulatorConfig Control.Lens..~ startEmulatorConfig

testStart :: Test.Tasty.TestTree
testStart =
  Plutus.Contract.Test.checkPredicateOptions
    startOptions
    "start contract is started successfully"
    startPredicate
    startTrace

startTrace :: Plutus.Trace.Emulator.EmulatorTrace ()
startTrace = do
  h1 <- Plutus.Trace.Emulator.activateContractWallet Plutus.Contract.Test.w1 OffChain.endpoints
  Plutus.Trace.Emulator.callEndpoint @"start" h1 tokenSaleParam
  Control.Monad.void Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
  Control.Monad.void Prelude.$ Control.Monad.Freer.Extras.logInfo @Prelude.String "Trace finished"

startPredicate :: Plutus.Contract.Test.TracePredicate
startPredicate =
  Plutus.Contract.Test.walletFundsChange
    Plutus.Contract.Test.w1
    (Ledger.Ada.lovelaceValueOf (-6_000_000) Prelude.<> Ledger.Value.assetClassValue token (-2))

testBuy :: Test.Tasty.TestTree
testBuy =
  Plutus.Contract.Test.checkPredicateOptions
    buyOptions
    "one token is bought successful"
    buyPredicate
    buyTrace

testBuyTwo :: Test.Tasty.TestTree
testBuyTwo =
  Plutus.Contract.Test.checkPredicateOptions
    buyOptions
    "one token is bought successful with two tokens in the script"
    buyPredicateTwo
    buyTraceTwo

buyOptions :: Plutus.Contract.Test.CheckOptions
buyOptions =
  Plutus.Contract.Test.defaultCheckOptions
    Control.Lens.& Plutus.Contract.Test.emulatorConfig Control.Lens..~ buyEmulatorConfig

buyPredicate :: Plutus.Contract.Test.TracePredicate
buyPredicate =
  Plutus.Contract.Test.walletFundsChange
    Plutus.Contract.Test.w1
    (Ledger.Ada.lovelaceValueOf 4_000_000 Prelude.<> Ledger.Value.assetClassValue token (-2))
    Plutus.Contract.Test..&&. Plutus.Contract.Test.walletFundsChange
      Plutus.Contract.Test.w2
      (Ledger.Ada.lovelaceValueOf (- tokenCost) Prelude.<> Ledger.Value.assetClassValue token 1)

buyPredicateTwo :: Plutus.Contract.Test.TracePredicate
buyPredicateTwo =
  Plutus.Contract.Test.walletFundsChange
    Plutus.Contract.Test.w1
    (Ledger.Ada.lovelaceValueOf tokenCost Prelude.<> Ledger.Value.assetClassValue token (-1))
    Plutus.Contract.Test..&&. Plutus.Contract.Test.walletFundsChange
      Plutus.Contract.Test.w2
      (Ledger.Ada.lovelaceValueOf (- tokenCost) Prelude.<> Ledger.Value.assetClassValue token 1)

runBuyTrace :: Prelude.IO ()
runBuyTrace = Plutus.Trace.Emulator.runEmulatorTraceIO' Data.Default.def buyEmulatorConfig buyTrace

buyEmulatorConfig :: Plutus.Trace.Emulator.EmulatorConfig
buyEmulatorConfig = Plutus.Trace.Emulator.EmulatorConfig (Prelude.Left initialDistributionBuy) Data.Default.def

currencySymbol :: Ledger.Value.CurrencySymbol
currencySymbol = "fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50"

tokenName :: Ledger.Value.TokenName
tokenName = "CLASSICBABYBLUE01"

token :: Ledger.Value.AssetClass
token = Ledger.Value.AssetClass (currencySymbol, tokenName)

tokenCost :: Prelude.Integer
tokenCost = 10_000_000

initialDistributionBuy :: Plutus.Contract.Test.InitialDistribution
initialDistributionBuy =
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
    { Script.tokenCost = tokenCost,
      Script.currencySymbol = currencySymbol,
      Script.tokenName = tokenName,
      Script.sellerPubKeyHash = Ledger.unPaymentPubKeyHash Prelude.$ Plutus.Contract.Test.mockWalletPaymentPubKeyHash Prelude.$ Plutus.Contract.Test.knownWallet 1
    }

buyTrace :: Plutus.Trace.Emulator.EmulatorTrace ()
buyTrace = do
  h1 <- Plutus.Trace.Emulator.activateContractWallet Plutus.Contract.Test.w1 OffChain.endpoints
  h2 <- Plutus.Trace.Emulator.activateContractWallet Plutus.Contract.Test.w2 OffChain.endpoints
  Plutus.Trace.Emulator.callEndpoint @"start" h1 tokenSaleParam
  Control.Monad.void Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
  Plutus.Trace.Emulator.callEndpoint @"buy" h2 tokenSaleParam
  Control.Monad.void Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
  Control.Monad.void Prelude.$ Control.Monad.Freer.Extras.logInfo @Prelude.String "Trace finished"

buyTraceTwo :: Plutus.Trace.Emulator.EmulatorTrace ()
buyTraceTwo = do
  h1 <- Plutus.Trace.Emulator.activateContractWallet Plutus.Contract.Test.w1 OffChain.endpoints
  h2 <- Plutus.Trace.Emulator.activateContractWallet Plutus.Contract.Test.w2 OffChain.endpoints
  Plutus.Trace.Emulator.callEndpoint @"start" h1 tokenSaleParam
  Control.Monad.void Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
  Plutus.Trace.Emulator.callEndpoint @"buy" h2 tokenSaleParam
  Control.Monad.void Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
  Plutus.Trace.Emulator.callEndpoint @"close" h1 tokenSaleParam
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
  h1 <- Plutus.Trace.Emulator.activateContractWallet Plutus.Contract.Test.w1 OffChain.endpoints
  Plutus.Trace.Emulator.callEndpoint @"start" h1 tokenSaleParam
  Control.Monad.void PlutusTx.Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
  Plutus.Trace.Emulator.callEndpoint @"close" h1 tokenSaleParam
  Control.Monad.void PlutusTx.Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
  Control.Monad.void PlutusTx.Prelude.$ Control.Monad.Freer.Extras.logInfo @Prelude.String "Trace finished"

closeOptions :: Plutus.Contract.Test.CheckOptions
closeOptions =
  Plutus.Contract.Test.defaultCheckOptions
    Control.Lens.& Plutus.Contract.Test.emulatorConfig Control.Lens..~ closeEmulatorConfig

testsClose :: Test.Tasty.TestTree
testsClose =
  Plutus.Contract.Test.checkPredicateOptions
    closeOptions
    "The contract is open but nobody buy the token"
    closePredicate
    closeTrace

closePredicate :: Plutus.Contract.Test.TracePredicate
closePredicate =
  Plutus.Contract.Test.walletFundsChange Plutus.Contract.Test.w1 Data.Monoid.mempty

closeEmulatorConfig :: Plutus.Trace.Emulator.EmulatorConfig
closeEmulatorConfig = Plutus.Trace.Emulator.EmulatorConfig (Prelude.Left initialDistributionClose) Data.Default.def

runCloseTrace :: Prelude.IO ()
runCloseTrace = Plutus.Trace.Emulator.runEmulatorTraceIO' Data.Default.def closeEmulatorConfig closeTrace
