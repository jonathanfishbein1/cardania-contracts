{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Spec.Test01TokenIsBought
    ( tests
    , runMyTrace
    ) where

import qualified Control.Lens
import qualified Control.Monad 
import qualified Control.Monad.Freer.Extras
import qualified Data.Default
import qualified Data.Map
import qualified Ledger
import qualified Ledger.Value                                 
import qualified Ledger.Ada
import qualified Plutus.Contract.Test
import qualified Plutus.Trace.Emulator
import qualified PlutusTx.Prelude
import qualified Test.Tasty
import qualified Script
import qualified Prelude

tests :: Test.Tasty.TestTree
tests = Plutus.Contract.Test.checkPredicateOptions
    myOptions
    "token is bought successful"
    myPredicate
    myTrace

myOptions :: Plutus.Contract.Test.CheckOptions
myOptions = Plutus.Contract.Test.defaultCheckOptions Control.Lens.& Plutus.Contract.Test.emulatorConfig Control.Lens..~ emCfg

myPredicate :: Plutus.Contract.Test.TracePredicate
myPredicate =
    Plutus.Contract.Test.walletFundsChange Plutus.Contract.Test.w1 (Ledger.Ada.lovelaceValueOf   10_000_000  Prelude.<> Ledger.Value.assetClassValue token (-1)) Plutus.Contract.Test..&&.
    Plutus.Contract.Test.walletFundsChange Plutus.Contract.Test.w2 (Ledger.Ada.lovelaceValueOf (-10_000_000) Prelude.<> Ledger.Value.assetClassValue token   1)
   

runMyTrace :: Prelude.IO ()
runMyTrace = Plutus.Trace.Emulator.runEmulatorTraceIO' Data.Default.def emCfg myTrace

emCfg :: Plutus.Trace.Emulator.EmulatorConfig
emCfg = Plutus.Trace.Emulator.EmulatorConfig (Prelude.Left initialDistribution) Data.Default.def 

currSym :: Ledger.Value.CurrencySymbol
currSym = "641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e"

tokNam :: Ledger.Value.TokenName 
tokNam = "CardaniaFounderWhite"

token :: Ledger.Value.AssetClass 
token = Ledger.Value.AssetClass (currSym, tokNam)

initialDistribution :: Plutus.Contract.Test.InitialDistribution 
initialDistribution = 
    Data.Map.fromList [
        (Plutus.Contract.Test.w1, v1),
        (Plutus.Contract.Test.w2, v2)
    ]
    where
        v1 :: Ledger.Value.Value 
        v1 = Ledger.Ada.lovelaceValueOf 100_000_000 Prelude.<>
            Ledger.Value.singleton currSym tokNam 1
        
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

myTrace :: Plutus.Trace.Emulator.EmulatorTrace ()
myTrace = do
    h1 <- Plutus.Trace.Emulator.activateContractWallet Plutus.Contract.Test.w1 Script.endpoints 
    h2 <- Plutus.Trace.Emulator.activateContractWallet Plutus.Contract.Test.w2 Script.endpoints 
    Plutus.Trace.Emulator.callEndpoint @"start" h1 tokenSaleParam
    Control.Monad.void Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
    Plutus.Trace.Emulator.callEndpoint @"buy" h2 tokenSaleParam
    Control.Monad.void Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
    Plutus.Trace.Emulator.callEndpoint @"close" h1 tokenSaleParam
    Control.Monad.void Prelude.$ Plutus.Trace.Emulator.waitNSlots 1
    Control.Monad.void Prelude.$ Control.Monad.Freer.Extras.logInfo @Prelude.String "Trace finished"

