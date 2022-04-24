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

import           Control.Exception                            (try)
import           Control.Lens
import           Control.Monad                                hiding (fmap)
import           Control.Monad.Freer.Extras                   as Extras
import           Data.Default                                 (Default (..))
import           Data.IORef
import qualified Data.Map                                     as Map
import           Data.Monoid                                  (Last (..))
import           Ledger                                       hiding (singleton)
import           Ledger.Value                                 
import           Ledger.Ada                                   as Ada
import           Plutus.Contract.Test
import           Plutus.Contract.Test.Coverage
import           Plutus.Trace.Emulator                        as Emulator
import qualified PlutusTx.Prelude                             as Plutus
import           System.Exit                                  (ExitCode (..))
import           Test.Tasty
import qualified Test.Tasty.HUnit                             as HUnit

import           Script


tests :: TestTree
tests = checkPredicateOptions
    myOptions
    "token is bought successful"
    myPredicate
    myTrace

myOptions :: CheckOptions
myOptions = defaultCheckOptions & emulatorConfig .~ emCfg

myPredicate :: TracePredicate
myPredicate =
    walletFundsChange w1 (Ada.lovelaceValueOf   10_000_000  <> assetClassValue token (-1)) .&&.
    walletFundsChange w2 (Ada.lovelaceValueOf (-10_000_000) <> assetClassValue token   1)
   

runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left initialDistribution) def 

currSym :: CurrencySymbol
currSym = "641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e"

tokNam :: TokenName 
tokNam = "CardaniaFounderWhite"

token :: AssetClass 
token = AssetClass (currSym, tokNam)

initialDistribution :: InitialDistribution 
initialDistribution = 
    Map.fromList [
        (w1, v1),
        (w2, v2)
    ]
    where
        v1 :: Value 
        v1 = Ada.lovelaceValueOf 100_000_000 <>
            singleton currSym tokNam 1
        
        v2 :: Value 
        v2 = Ada.lovelaceValueOf 100_000_000 

tokenSaleParam :: TokenSaleParam
tokenSaleParam =
  TokenSaleParam
    { tokenCost = 10_000_000,
      Script.currencySymbol = "641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e",
      Script.tokenName = "CardaniaFounderWhite",
      sellerPubKeyHash = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash $ knownWallet 1
    }

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet w1 endpoints 
    h2 <- activateContractWallet w2 endpoints 
    callEndpoint @"start" h1 tokenSaleParam
    void $ waitNSlots 1
    callEndpoint @"buy" h2 tokenSaleParam
    void $ waitNSlots 1
    callEndpoint @"close" h1 tokenSaleParam
    void $ waitNSlots 1
    void $ logInfo @String "Trace finished"

