{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Cardano.Api
import qualified Data.ByteString.Short
import qualified Data.String
import qualified Plutus.V1.Ledger.Api
import qualified PlutusTx.Builtins.Class
import qualified PlutusTx.Prelude
import qualified Script
import qualified System.Environment
import qualified Prelude

main :: Prelude.IO ()
main =
  do
    case Plutus.V1.Ledger.Api.defaultCostModelParams of
      Prelude.Just m ->
        let (logout, e) =
              Plutus.V1.Ledger.Api.evaluateScriptCounting
                Plutus.V1.Ledger.Api.Verbose
                m
                Script.burnSBS
                [ Plutus.V1.Ledger.Api.toData (0 :: PlutusTx.Prelude.Integer),
                  Plutus.V1.Ledger.Api.toData (0 :: PlutusTx.Prelude.Integer)
                ]
         in do
              Prelude.print ("e" Prelude.++ (Prelude.show e))
              Prelude.print ("Log output" :: Prelude.String) Prelude.>> Prelude.print logout
              Prelude.print ("e" Prelude.++ (Prelude.show e))
              case e of
                Prelude.Left evalErr -> Prelude.print ("Eval Error" :: Prelude.String) Prelude.>> Prelude.print evalErr
                Prelude.Right exbudget -> Prelude.print ("Ex Budget" :: Prelude.String) Prelude.>> Prelude.print exbudget
      Prelude.Nothing -> Prelude.error "defaultCostModelParams failed"
    result <- Cardano.Api.writeFileTextEnvelope "./transactions/result.plutus" Prelude.Nothing Script.burnSerialised
    case result of
      Prelude.Left err -> Prelude.print Prelude.$ Cardano.Api.displayError err
      Prelude.Right () -> Prelude.return ()