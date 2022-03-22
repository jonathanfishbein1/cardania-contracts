{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Cardano.Api
import qualified Cardano.Api.Shelley
import qualified Cardano.Crypto.Hash.Class
import qualified Cardano.Ledger.Credential
import qualified Cardano.Ledger.Crypto
import qualified Cardano.Ledger.Hashes
import qualified Cardano.Ledger.Keys
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Data.String
import qualified Data.Text
import qualified Ledger
import qualified Ledger.Address
import qualified Plutus.V1.Ledger.Address
import qualified Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Crypto
import qualified Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.Builtins
import qualified PlutusTx.Builtins.Class
import qualified PlutusTx.Prelude
import qualified Script
import qualified System.Environment
import qualified Prelude


main :: Prelude.IO ()
main = Prelude.return ()