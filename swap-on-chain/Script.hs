{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Script where

import qualified PlutusTx
import qualified PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Address
import qualified Plutus.V1.Ledger.Contexts
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

data TokenSaleParams = TokenSaleParams
    { tsSellerAddress  :: !Plutus.V1.Ledger.Address.Address
    , tsCost           :: !PlutusTx.Prelude.Integer
    }
PlutusTx.makeLift ''TokenSaleParams

{-# INLINABLE mkValidator #-}
mkValidator :: TokenSaleParams -> PlutusTx.BuiltinData ->  PlutusTx.BuiltinData -> Plutus.V1.Ledger.Contexts.ScriptContext -> ()
mkValidator ts _ _ context = PlutusTx.Prelude.traceError "BURNT!"
    where

      info :: Plutus.V1.Ledger.Contexts.TxInfo
      info = Plutus.V1.Ledger.Contexts.scriptContextTxInfo context

      currentTxOutputs :: [Plutus.V1.Ledger.Contexts.TxOut]
      currentTxOutputs = Plutus.V1.Ledger.Contexts.txInfoOutputs info

      tokenCost :: PlutusTx.Prelude.Integer
      tokenCost = tsCost ts

      sellerAddress :: Plutus.V1.Ledger.Address.Address
      sellerAddress = tsSellerAddress ts