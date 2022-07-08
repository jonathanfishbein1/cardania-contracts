{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

-- This is a Plutus Contract that enables native token sales.
-- The tokens need to have already been minted.
-- The token cost, token currency symbol, token name, and seller
-- public key hash are runtime parameters

-- The general structure of the Contract is as follows
-- Compiler Extensions are enabled
-- Module exports are listed
-- Module imports are listed qualified

module Script
  ( radSaleOnChainSerialised,
    TokenSaleParam
      ( TokenSaleParam,
        tokenCost,
        currencySymbol,
        tokenName,
        sellerPubKeyHash
      ),
    minLovelace,
    isValid,
    isTxToSeller,
    isTxToBuyer,
    correctOutputDatum,
    correctScriptOutputValue,
    mkRadSaleOnChainValidator,
    SaleAction (..),
    validator,
    typedValidator,
    scrAddress,
    radSaleHash,
  )
where

import qualified Cardano.Api
import qualified Cardano.Api.Shelley
import qualified Codec.Serialise
import qualified Control.Monad.Freer.Extras
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Monoid
import qualified Data.OpenApi
import qualified Data.Semigroup
import qualified Data.Text
import qualified Data.Void
import qualified GHC.Generics
import qualified Ledger
import qualified Ledger.Ada
import qualified Ledger.Address
import qualified Ledger.Constraints
import qualified Ledger.Constraints.OffChain
import qualified Ledger.Constraints.TxConstraints
import qualified Ledger.Typed.Scripts
import qualified Ledger.Value
import qualified Playground.Contract
import qualified Plutus.Contract
import qualified Plutus.Contract.Typed.Tx
import qualified Plutus.Trace
import qualified Plutus.V1.Ledger.Ada
import qualified Plutus.V1.Ledger.Address
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
import qualified Text.Printf
import qualified Wallet.Emulator.Wallet
import qualified Prelude

-- In the following a series of Types are declared inlcuding the
-- datum as unit, the redeemer as SaleAction TokenSaleParam as the runtime
-- parameters to the contract and additionally a minLovelace constant.

-- SaleAction is the Redeemer type.  It has two data constructors.
-- Buy for purchasing the token for sale and Close for closing the sale.

data SaleAction = Buy | Close
  deriving (Prelude.Show)

PlutusTx.unstableMakeIsData ''SaleAction
PlutusTx.makeLift ''SaleAction

-- This is a paramaterized Plutus Contract meaning that a variety of contracts
-- can be instantiated that vary based on the arguments passed in at runtime.
-- The parameters are
--   - the token cost for the token being sold
--   - the currency symbol of the token being sold
--   - the token name of the token being sold
--   - the public key hash of the seller selling the token

data TokenSaleParam = TokenSaleParam
  { tokenCost :: !PlutusTx.Prelude.Integer,
    currencySymbol :: !Plutus.V1.Ledger.Api.CurrencySymbol,
    tokenName :: !Plutus.V1.Ledger.Api.TokenName,
    sellerPubKeyHash :: !Ledger.PubKeyHash
  }
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Schema.ToSchema)
  deriving anyclass (Data.Aeson.FromJSON, Data.Aeson.ToJSON, Data.OpenApi.ToSchema)

PlutusTx.unstableMakeIsData ''TokenSaleParam
PlutusTx.makeLift ''TokenSaleParam

minLovelace :: PlutusTx.Prelude.Integer
minLovelace = 2_000_000

data RadSaleOnChain

instance Ledger.Typed.Scripts.ValidatorTypes RadSaleOnChain where
  type RedeemerType RadSaleOnChain = SaleAction
  type DatumType RadSaleOnChain = ()

-- In the following the main validation logic is defined including
-- addtional helper functions.

-- The isValid function is the compositional root of the contract logic.
-- The contract verifies the following
--   - the transaction to the seller of the token is valid
--   - the transaction to the buyer of the token is valid
--   - the output datum is correct
--   - the output value going back to the script is correct

{-# INLINEABLE isValid #-}
isValid ::
  PlutusTx.Prelude.Bool ->
  PlutusTx.Prelude.Bool ->
  PlutusTx.Prelude.Bool ->
  PlutusTx.Prelude.Bool ->
  PlutusTx.Prelude.Bool
isValid
  txToSeller
  txToBuyer
  outputDatum
  scriptOutputValue =
    txToSeller
      PlutusTx.Prelude.&& txToBuyer
      PlutusTx.Prelude.&& outputDatum
      PlutusTx.Prelude.&& scriptOutputValue
      PlutusTx.Prelude.== PlutusTx.Prelude.True

-- In the following validation logic helper functions are defined

-- A valid transaction must include the cost of the token
-- value paid to the seller of the token

isTxToSeller ::
  TokenSaleParam ->
  Plutus.V1.Ledger.Contexts.TxInfo ->
  PlutusTx.Either.Either
    PlutusTx.Builtins.Internal.BuiltinString
    PlutusTx.Prelude.Bool
isTxToSeller tkSaleParam info =
  if ( Plutus.V1.Ledger.Contexts.valuePaidTo
         info
         ( sellerPubKeyHash tkSaleParam
         )
         PlutusTx.Prelude.== Ledger.Ada.lovelaceValueOf
           (tokenCost tkSaleParam)
     )
    PlutusTx.Prelude.== PlutusTx.Prelude.True
    then PlutusTx.Either.Right PlutusTx.Prelude.True
    else PlutusTx.Either.Left "Incorrect Tx to seller"

-- A valid transaction must include a signature from the buyer of the token

tokenBuyerPaymentPubKeyHashsEither info =
  case (Plutus.V1.Ledger.Contexts.txInfoSignatories info) of
    [] ->
      PlutusTx.Either.Left "No signer"
    signatories ->
      PlutusTx.Either.Right signatories

-- A valid transaction must include exactly one output to the buyer
-- consisting of the token with the given currency symbol and
-- token name

isTxToBuyer ::
  TokenSaleParam ->
  Plutus.V1.Ledger.Contexts.TxInfo ->
  PlutusTx.Either.Either
    PlutusTx.Builtins.Internal.BuiltinString
    PlutusTx.Prelude.Bool
isTxToBuyer tkSaleParam info =
  tokenBuyerPaymentPubKeyHashsEither info
    PlutusTx.Prelude.>>= ( \signatories ->
                             let signer = PlutusTx.Prelude.head signatories
                                 valuePaidToBuyer =
                                   Plutus.V1.Ledger.Contexts.valuePaidTo
                                     info
                                     signer
                              in if ( Plutus.V1.Ledger.Value.assetClassValueOf
                                        valuePaidToBuyer
                                        ( Plutus.V1.Ledger.Value.assetClass
                                            (currencySymbol tkSaleParam)
                                            (tokenName tkSaleParam)
                                        )
                                        PlutusTx.Prelude.== 1
                                    )
                                   then PlutusTx.Either.Right PlutusTx.Prelude.True
                                   else PlutusTx.Either.Left "Incorrect Tx to buyer"
                         )

correctOutputDatum ::
  Plutus.V1.Ledger.Contexts.ScriptContext ->
  PlutusTx.Either.Either
    PlutusTx.Builtins.Internal.BuiltinString
    PlutusTx.Prelude.Bool
correctOutputDatum context = case Plutus.V1.Ledger.Contexts.getContinuingOutputs context of
  [o] ->
    let cOutDat =
          Ledger.txOutDatum o
            PlutusTx.Prelude.>>= ( \datumHash ->
                                     Ledger.findDatum datumHash (Plutus.V1.Ledger.Contexts.scriptContextTxInfo context)
                                       PlutusTx.Prelude.>>= ( \(Plutus.V1.Ledger.Scripts.Datum d) ->
                                                                (PlutusTx.fromBuiltinData d :: PlutusTx.Prelude.Maybe ())
                                                                  PlutusTx.Prelude.>>= (\datValue -> PlutusTx.Prelude.Just PlutusTx.Prelude.True)
                                                            )
                                 )
     in case cOutDat of
          PlutusTx.Prelude.Nothing ->
            PlutusTx.Either.Left "Error converting txOutDatum to unit"
          PlutusTx.Prelude.Just val ->
            PlutusTx.Either.Right val
  _ -> PlutusTx.Either.Left "No continuing output"

correctScriptOutputValue ::
  TokenSaleParam ->
  Plutus.V1.Ledger.Contexts.ScriptContext ->
  PlutusTx.Either.Either
    PlutusTx.Builtins.Internal.BuiltinString
    PlutusTx.Prelude.Bool
correctScriptOutputValue tokenSaleParam context =
  if PlutusTx.Prelude.any
    ( \output ->
        let info = Plutus.V1.Ledger.Contexts.scriptContextTxInfo context
            totalValue = Ledger.valueLockedBy info (Ledger.ownHash context)
            amountOfNativeToken =
              Plutus.V1.Ledger.Value.assetClassValueOf
                totalValue
                ( Plutus.V1.Ledger.Value.assetClass
                    (currencySymbol tokenSaleParam)
                    (tokenName tokenSaleParam)
                )
            newValueOfNativeToken =
              Plutus.V1.Ledger.Api.singleton
                (currencySymbol tokenSaleParam)
                (tokenName tokenSaleParam)
                (amountOfNativeToken)
            adaValue = (Plutus.V1.Ledger.Ada.toValue PlutusTx.Prelude.. Plutus.V1.Ledger.Ada.fromValue) totalValue
         in Plutus.V1.Ledger.Contexts.txOutValue output
              PlutusTx.Prelude.== adaValue PlutusTx.Prelude.<> newValueOfNativeToken
    )
    (Plutus.V1.Ledger.Contexts.getContinuingOutputs context)
    PlutusTx.Prelude.== PlutusTx.Prelude.True
    then PlutusTx.Prelude.Right PlutusTx.Prelude.True
    else PlutusTx.Prelude.Left "No output to script"

{-# INLINEABLE mkRadSaleOnChainValidator #-}
mkRadSaleOnChainValidator ::
  TokenSaleParam ->
  () ->
  SaleAction ->
  Plutus.V1.Ledger.Contexts.ScriptContext ->
  PlutusTx.Prelude.Bool
mkRadSaleOnChainValidator tkSaleParam () redeemer context =
  let info :: Plutus.V1.Ledger.Contexts.TxInfo
      info = Plutus.V1.Ledger.Contexts.scriptContextTxInfo context
   in case redeemer of
        Close ->
          Ledger.txSignedBy
            info
            (sellerPubKeyHash tkSaleParam)
        Buy ->
          case PlutusTx.Applicative.pure isValid
            PlutusTx.Applicative.<*> isTxToSeller tkSaleParam info
            PlutusTx.Applicative.<*> isTxToBuyer tkSaleParam info
            PlutusTx.Applicative.<*> correctOutputDatum context
            PlutusTx.Applicative.<*> correctScriptOutputValue tkSaleParam context of
            PlutusTx.Either.Right PlutusTx.Prelude.True -> PlutusTx.Prelude.True
            PlutusTx.Either.Left error ->
              PlutusTx.Prelude.traceIfFalse error PlutusTx.Prelude.False

typedValidator ::
  TokenSaleParam ->
  Ledger.Typed.Scripts.TypedValidator RadSaleOnChain
typedValidator p =
  Ledger.Typed.Scripts.mkTypedValidator @RadSaleOnChain
    ( $$(PlutusTx.compile [||mkRadSaleOnChainValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode p
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Ledger.Typed.Scripts.wrapValidator @() @SaleAction

validator :: TokenSaleParam -> Plutus.V1.Ledger.Scripts.Validator
validator =
  Ledger.Typed.Scripts.validatorScript
    PlutusTx.Prelude.. typedValidator

radSaleHash :: Script.TokenSaleParam -> Ledger.ValidatorHash
radSaleHash tokenSaleParam =
  Ledger.Typed.Scripts.validatorHash
    (Script.typedValidator tokenSaleParam)

scrAddress :: Script.TokenSaleParam -> Ledger.Address
scrAddress tokenSaleParam =
  Ledger.Address.scriptAddress
    (Script.validator tokenSaleParam)

radSaleOnChainScript :: TokenSaleParam -> Plutus.V1.Ledger.Scripts.Script
radSaleOnChainScript =
  Plutus.V1.Ledger.Scripts.unValidatorScript
    PlutusTx.Prelude.. validator

radSaleOnChainSBS :: TokenSaleParam -> Data.ByteString.Short.ShortByteString
radSaleOnChainSBS p =
  Data.ByteString.Short.toShort
    PlutusTx.Prelude.. Data.ByteString.Lazy.toStrict
    PlutusTx.Prelude.$ Codec.Serialise.serialise (radSaleOnChainScript p)

radSaleOnChainSerialised ::
  TokenSaleParam ->
  Cardano.Api.PlutusScript Cardano.Api.PlutusScriptV1
radSaleOnChainSerialised p = Cardano.Api.Shelley.PlutusScriptSerialised (radSaleOnChainSBS p)
