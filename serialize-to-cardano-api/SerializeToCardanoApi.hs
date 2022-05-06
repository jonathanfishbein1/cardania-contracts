{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SerializeToCardanoApi
  ( dataToScriptData,
    writeJSON,
    unsafePaymentPubKeyHash,
    unsafeReadAddress,
    unsafeTokenNameToHex,
  )
where

import qualified Cardano.Api
import qualified Cardano.Api.Shelley
import qualified Cardano.Crypto.Hash.Class
import qualified Cardano.Ledger.Credential
import qualified Cardano.Ledger.Crypto
import qualified Cardano.Ledger.Hashes
import qualified Cardano.Ledger.Keys
import qualified Data.Aeson
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.Maybe
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
import qualified PlutusTx.Builtins.Internal
import qualified PlutusTx.Prelude
import qualified System.Environment
import qualified Prelude

dataToScriptData :: PlutusTx.Data -> Cardano.Api.Shelley.ScriptData
dataToScriptData (PlutusTx.Constr n xs) = Cardano.Api.Shelley.ScriptDataConstructor n PlutusTx.Prelude.$ dataToScriptData PlutusTx.Prelude.<$> xs
dataToScriptData (PlutusTx.Map xs) = Cardano.Api.Shelley.ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (PlutusTx.List xs) = Cardano.Api.Shelley.ScriptDataList PlutusTx.Prelude.$ dataToScriptData PlutusTx.Prelude.<$> xs
dataToScriptData (PlutusTx.I n) = Cardano.Api.Shelley.ScriptDataNumber n
dataToScriptData (PlutusTx.B bs) = Cardano.Api.Shelley.ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => Prelude.FilePath -> a -> Prelude.IO ()
writeJSON file =
  Data.ByteString.Lazy.writeFile file
    PlutusTx.Prelude.. Data.Aeson.encode
    PlutusTx.Prelude.. Cardano.Api.Shelley.scriptDataToJson Cardano.Api.Shelley.ScriptDataJsonDetailedSchema
    PlutusTx.Prelude.. dataToScriptData
    PlutusTx.Prelude.. PlutusTx.toData

writeUnit :: Prelude.IO ()
writeUnit = writeJSON "/home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json" ()

credentialLedgerToPlutus ::
  Cardano.Ledger.Credential.Credential a Cardano.Ledger.Crypto.StandardCrypto ->
  Plutus.V1.Ledger.Api.Credential
credentialLedgerToPlutus
  ( Cardano.Ledger.Credential.ScriptHashObj
      (Cardano.Ledger.Hashes.ScriptHash h)
    ) =
    Plutus.V1.Ledger.Api.ScriptCredential PlutusTx.Prelude.$
      Ledger.ValidatorHash PlutusTx.Prelude.$
        PlutusTx.Builtins.toBuiltin PlutusTx.Prelude.$
          Cardano.Crypto.Hash.Class.hashToBytes h
credentialLedgerToPlutus (Cardano.Ledger.Credential.KeyHashObj (Cardano.Ledger.Keys.KeyHash h)) =
  Plutus.V1.Ledger.Api.PubKeyCredential PlutusTx.Prelude.$
    Ledger.PubKeyHash PlutusTx.Prelude.$
      PlutusTx.Builtins.toBuiltin PlutusTx.Prelude.$ Cardano.Crypto.Hash.Class.hashToBytes h

tryReadAddress ::
  Prelude.String ->
  PlutusTx.Prelude.Maybe Ledger.Address
tryReadAddress x = case Cardano.Api.Shelley.deserialiseAddress
  Cardano.Api.Shelley.AsAddressAny
  PlutusTx.Prelude.$ Data.Text.pack x of
  PlutusTx.Prelude.Nothing -> PlutusTx.Prelude.Nothing
  PlutusTx.Prelude.Just (Cardano.Api.AddressByron _) -> PlutusTx.Prelude.Nothing
  PlutusTx.Prelude.Just
    (Cardano.Api.AddressShelley (Cardano.Api.Shelley.ShelleyAddress _ p s)) ->
      PlutusTx.Prelude.Just
        Ledger.Address
          { Ledger.addressCredential = credentialLedgerToPlutus p,
            Ledger.addressStakingCredential = PlutusTx.Prelude.Nothing
          }

unsafeReadAddress :: Prelude.String -> Ledger.Address
unsafeReadAddress s =
  PlutusTx.Prelude.fromMaybe
    ( Prelude.error PlutusTx.Prelude.$
        "can't parse " PlutusTx.Prelude.++ s PlutusTx.Prelude.++ " as an address"
    )
    PlutusTx.Prelude.$ tryReadAddress s

getCredentials ::
  Ledger.Address ->
  PlutusTx.Prelude.Maybe (Ledger.PaymentPubKeyHash, PlutusTx.Prelude.Maybe Ledger.StakePubKeyHash)
getCredentials (Ledger.Address x y) = case x of
  Plutus.V1.Ledger.Api.ScriptCredential _ -> PlutusTx.Prelude.Nothing
  Plutus.V1.Ledger.Api.PubKeyCredential pkh ->
    let ppkh = Ledger.PaymentPubKeyHash pkh
     in case y of
          PlutusTx.Prelude.Nothing ->
            PlutusTx.Prelude.Just
              (ppkh, PlutusTx.Prelude.Nothing)
          PlutusTx.Prelude.Just (Plutus.V1.Ledger.Api.StakingPtr _ _ _) ->
            PlutusTx.Prelude.Nothing
          PlutusTx.Prelude.Just (Plutus.V1.Ledger.Api.StakingHash h) -> case h of
            Plutus.V1.Ledger.Api.ScriptCredential _ ->
              PlutusTx.Prelude.Nothing
            Plutus.V1.Ledger.Api.PubKeyCredential pkh' ->
              PlutusTx.Prelude.Just
                (ppkh, PlutusTx.Prelude.Just PlutusTx.Prelude.$ Ledger.StakePubKeyHash pkh')

unsafePaymentPubKeyHash :: Ledger.Address -> Ledger.PaymentPubKeyHash
unsafePaymentPubKeyHash addr =
  PlutusTx.Prelude.maybe
    ( Prelude.error PlutusTx.Prelude.$
        "script address "
          PlutusTx.Prelude.++ Prelude.show addr
          PlutusTx.Prelude.++ " does not contain a payment key"
    )
    PlutusTx.Prelude.fst
    PlutusTx.Prelude.$ getCredentials addr

unsafeTokenNameToHex :: Plutus.V1.Ledger.Value.TokenName -> Prelude.String
unsafeTokenNameToHex =
  Data.ByteString.Char8.unpack
    PlutusTx.Prelude.. Cardano.Api.Shelley.serialiseToRawBytesHex
    PlutusTx.Prelude.. Data.Maybe.fromJust
    PlutusTx.Prelude.. Cardano.Api.Shelley.deserialiseFromRawBytes Cardano.Api.Shelley.AsAssetName
    PlutusTx.Prelude.. getByteString
    PlutusTx.Prelude.. Plutus.V1.Ledger.Value.unTokenName
  where
    getByteString (PlutusTx.Builtins.Internal.BuiltinByteString bs) = bs