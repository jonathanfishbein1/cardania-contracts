-- File auto generated by purescript-bridge! --
module Plutus.Contract.Effects where

import Prelude

import Control.Lazy (defer)
import Control.Monad.Freer.Extras.Pagination (PageQuery)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.RawJson (RawJson)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Ledger.Address (PaymentPubKeyHash)
import Ledger.Constraints.OffChain (UnbalancedTx)
import Ledger.TimeSlot (SlotConversionError)
import Ledger.Tx (CardanoTx, ChainIndexTxOut)
import Plutus.ChainIndex.Api (IsUtxoResponse, TxosResponse, UtxosResponse)
import Plutus.ChainIndex.Tx (ChainIndexTx)
import Plutus.ChainIndex.Types (RollbackState, Tip, TxOutState)
import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Credential (Credential)
import Plutus.V1.Ledger.Interval (Interval)
import Plutus.V1.Ledger.Scripts (DatumHash, MintingPolicy, StakeValidator, Validator)
import Plutus.V1.Ledger.Slot (Slot)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Tx (TxOutRef)
import Plutus.V1.Ledger.TxId (TxId)
import Plutus.V1.Ledger.Value (AssetClass)
import Type.Proxy (Proxy(Proxy))
import Wallet.Emulator.Error (WalletAPIError)
import Wallet.Types (ContractInstanceId, EndpointDescription, EndpointValue)
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Map as Map

newtype ActiveEndpoint = ActiveEndpoint
  { aeDescription :: EndpointDescription
  , aeMetadata :: Maybe RawJson
  }

derive instance Eq ActiveEndpoint

instance Show ActiveEndpoint where
  show a = genericShow a

instance EncodeJson ActiveEndpoint where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { aeDescription: E.value :: _ EndpointDescription
        , aeMetadata: (E.maybe E.value) :: _ (Maybe RawJson)
        }
    )

instance DecodeJson ActiveEndpoint where
  decodeJson = defer \_ -> D.decode $
    ( ActiveEndpoint <$> D.record "ActiveEndpoint"
        { aeDescription: D.value :: _ EndpointDescription
        , aeMetadata: (D.maybe D.value) :: _ (Maybe RawJson)
        }
    )

derive instance Generic ActiveEndpoint _

derive instance Newtype ActiveEndpoint _

--------------------------------------------------------------------------------

_ActiveEndpoint :: Iso' ActiveEndpoint { aeDescription :: EndpointDescription, aeMetadata :: Maybe RawJson }
_ActiveEndpoint = _Newtype

--------------------------------------------------------------------------------

data BalanceTxResponse
  = BalanceTxFailed WalletAPIError
  | BalanceTxSuccess CardanoTx

derive instance Eq BalanceTxResponse

instance Show BalanceTxResponse where
  show a = genericShow a

instance EncodeJson BalanceTxResponse where
  encodeJson = defer \_ -> case _ of
    BalanceTxFailed a -> E.encodeTagged "BalanceTxFailed" a E.value
    BalanceTxSuccess a -> E.encodeTagged "BalanceTxSuccess" a E.value

instance DecodeJson BalanceTxResponse where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "BalanceTxResponse"
    $ Map.fromFoldable
        [ "BalanceTxFailed" /\ D.content (BalanceTxFailed <$> D.value)
        , "BalanceTxSuccess" /\ D.content (BalanceTxSuccess <$> D.value)
        ]

derive instance Generic BalanceTxResponse _

--------------------------------------------------------------------------------

_BalanceTxFailed :: Prism' BalanceTxResponse WalletAPIError
_BalanceTxFailed = prism' BalanceTxFailed case _ of
  (BalanceTxFailed a) -> Just a
  _ -> Nothing

_BalanceTxSuccess :: Prism' BalanceTxResponse CardanoTx
_BalanceTxSuccess = prism' BalanceTxSuccess case _ of
  (BalanceTxSuccess a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

data ChainIndexQuery
  = DatumFromHash DatumHash
  | ValidatorFromHash String
  | MintingPolicyFromHash String
  | StakeValidatorFromHash String
  | RedeemerFromHash String
  | UnspentTxOutFromRef TxOutRef
  | UtxoSetMembership TxOutRef
  | UtxoSetAtAddress (PageQuery TxOutRef) Credential
  | UtxoSetWithCurrency (PageQuery TxOutRef) AssetClass
  | TxoSetAtAddress (PageQuery TxOutRef) Credential
  | GetTip

derive instance Eq ChainIndexQuery

instance Show ChainIndexQuery where
  show a = genericShow a

instance EncodeJson ChainIndexQuery where
  encodeJson = defer \_ -> case _ of
    DatumFromHash a -> E.encodeTagged "DatumFromHash" a E.value
    ValidatorFromHash a -> E.encodeTagged "ValidatorFromHash" a E.value
    MintingPolicyFromHash a -> E.encodeTagged "MintingPolicyFromHash" a E.value
    StakeValidatorFromHash a -> E.encodeTagged "StakeValidatorFromHash" a E.value
    RedeemerFromHash a -> E.encodeTagged "RedeemerFromHash" a E.value
    UnspentTxOutFromRef a -> E.encodeTagged "UnspentTxOutFromRef" a E.value
    UtxoSetMembership a -> E.encodeTagged "UtxoSetMembership" a E.value
    UtxoSetAtAddress a b -> E.encodeTagged "UtxoSetAtAddress" (a /\ b) (E.tuple (E.value >/\< E.value))
    UtxoSetWithCurrency a b -> E.encodeTagged "UtxoSetWithCurrency" (a /\ b) (E.tuple (E.value >/\< E.value))
    TxoSetAtAddress a b -> E.encodeTagged "TxoSetAtAddress" (a /\ b) (E.tuple (E.value >/\< E.value))
    GetTip -> encodeJson { tag: "GetTip", contents: jsonNull }

instance DecodeJson ChainIndexQuery where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "ChainIndexQuery"
    $ Map.fromFoldable
        [ "DatumFromHash" /\ D.content (DatumFromHash <$> D.value)
        , "ValidatorFromHash" /\ D.content (ValidatorFromHash <$> D.value)
        , "MintingPolicyFromHash" /\ D.content (MintingPolicyFromHash <$> D.value)
        , "StakeValidatorFromHash" /\ D.content (StakeValidatorFromHash <$> D.value)
        , "RedeemerFromHash" /\ D.content (RedeemerFromHash <$> D.value)
        , "UnspentTxOutFromRef" /\ D.content (UnspentTxOutFromRef <$> D.value)
        , "UtxoSetMembership" /\ D.content (UtxoSetMembership <$> D.value)
        , "UtxoSetAtAddress" /\ D.content (D.tuple $ UtxoSetAtAddress </$\> D.value </*\> D.value)
        , "UtxoSetWithCurrency" /\ D.content (D.tuple $ UtxoSetWithCurrency </$\> D.value </*\> D.value)
        , "TxoSetAtAddress" /\ D.content (D.tuple $ TxoSetAtAddress </$\> D.value </*\> D.value)
        , "GetTip" /\ pure GetTip
        ]

derive instance Generic ChainIndexQuery _

--------------------------------------------------------------------------------

_DatumFromHash :: Prism' ChainIndexQuery DatumHash
_DatumFromHash = prism' DatumFromHash case _ of
  (DatumFromHash a) -> Just a
  _ -> Nothing

_ValidatorFromHash :: Prism' ChainIndexQuery String
_ValidatorFromHash = prism' ValidatorFromHash case _ of
  (ValidatorFromHash a) -> Just a
  _ -> Nothing

_MintingPolicyFromHash :: Prism' ChainIndexQuery String
_MintingPolicyFromHash = prism' MintingPolicyFromHash case _ of
  (MintingPolicyFromHash a) -> Just a
  _ -> Nothing

_StakeValidatorFromHash :: Prism' ChainIndexQuery String
_StakeValidatorFromHash = prism' StakeValidatorFromHash case _ of
  (StakeValidatorFromHash a) -> Just a
  _ -> Nothing

_RedeemerFromHash :: Prism' ChainIndexQuery String
_RedeemerFromHash = prism' RedeemerFromHash case _ of
  (RedeemerFromHash a) -> Just a
  _ -> Nothing

_UnspentTxOutFromRef :: Prism' ChainIndexQuery TxOutRef
_UnspentTxOutFromRef = prism' UnspentTxOutFromRef case _ of
  (UnspentTxOutFromRef a) -> Just a
  _ -> Nothing

_UtxoSetMembership :: Prism' ChainIndexQuery TxOutRef
_UtxoSetMembership = prism' UtxoSetMembership case _ of
  (UtxoSetMembership a) -> Just a
  _ -> Nothing

_UtxoSetAtAddress :: Prism' ChainIndexQuery { a :: PageQuery TxOutRef, b :: Credential }
_UtxoSetAtAddress = prism' (\{ a, b } -> (UtxoSetAtAddress a b)) case _ of
  (UtxoSetAtAddress a b) -> Just { a, b }
  _ -> Nothing

_UtxoSetWithCurrency :: Prism' ChainIndexQuery { a :: PageQuery TxOutRef, b :: AssetClass }
_UtxoSetWithCurrency = prism' (\{ a, b } -> (UtxoSetWithCurrency a b)) case _ of
  (UtxoSetWithCurrency a b) -> Just { a, b }
  _ -> Nothing

_TxoSetAtAddress :: Prism' ChainIndexQuery { a :: PageQuery TxOutRef, b :: Credential }
_TxoSetAtAddress = prism' (\{ a, b } -> (TxoSetAtAddress a b)) case _ of
  (TxoSetAtAddress a b) -> Just { a, b }
  _ -> Nothing

_GetTip :: Prism' ChainIndexQuery Unit
_GetTip = prism' (const GetTip) case _ of
  GetTip -> Just unit
  _ -> Nothing

--------------------------------------------------------------------------------

data ChainIndexResponse
  = DatumHashResponse (Maybe String)
  | ValidatorHashResponse (Maybe Validator)
  | MintingPolicyHashResponse (Maybe MintingPolicy)
  | StakeValidatorHashResponse (Maybe StakeValidator)
  | UnspentTxOutResponse (Maybe ChainIndexTxOut)
  | RedeemerHashResponse (Maybe String)
  | TxIdResponse (Maybe ChainIndexTx)
  | UtxoSetMembershipResponse IsUtxoResponse
  | UtxoSetAtResponse UtxosResponse
  | UtxoSetWithCurrencyResponse UtxosResponse
  | TxIdsResponse (Array ChainIndexTx)
  | TxoSetAtResponse TxosResponse
  | GetTipResponse Tip

derive instance Eq ChainIndexResponse

instance Show ChainIndexResponse where
  show a = genericShow a

instance EncodeJson ChainIndexResponse where
  encodeJson = defer \_ -> case _ of
    DatumHashResponse a -> E.encodeTagged "DatumHashResponse" a (E.maybe E.value)
    ValidatorHashResponse a -> E.encodeTagged "ValidatorHashResponse" a (E.maybe E.value)
    MintingPolicyHashResponse a -> E.encodeTagged "MintingPolicyHashResponse" a (E.maybe E.value)
    StakeValidatorHashResponse a -> E.encodeTagged "StakeValidatorHashResponse" a (E.maybe E.value)
    UnspentTxOutResponse a -> E.encodeTagged "UnspentTxOutResponse" a (E.maybe E.value)
    RedeemerHashResponse a -> E.encodeTagged "RedeemerHashResponse" a (E.maybe E.value)
    TxIdResponse a -> E.encodeTagged "TxIdResponse" a (E.maybe E.value)
    UtxoSetMembershipResponse a -> E.encodeTagged "UtxoSetMembershipResponse" a E.value
    UtxoSetAtResponse a -> E.encodeTagged "UtxoSetAtResponse" a E.value
    UtxoSetWithCurrencyResponse a -> E.encodeTagged "UtxoSetWithCurrencyResponse" a E.value
    TxIdsResponse a -> E.encodeTagged "TxIdsResponse" a E.value
    TxoSetAtResponse a -> E.encodeTagged "TxoSetAtResponse" a E.value
    GetTipResponse a -> E.encodeTagged "GetTipResponse" a E.value

instance DecodeJson ChainIndexResponse where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "ChainIndexResponse"
    $ Map.fromFoldable
        [ "DatumHashResponse" /\ D.content (DatumHashResponse <$> (D.maybe D.value))
        , "ValidatorHashResponse" /\ D.content (ValidatorHashResponse <$> (D.maybe D.value))
        , "MintingPolicyHashResponse" /\ D.content (MintingPolicyHashResponse <$> (D.maybe D.value))
        , "StakeValidatorHashResponse" /\ D.content (StakeValidatorHashResponse <$> (D.maybe D.value))
        , "UnspentTxOutResponse" /\ D.content (UnspentTxOutResponse <$> (D.maybe D.value))
        , "RedeemerHashResponse" /\ D.content (RedeemerHashResponse <$> (D.maybe D.value))
        , "TxIdResponse" /\ D.content (TxIdResponse <$> (D.maybe D.value))
        , "UtxoSetMembershipResponse" /\ D.content (UtxoSetMembershipResponse <$> D.value)
        , "UtxoSetAtResponse" /\ D.content (UtxoSetAtResponse <$> D.value)
        , "UtxoSetWithCurrencyResponse" /\ D.content (UtxoSetWithCurrencyResponse <$> D.value)
        , "TxIdsResponse" /\ D.content (TxIdsResponse <$> D.value)
        , "TxoSetAtResponse" /\ D.content (TxoSetAtResponse <$> D.value)
        , "GetTipResponse" /\ D.content (GetTipResponse <$> D.value)
        ]

derive instance Generic ChainIndexResponse _

--------------------------------------------------------------------------------

_DatumHashResponse :: Prism' ChainIndexResponse (Maybe String)
_DatumHashResponse = prism' DatumHashResponse case _ of
  (DatumHashResponse a) -> Just a
  _ -> Nothing

_ValidatorHashResponse :: Prism' ChainIndexResponse (Maybe Validator)
_ValidatorHashResponse = prism' ValidatorHashResponse case _ of
  (ValidatorHashResponse a) -> Just a
  _ -> Nothing

_MintingPolicyHashResponse :: Prism' ChainIndexResponse (Maybe MintingPolicy)
_MintingPolicyHashResponse = prism' MintingPolicyHashResponse case _ of
  (MintingPolicyHashResponse a) -> Just a
  _ -> Nothing

_StakeValidatorHashResponse :: Prism' ChainIndexResponse (Maybe StakeValidator)
_StakeValidatorHashResponse = prism' StakeValidatorHashResponse case _ of
  (StakeValidatorHashResponse a) -> Just a
  _ -> Nothing

_UnspentTxOutResponse :: Prism' ChainIndexResponse (Maybe ChainIndexTxOut)
_UnspentTxOutResponse = prism' UnspentTxOutResponse case _ of
  (UnspentTxOutResponse a) -> Just a
  _ -> Nothing

_RedeemerHashResponse :: Prism' ChainIndexResponse (Maybe String)
_RedeemerHashResponse = prism' RedeemerHashResponse case _ of
  (RedeemerHashResponse a) -> Just a
  _ -> Nothing

_TxIdResponse :: Prism' ChainIndexResponse (Maybe ChainIndexTx)
_TxIdResponse = prism' TxIdResponse case _ of
  (TxIdResponse a) -> Just a
  _ -> Nothing

_UtxoSetMembershipResponse :: Prism' ChainIndexResponse IsUtxoResponse
_UtxoSetMembershipResponse = prism' UtxoSetMembershipResponse case _ of
  (UtxoSetMembershipResponse a) -> Just a
  _ -> Nothing

_UtxoSetAtResponse :: Prism' ChainIndexResponse UtxosResponse
_UtxoSetAtResponse = prism' UtxoSetAtResponse case _ of
  (UtxoSetAtResponse a) -> Just a
  _ -> Nothing

_UtxoSetWithCurrencyResponse :: Prism' ChainIndexResponse UtxosResponse
_UtxoSetWithCurrencyResponse = prism' UtxoSetWithCurrencyResponse case _ of
  (UtxoSetWithCurrencyResponse a) -> Just a
  _ -> Nothing

_TxIdsResponse :: Prism' ChainIndexResponse (Array ChainIndexTx)
_TxIdsResponse = prism' TxIdsResponse case _ of
  (TxIdsResponse a) -> Just a
  _ -> Nothing

_TxoSetAtResponse :: Prism' ChainIndexResponse TxosResponse
_TxoSetAtResponse = prism' TxoSetAtResponse case _ of
  (TxoSetAtResponse a) -> Just a
  _ -> Nothing

_GetTipResponse :: Prism' ChainIndexResponse Tip
_GetTipResponse = prism' GetTipResponse case _ of
  (GetTipResponse a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

data PABReq
  = AwaitSlotReq Slot
  | AwaitTimeReq POSIXTime
  | AwaitUtxoSpentReq TxOutRef
  | AwaitUtxoProducedReq Address
  | AwaitTxStatusChangeReq TxId
  | AwaitTxOutStatusChangeReq TxOutRef
  | CurrentSlotReq
  | CurrentTimeReq
  | OwnContractInstanceIdReq
  | OwnPaymentPublicKeyHashReq
  | ChainIndexQueryReq ChainIndexQuery
  | BalanceTxReq UnbalancedTx
  | WriteBalancedTxReq CardanoTx
  | ExposeEndpointReq ActiveEndpoint
  | PosixTimeRangeToContainedSlotRangeReq (Interval POSIXTime)
  | YieldUnbalancedTxReq UnbalancedTx

derive instance Eq PABReq

instance Show PABReq where
  show a = genericShow a

instance EncodeJson PABReq where
  encodeJson = defer \_ -> case _ of
    AwaitSlotReq a -> E.encodeTagged "AwaitSlotReq" a E.value
    AwaitTimeReq a -> E.encodeTagged "AwaitTimeReq" a E.value
    AwaitUtxoSpentReq a -> E.encodeTagged "AwaitUtxoSpentReq" a E.value
    AwaitUtxoProducedReq a -> E.encodeTagged "AwaitUtxoProducedReq" a E.value
    AwaitTxStatusChangeReq a -> E.encodeTagged "AwaitTxStatusChangeReq" a E.value
    AwaitTxOutStatusChangeReq a -> E.encodeTagged "AwaitTxOutStatusChangeReq" a E.value
    CurrentSlotReq -> encodeJson { tag: "CurrentSlotReq", contents: jsonNull }
    CurrentTimeReq -> encodeJson { tag: "CurrentTimeReq", contents: jsonNull }
    OwnContractInstanceIdReq -> encodeJson { tag: "OwnContractInstanceIdReq", contents: jsonNull }
    OwnPaymentPublicKeyHashReq -> encodeJson { tag: "OwnPaymentPublicKeyHashReq", contents: jsonNull }
    ChainIndexQueryReq a -> E.encodeTagged "ChainIndexQueryReq" a E.value
    BalanceTxReq a -> E.encodeTagged "BalanceTxReq" a E.value
    WriteBalancedTxReq a -> E.encodeTagged "WriteBalancedTxReq" a E.value
    ExposeEndpointReq a -> E.encodeTagged "ExposeEndpointReq" a E.value
    PosixTimeRangeToContainedSlotRangeReq a -> E.encodeTagged "PosixTimeRangeToContainedSlotRangeReq" a E.value
    YieldUnbalancedTxReq a -> E.encodeTagged "YieldUnbalancedTxReq" a E.value

instance DecodeJson PABReq where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "PABReq"
    $ Map.fromFoldable
        [ "AwaitSlotReq" /\ D.content (AwaitSlotReq <$> D.value)
        , "AwaitTimeReq" /\ D.content (AwaitTimeReq <$> D.value)
        , "AwaitUtxoSpentReq" /\ D.content (AwaitUtxoSpentReq <$> D.value)
        , "AwaitUtxoProducedReq" /\ D.content (AwaitUtxoProducedReq <$> D.value)
        , "AwaitTxStatusChangeReq" /\ D.content (AwaitTxStatusChangeReq <$> D.value)
        , "AwaitTxOutStatusChangeReq" /\ D.content (AwaitTxOutStatusChangeReq <$> D.value)
        , "CurrentSlotReq" /\ pure CurrentSlotReq
        , "CurrentTimeReq" /\ pure CurrentTimeReq
        , "OwnContractInstanceIdReq" /\ pure OwnContractInstanceIdReq
        , "OwnPaymentPublicKeyHashReq" /\ pure OwnPaymentPublicKeyHashReq
        , "ChainIndexQueryReq" /\ D.content (ChainIndexQueryReq <$> D.value)
        , "BalanceTxReq" /\ D.content (BalanceTxReq <$> D.value)
        , "WriteBalancedTxReq" /\ D.content (WriteBalancedTxReq <$> D.value)
        , "ExposeEndpointReq" /\ D.content (ExposeEndpointReq <$> D.value)
        , "PosixTimeRangeToContainedSlotRangeReq" /\ D.content (PosixTimeRangeToContainedSlotRangeReq <$> D.value)
        , "YieldUnbalancedTxReq" /\ D.content (YieldUnbalancedTxReq <$> D.value)
        ]

derive instance Generic PABReq _

--------------------------------------------------------------------------------

_AwaitSlotReq :: Prism' PABReq Slot
_AwaitSlotReq = prism' AwaitSlotReq case _ of
  (AwaitSlotReq a) -> Just a
  _ -> Nothing

_AwaitTimeReq :: Prism' PABReq POSIXTime
_AwaitTimeReq = prism' AwaitTimeReq case _ of
  (AwaitTimeReq a) -> Just a
  _ -> Nothing

_AwaitUtxoSpentReq :: Prism' PABReq TxOutRef
_AwaitUtxoSpentReq = prism' AwaitUtxoSpentReq case _ of
  (AwaitUtxoSpentReq a) -> Just a
  _ -> Nothing

_AwaitUtxoProducedReq :: Prism' PABReq Address
_AwaitUtxoProducedReq = prism' AwaitUtxoProducedReq case _ of
  (AwaitUtxoProducedReq a) -> Just a
  _ -> Nothing

_AwaitTxStatusChangeReq :: Prism' PABReq TxId
_AwaitTxStatusChangeReq = prism' AwaitTxStatusChangeReq case _ of
  (AwaitTxStatusChangeReq a) -> Just a
  _ -> Nothing

_AwaitTxOutStatusChangeReq :: Prism' PABReq TxOutRef
_AwaitTxOutStatusChangeReq = prism' AwaitTxOutStatusChangeReq case _ of
  (AwaitTxOutStatusChangeReq a) -> Just a
  _ -> Nothing

_CurrentSlotReq :: Prism' PABReq Unit
_CurrentSlotReq = prism' (const CurrentSlotReq) case _ of
  CurrentSlotReq -> Just unit
  _ -> Nothing

_CurrentTimeReq :: Prism' PABReq Unit
_CurrentTimeReq = prism' (const CurrentTimeReq) case _ of
  CurrentTimeReq -> Just unit
  _ -> Nothing

_OwnContractInstanceIdReq :: Prism' PABReq Unit
_OwnContractInstanceIdReq = prism' (const OwnContractInstanceIdReq) case _ of
  OwnContractInstanceIdReq -> Just unit
  _ -> Nothing

_OwnPaymentPublicKeyHashReq :: Prism' PABReq Unit
_OwnPaymentPublicKeyHashReq = prism' (const OwnPaymentPublicKeyHashReq) case _ of
  OwnPaymentPublicKeyHashReq -> Just unit
  _ -> Nothing

_ChainIndexQueryReq :: Prism' PABReq ChainIndexQuery
_ChainIndexQueryReq = prism' ChainIndexQueryReq case _ of
  (ChainIndexQueryReq a) -> Just a
  _ -> Nothing

_BalanceTxReq :: Prism' PABReq UnbalancedTx
_BalanceTxReq = prism' BalanceTxReq case _ of
  (BalanceTxReq a) -> Just a
  _ -> Nothing

_WriteBalancedTxReq :: Prism' PABReq CardanoTx
_WriteBalancedTxReq = prism' WriteBalancedTxReq case _ of
  (WriteBalancedTxReq a) -> Just a
  _ -> Nothing

_ExposeEndpointReq :: Prism' PABReq ActiveEndpoint
_ExposeEndpointReq = prism' ExposeEndpointReq case _ of
  (ExposeEndpointReq a) -> Just a
  _ -> Nothing

_PosixTimeRangeToContainedSlotRangeReq :: Prism' PABReq (Interval POSIXTime)
_PosixTimeRangeToContainedSlotRangeReq = prism' PosixTimeRangeToContainedSlotRangeReq case _ of
  (PosixTimeRangeToContainedSlotRangeReq a) -> Just a
  _ -> Nothing

_YieldUnbalancedTxReq :: Prism' PABReq UnbalancedTx
_YieldUnbalancedTxReq = prism' YieldUnbalancedTxReq case _ of
  (YieldUnbalancedTxReq a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

data PABResp
  = AwaitSlotResp Slot
  | AwaitTimeResp POSIXTime
  | AwaitUtxoSpentResp ChainIndexTx
  | AwaitUtxoProducedResp (NonEmptyList ChainIndexTx)
  | AwaitTxStatusChangeResp TxId (RollbackState Unit)
  | AwaitTxOutStatusChangeResp TxOutRef (RollbackState TxOutState)
  | CurrentSlotResp Slot
  | CurrentTimeResp POSIXTime
  | OwnContractInstanceIdResp ContractInstanceId
  | OwnPaymentPublicKeyHashResp PaymentPubKeyHash
  | ChainIndexQueryResp ChainIndexResponse
  | BalanceTxResp BalanceTxResponse
  | WriteBalancedTxResp WriteBalancedTxResponse
  | ExposeEndpointResp EndpointDescription (EndpointValue RawJson)
  | PosixTimeRangeToContainedSlotRangeResp (Either SlotConversionError (Interval Slot))
  | YieldUnbalancedTxResp Unit

derive instance Eq PABResp

instance Show PABResp where
  show a = genericShow a

instance EncodeJson PABResp where
  encodeJson = defer \_ -> case _ of
    AwaitSlotResp a -> E.encodeTagged "AwaitSlotResp" a E.value
    AwaitTimeResp a -> E.encodeTagged "AwaitTimeResp" a E.value
    AwaitUtxoSpentResp a -> E.encodeTagged "AwaitUtxoSpentResp" a E.value
    AwaitUtxoProducedResp a -> E.encodeTagged "AwaitUtxoProducedResp" a E.value
    AwaitTxStatusChangeResp a b -> E.encodeTagged "AwaitTxStatusChangeResp" (a /\ b) (E.tuple (E.value >/\< E.value))
    AwaitTxOutStatusChangeResp a b -> E.encodeTagged "AwaitTxOutStatusChangeResp" (a /\ b) (E.tuple (E.value >/\< E.value))
    CurrentSlotResp a -> E.encodeTagged "CurrentSlotResp" a E.value
    CurrentTimeResp a -> E.encodeTagged "CurrentTimeResp" a E.value
    OwnContractInstanceIdResp a -> E.encodeTagged "OwnContractInstanceIdResp" a E.value
    OwnPaymentPublicKeyHashResp a -> E.encodeTagged "OwnPaymentPublicKeyHashResp" a E.value
    ChainIndexQueryResp a -> E.encodeTagged "ChainIndexQueryResp" a E.value
    BalanceTxResp a -> E.encodeTagged "BalanceTxResp" a E.value
    WriteBalancedTxResp a -> E.encodeTagged "WriteBalancedTxResp" a E.value
    ExposeEndpointResp a b -> E.encodeTagged "ExposeEndpointResp" (a /\ b) (E.tuple (E.value >/\< E.value))
    PosixTimeRangeToContainedSlotRangeResp a -> E.encodeTagged "PosixTimeRangeToContainedSlotRangeResp" a (E.either E.value E.value)
    YieldUnbalancedTxResp a -> E.encodeTagged "YieldUnbalancedTxResp" a E.unit

instance DecodeJson PABResp where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "PABResp"
    $ Map.fromFoldable
        [ "AwaitSlotResp" /\ D.content (AwaitSlotResp <$> D.value)
        , "AwaitTimeResp" /\ D.content (AwaitTimeResp <$> D.value)
        , "AwaitUtxoSpentResp" /\ D.content (AwaitUtxoSpentResp <$> D.value)
        , "AwaitUtxoProducedResp" /\ D.content (AwaitUtxoProducedResp <$> D.value)
        , "AwaitTxStatusChangeResp" /\ D.content (D.tuple $ AwaitTxStatusChangeResp </$\> D.value </*\> D.value)
        , "AwaitTxOutStatusChangeResp" /\ D.content (D.tuple $ AwaitTxOutStatusChangeResp </$\> D.value </*\> D.value)
        , "CurrentSlotResp" /\ D.content (CurrentSlotResp <$> D.value)
        , "CurrentTimeResp" /\ D.content (CurrentTimeResp <$> D.value)
        , "OwnContractInstanceIdResp" /\ D.content (OwnContractInstanceIdResp <$> D.value)
        , "OwnPaymentPublicKeyHashResp" /\ D.content (OwnPaymentPublicKeyHashResp <$> D.value)
        , "ChainIndexQueryResp" /\ D.content (ChainIndexQueryResp <$> D.value)
        , "BalanceTxResp" /\ D.content (BalanceTxResp <$> D.value)
        , "WriteBalancedTxResp" /\ D.content (WriteBalancedTxResp <$> D.value)
        , "ExposeEndpointResp" /\ D.content (D.tuple $ ExposeEndpointResp </$\> D.value </*\> D.value)
        , "PosixTimeRangeToContainedSlotRangeResp" /\ D.content (PosixTimeRangeToContainedSlotRangeResp <$> (D.either D.value D.value))
        , "YieldUnbalancedTxResp" /\ D.content (YieldUnbalancedTxResp <$> D.unit)
        ]

derive instance Generic PABResp _

--------------------------------------------------------------------------------

_AwaitSlotResp :: Prism' PABResp Slot
_AwaitSlotResp = prism' AwaitSlotResp case _ of
  (AwaitSlotResp a) -> Just a
  _ -> Nothing

_AwaitTimeResp :: Prism' PABResp POSIXTime
_AwaitTimeResp = prism' AwaitTimeResp case _ of
  (AwaitTimeResp a) -> Just a
  _ -> Nothing

_AwaitUtxoSpentResp :: Prism' PABResp ChainIndexTx
_AwaitUtxoSpentResp = prism' AwaitUtxoSpentResp case _ of
  (AwaitUtxoSpentResp a) -> Just a
  _ -> Nothing

_AwaitUtxoProducedResp :: Prism' PABResp (NonEmptyList ChainIndexTx)
_AwaitUtxoProducedResp = prism' AwaitUtxoProducedResp case _ of
  (AwaitUtxoProducedResp a) -> Just a
  _ -> Nothing

_AwaitTxStatusChangeResp :: Prism' PABResp { a :: TxId, b :: RollbackState Unit }
_AwaitTxStatusChangeResp = prism' (\{ a, b } -> (AwaitTxStatusChangeResp a b)) case _ of
  (AwaitTxStatusChangeResp a b) -> Just { a, b }
  _ -> Nothing

_AwaitTxOutStatusChangeResp :: Prism' PABResp { a :: TxOutRef, b :: RollbackState TxOutState }
_AwaitTxOutStatusChangeResp = prism' (\{ a, b } -> (AwaitTxOutStatusChangeResp a b)) case _ of
  (AwaitTxOutStatusChangeResp a b) -> Just { a, b }
  _ -> Nothing

_CurrentSlotResp :: Prism' PABResp Slot
_CurrentSlotResp = prism' CurrentSlotResp case _ of
  (CurrentSlotResp a) -> Just a
  _ -> Nothing

_CurrentTimeResp :: Prism' PABResp POSIXTime
_CurrentTimeResp = prism' CurrentTimeResp case _ of
  (CurrentTimeResp a) -> Just a
  _ -> Nothing

_OwnContractInstanceIdResp :: Prism' PABResp ContractInstanceId
_OwnContractInstanceIdResp = prism' OwnContractInstanceIdResp case _ of
  (OwnContractInstanceIdResp a) -> Just a
  _ -> Nothing

_OwnPaymentPublicKeyHashResp :: Prism' PABResp PaymentPubKeyHash
_OwnPaymentPublicKeyHashResp = prism' OwnPaymentPublicKeyHashResp case _ of
  (OwnPaymentPublicKeyHashResp a) -> Just a
  _ -> Nothing

_ChainIndexQueryResp :: Prism' PABResp ChainIndexResponse
_ChainIndexQueryResp = prism' ChainIndexQueryResp case _ of
  (ChainIndexQueryResp a) -> Just a
  _ -> Nothing

_BalanceTxResp :: Prism' PABResp BalanceTxResponse
_BalanceTxResp = prism' BalanceTxResp case _ of
  (BalanceTxResp a) -> Just a
  _ -> Nothing

_WriteBalancedTxResp :: Prism' PABResp WriteBalancedTxResponse
_WriteBalancedTxResp = prism' WriteBalancedTxResp case _ of
  (WriteBalancedTxResp a) -> Just a
  _ -> Nothing

_ExposeEndpointResp :: Prism' PABResp { a :: EndpointDescription, b :: EndpointValue RawJson }
_ExposeEndpointResp = prism' (\{ a, b } -> (ExposeEndpointResp a b)) case _ of
  (ExposeEndpointResp a b) -> Just { a, b }
  _ -> Nothing

_PosixTimeRangeToContainedSlotRangeResp :: Prism' PABResp (Either SlotConversionError (Interval Slot))
_PosixTimeRangeToContainedSlotRangeResp = prism' PosixTimeRangeToContainedSlotRangeResp case _ of
  (PosixTimeRangeToContainedSlotRangeResp a) -> Just a
  _ -> Nothing

_YieldUnbalancedTxResp :: Prism' PABResp Unit
_YieldUnbalancedTxResp = prism' YieldUnbalancedTxResp case _ of
  (YieldUnbalancedTxResp a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

data WriteBalancedTxResponse
  = WriteBalancedTxFailed WalletAPIError
  | WriteBalancedTxSuccess CardanoTx

derive instance Eq WriteBalancedTxResponse

instance Show WriteBalancedTxResponse where
  show a = genericShow a

instance EncodeJson WriteBalancedTxResponse where
  encodeJson = defer \_ -> case _ of
    WriteBalancedTxFailed a -> E.encodeTagged "WriteBalancedTxFailed" a E.value
    WriteBalancedTxSuccess a -> E.encodeTagged "WriteBalancedTxSuccess" a E.value

instance DecodeJson WriteBalancedTxResponse where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "WriteBalancedTxResponse"
    $ Map.fromFoldable
        [ "WriteBalancedTxFailed" /\ D.content (WriteBalancedTxFailed <$> D.value)
        , "WriteBalancedTxSuccess" /\ D.content (WriteBalancedTxSuccess <$> D.value)
        ]

derive instance Generic WriteBalancedTxResponse _

--------------------------------------------------------------------------------

_WriteBalancedTxFailed :: Prism' WriteBalancedTxResponse WalletAPIError
_WriteBalancedTxFailed = prism' WriteBalancedTxFailed case _ of
  (WriteBalancedTxFailed a) -> Just a
  _ -> Nothing

_WriteBalancedTxSuccess :: Prism' WriteBalancedTxResponse CardanoTx
_WriteBalancedTxSuccess = prism' WriteBalancedTxSuccess case _ of
  (WriteBalancedTxSuccess a) -> Just a
  _ -> Nothing