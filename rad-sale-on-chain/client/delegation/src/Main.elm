port module Main exposing
    ( Account
    , DelegationStatus(..)
    , Model(..)
    , Msg(..)
    , SupportedWallet(..)
    , encodeWallet
    , init
    , main
    , update
    )

import Browser
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Decode.Pipeline


decodeWallet : String -> Maybe SupportedWallet
decodeWallet status =
    case status of
        "nami" ->
            Just Nami

        "eternl" ->
            Just Eternl

        "flint" ->
            Just Flint

        _ ->
            Nothing


decodeAccount : Json.Decode.Decoder Account
decodeAccount =
    Json.Decode.succeed Account
        |> Json.Decode.Pipeline.required "stake_address" Json.Decode.string
        |> Json.Decode.Pipeline.optional "pool_id" Json.Decode.string ""
        |> Json.Decode.Pipeline.required "active" Json.Decode.bool


encodeWallet : SupportedWallet -> String
encodeWallet wallet =
    case wallet of
        Nami ->
            "nami"

        Eternl ->
            "eternl"

        Flint ->
            "flint"


type alias Account =
    { stake_address : String
    , pool_id : String
    , active : Bool
    }


type SupportedWallet
    = Nami
    | Eternl
    | Flint


type Msg
    = Connect String SupportedWallet
    | Disconnect String SupportedWallet
    | NoOp
    | ReceiveWalletConnected (Maybe SupportedWallet)
    | ReceiveAccountStatus (Result Json.Decode.Error Account)
    | RegisterAndDelegateToSumn Account
    | ReceiveRegisterAndDelegateStatus Bool
    | DelegateToSumn
    | UndelegateFromSumn


type DelegationStatus
    = NotDelegating
    | DelegatingToSumn
    | DelegatingToOther


type Model
    = NotConnectedNotAbleTo
    | NotConnectedAbleTo String SupportedWallet
    | Connecting String
    | ConnectionEstablished String SupportedWallet
    | Connected String SupportedWallet Account DelegationStatus
    | Delegating String SupportedWallet Account
    | NullState


init : ( String, String ) -> ( Model, Cmd Msg )
init ( supportedWallet, sumnPoolId ) =
    let
        wallet =
            decodeWallet supportedWallet
    in
    ( case wallet of
        Just w ->
            NotConnectedAbleTo sumnPoolId w

        Nothing ->
            NotConnectedNotAbleTo
    , Cmd.none
    )


update msg model =
    case ( msg, model ) of
        ( Connect sumnPoolId w, NotConnectedAbleTo p wallet ) ->
            ( Connecting sumnPoolId, connectWallet (encodeWallet w) )

        ( Disconnect sumnPoolId wallet, _ ) ->
            ( NotConnectedAbleTo sumnPoolId wallet, Cmd.none )

        ( ReceiveWalletConnected wallet, Connecting sumnPoolId ) ->
            ( case wallet of
                Just w ->
                    ConnectionEstablished sumnPoolId w

                Nothing ->
                    NotConnectedNotAbleTo
            , getAccountStatus ()
            )

        ( ReceiveAccountStatus account, ConnectionEstablished sumnPoolId wallet ) ->
            ( case account of
                Ok acc ->
                    Connected sumnPoolId
                        wallet
                        acc
                        (if acc.active == False then
                            NotDelegating

                         else if acc.active == True && acc.pool_id /= sumnPoolId then
                            DelegatingToOther

                         else if acc.active && acc.pool_id == sumnPoolId then
                            DelegatingToSumn

                         else
                            NotDelegating
                        )

                Err e ->
                    NullState
            , Cmd.none
            )

        ( RegisterAndDelegateToSumn a, Connected p w account NotDelegating ) ->
            ( Delegating p w account, registerAndDelegateToSumn account.stake_address )

        ( ReceiveRegisterAndDelegateStatus result, Delegating p w account ) ->
            let
                newModel =
                    if result == True then
                        Connected p w account DelegatingToSumn

                    else
                        Connected p w account NotDelegating
            in
            ( newModel
            , Cmd.none
            )

        ( DelegateToSumn, Connected p w account DelegatingToOther ) ->
            ( model, Cmd.none )

        ( UndelegateFromSumn, Connected p w account DelegatingToSumn ) ->
            ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.button
        [ Html.Events.onClick
            (case model of
                NotConnectedAbleTo sumnPoolId w ->
                    Connect sumnPoolId w

                NotConnectedNotAbleTo ->
                    NoOp

                ConnectionEstablished sumnPoolId w ->
                    NoOp

                Connected _ w acc d ->
                    case d of
                        NotDelegating ->
                            RegisterAndDelegateToSumn acc

                        DelegatingToOther ->
                            DelegateToSumn

                        DelegatingToSumn ->
                            UndelegateFromSumn

                Connecting _ ->
                    NoOp

                Delegating _ _ _ ->
                    NoOp

                NullState ->
                    NoOp
            )
        ]
        [ Html.text
            (case model of
                NotConnectedAbleTo _ w ->
                    "Connect"

                NotConnectedNotAbleTo ->
                    "No available wallet"

                ConnectionEstablished _ w ->
                    "Connection established"

                Connected _ w _ d ->
                    case d of
                        NotDelegating ->
                            "Delegate"

                        DelegatingToOther ->
                            "Delegate"

                        DelegatingToSumn ->
                            "Undelegate"

                Connecting _ ->
                    "Connecting"

                NullState ->
                    "Connect"

                Delegating _ _ _ ->
                    "Delegating"
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveWalletConnection (\s -> ReceiveWalletConnected (decodeWallet s))
        , receiveAccountStatus (\s -> ReceiveAccountStatus (Json.Decode.decodeString decodeAccount s))
        , receiveRegisterAndDelegateStatus ReceiveRegisterAndDelegateStatus
        ]


main : Program ( String, String ) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port connectWallet : String -> Cmd msg


port receiveWalletConnection : (String -> msg) -> Sub msg


port getAccountStatus : () -> Cmd msg


port receiveAccountStatus : (String -> msg) -> Sub msg


port registerAndDelegateToSumn : String -> Cmd msg


port receiveRegisterAndDelegateStatus : (Bool -> msg) -> Sub msg
