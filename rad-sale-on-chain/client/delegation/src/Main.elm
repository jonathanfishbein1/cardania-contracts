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


type alias Success =
    { success : Bool
    }


decodeSuccess : Json.Decode.Decoder Success
decodeSuccess =
    Json.Decode.succeed Success
        |> Json.Decode.Pipeline.required "success" Json.Decode.bool


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
    | ReceiveRegisterAndDelegateStatus (Result Json.Decode.Error Success)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Connect sumnPoolId w ->
            case model of
                NotConnectedAbleTo p wallet ->
                    ( Connecting sumnPoolId, connectWallet (encodeWallet w) )

                _ ->
                    ( NotConnectedNotAbleTo
                    , Cmd.none
                    )

        Disconnect sumnPoolId wallet ->
            ( NotConnectedAbleTo sumnPoolId wallet, Cmd.none )

        ReceiveWalletConnected wallet ->
            case model of
                Connecting sumnPoolId ->
                    ( case wallet of
                        Just w ->
                            ConnectionEstablished sumnPoolId w

                        Nothing ->
                            NotConnectedNotAbleTo
                    , getAccountStatus ()
                    )

                _ ->
                    ( NullState
                    , Cmd.none
                    )

        ReceiveAccountStatus account ->
            case model of
                ConnectionEstablished sumnPoolId wallet ->
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

                _ ->
                    ( NullState
                    , Cmd.none
                    )

        RegisterAndDelegateToSumn account ->
            ( model, registerAndDelegateToSumn account.stake_address )

        ReceiveRegisterAndDelegateStatus result ->
            let
                s =
                    case result of
                        Ok r ->
                            Debug.log
                                (case r.success of
                                    True ->
                                        "true"

                                    False ->
                                        "false"
                                )
                                ()

                        Err e ->
                            Debug.log "e" ()
            in
            ( model
            , Cmd.none
            )

        DelegateToSumn ->
            ( model, Cmd.none )

        UndelegateFromSumn ->
            ( model, Cmd.none )

        NoOp ->
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
                    Disconnect sumnPoolId w

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
                    "Disconnect"

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
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveWalletConnection (\s -> ReceiveWalletConnected (decodeWallet s))
        , receiveAccountStatus (\s -> ReceiveAccountStatus (Json.Decode.decodeString decodeAccount s))
        , receiveRegisterAndDelegateStatus (\s -> ReceiveRegisterAndDelegateStatus (Json.Decode.decodeString decodeSuccess s))
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


port receiveRegisterAndDelegateStatus : (String -> msg) -> Sub msg
