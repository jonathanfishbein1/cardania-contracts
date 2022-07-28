port module Main exposing (main)

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
    = Connect SupportedWallet
    | Disconnect SupportedWallet
    | NoOp
    | WalletConnected (Maybe SupportedWallet)
    | ReceiveAccountStatus (Result Json.Decode.Error Account)


type DelegationStatus
    = NotDelegating
    | DelegatingToSumn
    | DelegatingToOther


type Model
    = NotConnectedAbleTo SupportedWallet
    | NotConnectedNotAbleTo
    | Connecting
    | ConnectionEstablished SupportedWallet
    | Connected SupportedWallet DelegationStatus
    | NullState


init : String -> ( Model, Cmd Msg )
init supportedWallet =
    let
        wallet =
            decodeWallet supportedWallet
    in
    ( case wallet of
        Just w ->
            NotConnectedAbleTo w

        Nothing ->
            NotConnectedNotAbleTo
    , Cmd.none
    )


sumnPoolId =
    "pool13dgxp4ph2ut5datuh5na4wy7hrnqgkj4fyvac3e8fzfqcc7qh0h"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Connect w ->
            ( Connecting
            , connectWallet (encodeWallet w)
            )

        Disconnect wallet ->
            ( NotConnectedAbleTo wallet, Cmd.none )

        WalletConnected wallet ->
            ( case wallet of
                Just w ->
                    ConnectionEstablished w

                Nothing ->
                    NotConnectedNotAbleTo
            , getAccountStatus ()
            )

        ReceiveAccountStatus account ->
            let
                wallet =
                    case model of
                        ConnectionEstablished w ->
                            w

                        _ ->
                            Nami
            in
            ( case account of
                Ok res ->
                    Connected wallet
                        (if res.active == False then
                            NotDelegating

                         else if res.active == True && res.pool_id /= sumnPoolId then
                            DelegatingToOther

                         else if res.active && res.pool_id == sumnPoolId then
                            DelegatingToSumn

                         else
                            NotDelegating
                        )

                Err e ->
                    let
                        p =
                            Debug.log "" ()
                    in
                    NullState
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.button
        [ Html.Events.onClick
            (case model of
                NotConnectedAbleTo w ->
                    Connect w

                NotConnectedNotAbleTo ->
                    NoOp

                ConnectionEstablished w ->
                    Disconnect w

                Connected w d ->
                    Disconnect w

                Connecting ->
                    NoOp

                NullState ->
                    NoOp
            )
        ]
        [ Html.text
            (case model of
                NotConnectedAbleTo w ->
                    "Connect"

                NotConnectedNotAbleTo ->
                    "No available wallet"

                ConnectionEstablished w ->
                    "Disconnect"

                Connected w d ->
                    case d of
                        NotDelegating ->
                            "Delegate"

                        DelegatingToOther ->
                            "Delegate"

                        DelegatingToSumn ->
                            "Undelegate"

                Connecting ->
                    "Connecting"

                NullState ->
                    "Connect"
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ walletConnection (\s -> WalletConnected (decodeWallet s))
        , receiveAccountStatus (\s -> ReceiveAccountStatus (Json.Decode.decodeString decodeAccount s))
        ]


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port connectWallet : String -> Cmd msg


port walletConnection : (String -> msg) -> Sub msg


port getAccountStatus : () -> Cmd msg


port receiveAccountStatus : (String -> msg) -> Sub msg
