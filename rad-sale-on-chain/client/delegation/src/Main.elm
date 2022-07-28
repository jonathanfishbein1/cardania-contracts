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
    = Connect String SupportedWallet
    | Disconnect String SupportedWallet
    | NoOp
    | WalletConnected (Maybe SupportedWallet)
    | ReceiveAccountStatus (Result Json.Decode.Error Account)
    | RegisterAndDelegateToSumn Account
    | DelegateToSumn
    | UndelegateFromSumn


type DelegationStatus
    = NotDelegating
    | DelegatingToSumn
    | DelegatingToOther


type Model
    = NotConnectedAbleTo String SupportedWallet
    | NotConnectedNotAbleTo
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
            ( Connecting sumnPoolId
            , connectWallet (encodeWallet w)
            )

        Disconnect sumnPoolId wallet ->
            ( NotConnectedAbleTo sumnPoolId wallet, Cmd.none )

        WalletConnected wallet ->
            let
                sumnPoolId =
                    case model of
                        ConnectionEstablished s _ ->
                            s

                        _ ->
                            ""
            in
            ( case wallet of
                Just w ->
                    ConnectionEstablished sumnPoolId w

                Nothing ->
                    NotConnectedNotAbleTo
            , getAccountStatus ()
            )

        ReceiveAccountStatus account ->
            let
                wallet =
                    case model of
                        ConnectionEstablished _ w ->
                            w

                        _ ->
                            Nami

                sumnPoolId =
                    case model of
                        ConnectionEstablished s _ ->
                            s

                        _ ->
                            ""
            in
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
                    let
                        p =
                            Debug.log "" ()
                    in
                    NullState
            , Cmd.none
            )

        RegisterAndDelegateToSumn account ->
            ( model, registerAndDelegateToSumn account.stake_address )

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
        [ walletConnection (\s -> WalletConnected (decodeWallet s))
        , receiveAccountStatus (\s -> ReceiveAccountStatus (Json.Decode.decodeString decodeAccount s))
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


port walletConnection : (String -> msg) -> Sub msg


port getAccountStatus : () -> Cmd msg


port receiveAccountStatus : (String -> msg) -> Sub msg


port registerAndDelegateToSumn : String -> Cmd msg
