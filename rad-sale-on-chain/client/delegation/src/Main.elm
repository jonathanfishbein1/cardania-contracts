port module Main exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events


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


encodeWallet : SupportedWallet -> String
encodeWallet wallet =
    case wallet of
        Nami ->
            "nami"

        Eternl ->
            "eternl"

        Flint ->
            "flint"


type SupportedWallet
    = Nami
    | Eternl
    | Flint


type Msg
    = Connect SupportedWallet
    | Disconnect SupportedWallet
    | NoOp
    | WalletConnected (Maybe SupportedWallet)


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
            , getDelegationStatus ()
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
                    "Disconnect"

                Connecting ->
                    "Connecting"

                NullState ->
                    "Connect"
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    walletConnection (\s -> WalletConnected (decodeWallet s))


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


port getDelegationStatus : () -> Cmd msg
