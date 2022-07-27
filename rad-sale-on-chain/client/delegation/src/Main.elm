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
    = Connect
    | Disconnect


type State
    = NotConnected
      -- | Connecting
    | Connected


type alias Model =
    { state : State
    , wallet : Maybe SupportedWallet
    }


init : String -> ( Model, Cmd Msg )
init supportedWallet =
    let
        wallet =
            decodeWallet supportedWallet
    in
    ( { state =
            case wallet of
                Just w ->
                    Connected

                Nothing ->
                    NotConnected
      , wallet = wallet
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Connect ->
            ( { model | state = Connected }
            , case model.wallet of
                Just w ->
                    connectWallet (encodeWallet w)

                Nothing ->
                    Cmd.none
            )

        Disconnect ->
            ( { model | state = NotConnected }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.button
        [ Html.Events.onClick
            (case model.state of
                NotConnected ->
                    Connect

                Connected ->
                    Disconnect
            )
        ]
        [ Html.text
            (case model.state of
                NotConnected ->
                    "Connect"

                Connected ->
                    "Disconnect"
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port connectWallet : String -> Cmd msg
