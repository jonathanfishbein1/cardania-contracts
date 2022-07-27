port module Main exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events


decodeWallet : String -> SupportedWallet
decodeWallet status =
    case status of
        "nami" ->
            Nami

        "Eternl" ->
            Eternl

        _ ->
            Flint


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
    , wallet : SupportedWallet
    }


init : String -> ( Model, Cmd Msg )
init supportedWallet =
    ( { state = NotConnected
      , wallet = decodeWallet supportedWallet
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Connect ->
            ( { model | state = Connected }, connectWallet () )

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
        [ Html.text (Debug.toString model) ]


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


port connectWallet : () -> Cmd msg
