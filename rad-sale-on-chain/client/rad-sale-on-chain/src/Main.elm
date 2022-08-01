port module Main exposing
    ( Model(..)
    , Msg(..)
    , SupportedWallet(..)
    , encodeWallet
    , init
    , main
    , update
    , view
    )

import Browser
import Element
import Element.Background
import Element.Border
import Element.Input
import Html
import Html.Attributes
import Json.Decode
import Json.Decode.Pipeline


type alias TransactionSuccessStatus =
    Bool


type alias MouseOver =
    Bool


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
    | Disconnect SupportedWallet MouseOver
    | NoOp
    | ReceiveWalletConnected (Maybe SupportedWallet)
    | ReceiveMousedOverEvent MouseOver
    | ReceiveMouseOutEvent MouseOver


type Model
    = NotConnectedNotAbleTo
    | NotConnectedAbleTo SupportedWallet MouseOver
    | Connecting
    | ConnectionEstablished SupportedWallet
    | Connected SupportedWallet MouseOver
    | NullState


init : String -> ( Model, Cmd Msg )
init supportedWallet =
    let
        wallet =
            decodeWallet supportedWallet
    in
    ( case wallet of
        Just w ->
            NotConnectedAbleTo w False

        Nothing ->
            NotConnectedNotAbleTo
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( msg, model ) of
        ( Connect w, NotConnectedAbleTo wallet _ ) ->
            ( Connecting, connectWallet (encodeWallet w) )

        ( Disconnect wallet m, _ ) ->
            ( NotConnectedAbleTo wallet m, Cmd.none )

        ( ReceiveWalletConnected wallet, Connecting ) ->
            case wallet of
                Just w ->
                    ( ConnectionEstablished w, Cmd.none )

                Nothing ->
                    ( NotConnectedNotAbleTo, Cmd.none )

        ( ReceiveMousedOverEvent m, NotConnectedAbleTo b _ ) ->
            ( NotConnectedAbleTo b m, Cmd.none )

        ( ReceiveMouseOutEvent m, NotConnectedAbleTo b _ ) ->
            ( NotConnectedAbleTo b m, Cmd.none )

        ( ReceiveMousedOverEvent m, Connected b d ) ->
            ( Connected b m, Cmd.none )

        ( ReceiveMouseOutEvent m, Connected b d ) ->
            ( Connected b m, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        id =
            Element.htmlAttribute (Html.Attributes.id "delegationButton")

        ( buttonOnPress, buttonText, styles ) =
            case model of
                NotConnectedNotAbleTo ->
                    ( NoOp
                    , "No available wallet"
                    , [ Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                NotConnectedAbleTo w m ->
                    ( Connect w
                    , "Connect"
                    , [ Element.Background.color buttonHoverColor
                      , Element.Border.glow buttonHoverColor
                            (if m == True then
                                10

                             else
                                2
                            )
                      , id
                      ]
                    )

                ConnectionEstablished w ->
                    ( NoOp
                    , "Connection established"
                    , [ Element.Background.color buttonHoverColor
                      , Element.Border.glow buttonHoverColor 2
                      , Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                Connected w m ->
                    ( NoOp
                    , "Connection established"
                    , [ Element.Background.color buttonHoverColor
                      , Element.Border.glow buttonHoverColor 2
                      , Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                Connecting ->
                    ( NoOp
                    , "Connecting"
                    , [ Element.Background.color buttonHoverColor
                      , Element.Border.glow buttonHoverColor 2
                      , Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                NullState ->
                    ( NoOp
                    , "Connect"
                    , [ Element.Background.color buttonHoverColor
                      , Element.Border.glow buttonHoverColor 2
                      , id
                      ]
                    )
    in
    Element.layout []
        (Element.Input.button
            styles
            { onPress =
                Just
                    buttonOnPress
            , label =
                Element.text
                    buttonText
            }
        )


buttonHoverColor : Element.Color
buttonHoverColor =
    Element.rgb255 3 233 244


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveWalletConnection (\s -> ReceiveWalletConnected (decodeWallet s))
        , receiveMousedOverEvent ReceiveMousedOverEvent
        , receiveMouseOutEvent ReceiveMouseOutEvent
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


port receiveWalletConnection : (String -> msg) -> Sub msg


port receiveMousedOverEvent : (MouseOver -> msg) -> Sub msg


port receiveMouseOutEvent : (MouseOver -> msg) -> Sub msg
