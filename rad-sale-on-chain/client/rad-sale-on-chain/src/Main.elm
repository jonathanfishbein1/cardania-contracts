port module Main exposing
    ( BuyButtonState(..)
    , CloseButtonState(..)
    , Model(..)
    , Msg(..)
    , StartButtonState(..)
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
    | ReceiveConnectionEstablished
    | ReceiveMouseStartButtonEvent MouseOver
    | ReceiveMouseBuyButtonEvent MouseOver
    | StartContract
    | ReceiveStartContractStatus TransactionSuccessStatus
    | BuyContract
    | ReceiveBuyContractStatus TransactionSuccessStatus
    | CloseContract
    | ReceiveCloseContractStatus TransactionSuccessStatus


type StartButtonState
    = NotStarted
    | Starting
    | Started
    | StartError


type BuyButtonState
    = NotBought
    | Buying
    | Bought
    | BuyError


type CloseButtonState
    = NotClosed
    | Closing
    | Closed
    | CloseError


type Model
    = NotConnectedNotAbleTo
    | NotConnectedAbleTo SupportedWallet MouseOver
    | Connecting
    | ConnectionEstablished SupportedWallet
    | Connected SupportedWallet MouseOver StartButtonState BuyButtonState CloseButtonState
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
                    update ReceiveConnectionEstablished (ConnectionEstablished w)

                Nothing ->
                    ( NotConnectedNotAbleTo, Cmd.none )

        ( ReceiveConnectionEstablished, ConnectionEstablished w ) ->
            ( Connected w False NotStarted NotBought NotClosed, Cmd.none )

        ( ReceiveMouseStartButtonEvent m, NotConnectedAbleTo b _ ) ->
            ( NotConnectedAbleTo b m, Cmd.none )

        ( ReceiveMouseBuyButtonEvent m, NotConnectedAbleTo b _ ) ->
            ( NotConnectedAbleTo b m, Cmd.none )

        ( ReceiveMouseStartButtonEvent m, Connected b d ss bs cs ) ->
            ( Connected b m ss bs cs, Cmd.none )

        ( ReceiveMouseBuyButtonEvent m, Connected b d ss bs cs ) ->
            ( Connected b m ss bs cs, Cmd.none )

        ( StartContract, Connected b d ss bs cs ) ->
            ( Connected b d Starting bs cs, startContract () )

        ( ReceiveStartContractStatus result, Connected b d ss bs cs ) ->
            let
                newModel =
                    if result == True then
                        Connected b d Started bs cs

                    else
                        Connected b d StartError bs cs
            in
            ( newModel
            , Cmd.none
            )

        ( BuyContract, Connected b d ss bs cs ) ->
            ( Connected b d ss Buying cs, buyContract () )

        ( ReceiveBuyContractStatus result, Connected b d ss bs cs ) ->
            let
                newModel =
                    if result == True then
                        Connected b d ss Bought cs

                    else
                        Connected b d ss BuyError cs
            in
            ( newModel
            , Cmd.none
            )

        ( CloseContract, Connected b d ss bs cs ) ->
            ( Connected b d ss bs Closing, closeContract () )

        ( ReceiveCloseContractStatus result, Connected b d ss bs cs ) ->
            let
                newModel =
                    if result == True then
                        Connected b d ss bs Closed

                    else
                        Connected b d ss bs CloseError
            in
            ( newModel
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        startId =
            Element.htmlAttribute (Html.Attributes.id "startButton")

        buyId =
            Element.htmlAttribute (Html.Attributes.id "buyButton")

        closeId =
            Element.htmlAttribute (Html.Attributes.id "closeButton")

        startButtonProperties =
            case model of
                NotConnectedNotAbleTo ->
                    { msg = NoOp
                    , text = "No available wallet"
                    , attributes =
                        [ Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                NotConnectedAbleTo w m ->
                    { msg = Connect w
                    , text = "Connect"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor
                            (if m == True then
                                10

                             else
                                2
                            )
                        , startId
                        ]
                    }

                ConnectionEstablished w ->
                    { msg = NoOp
                    , text = "Connection established"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                Connecting ->
                    { msg = NoOp
                    , text = "Connecting"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                Connected w m NotStarted _ cs ->
                    { msg = StartContract
                    , text = "Start"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                Connected w m Starting _ cs ->
                    { msg = NoOp
                    , text = "Starting"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                Connected w m Started _ cs ->
                    { msg = NoOp
                    , text = "Started"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                Connected w m StartError _ cs ->
                    { msg = NoOp
                    , text = "Start Error"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                NullState ->
                    { msg = NoOp
                    , text = "Connect"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , startId
                        ]
                    }

        buyBttonProperties =
            case model of
                NotConnectedNotAbleTo ->
                    { msg = NoOp
                    , text = "No available wallet"
                    , attributes =
                        [ Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                    }

                NotConnectedAbleTo w m ->
                    { msg = Connect w
                    , text = "Connect"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor
                            (if m == True then
                                10

                             else
                                2
                            )
                        , buyId
                        ]
                    }

                ConnectionEstablished w ->
                    { msg = NoOp
                    , text = "Connection established"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                    }

                Connecting ->
                    { msg = NoOp
                    , text = "Connecting"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                    }

                Connected w m s NotBought cs ->
                    { msg = BuyContract
                    , text = "Buy"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                Connected w m s Buying cs ->
                    { msg = NoOp
                    , text = "Buying"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                Connected w m s Bought cs ->
                    { msg = NoOp
                    , text = "Bought"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                Connected w m s BuyError cs ->
                    { msg = NoOp
                    , text = "Buy Error"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                NullState ->
                    { msg = NoOp
                    , text = "Connect"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , buyId
                        ]
                    }

        closeButtonProperties =
            case model of
                NotConnectedNotAbleTo ->
                    { msg = NoOp
                    , text = "No available wallet"
                    , attributes =
                        [ Element.htmlAttribute (Html.Attributes.disabled True)
                        , closeId
                        ]
                    }

                NotConnectedAbleTo w m ->
                    { msg = Connect w
                    , text = "Connect"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor
                            (if m == True then
                                10

                             else
                                2
                            )
                        , closeId
                        ]
                    }

                ConnectionEstablished w ->
                    { msg = NoOp
                    , text = "Connection established"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , closeId
                        ]
                    }

                Connecting ->
                    { msg = NoOp
                    , text = "Connecting"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , closeId
                        ]
                    }

                Connected w m _ _ NotClosed ->
                    { msg = CloseContract
                    , text = "Close"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , closeId
                        ]
                    }

                Connected w m _ _ Closing ->
                    { msg = NoOp
                    , text = "Closing"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                Connected w m _ _ Closed ->
                    { msg = NoOp
                    , text = "Closed"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                Connected w m _ _ CloseError ->
                    { msg = NoOp
                    , text = "Close Error"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                    }

                NullState ->
                    { msg = NoOp
                    , text = "Connect"
                    , attributes =
                        [ Element.Background.color buttonHoverColor
                        , Element.Border.glow buttonHoverColor 2
                        , startId
                        ]
                    }
    in
    Element.layout []
        (Element.column []
            [ Element.Input.button
                startButtonProperties.attributes
                { onPress =
                    Just
                        startButtonProperties.msg
                , label =
                    Element.text
                        startButtonProperties.text
                }
            , Element.Input.button
                buyBttonProperties.attributes
                { onPress =
                    Just
                        buyBttonProperties.msg
                , label =
                    Element.text
                        buyBttonProperties.text
                }
            , Element.Input.button
                closeButtonProperties.attributes
                { onPress =
                    Just
                        closeButtonProperties.msg
                , label =
                    Element.text
                        closeButtonProperties.text
                }
            ]
        )


buttonHoverColor : Element.Color
buttonHoverColor =
    Element.rgb255 3 233 244


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveWalletConnection (\s -> ReceiveWalletConnected (decodeWallet s))
        , receiveMouseStartButtonEvent ReceiveMouseStartButtonEvent
        , receiveMouseBuyButtonEvent ReceiveMouseBuyButtonEvent
        , receiveStartContractStatus ReceiveStartContractStatus
        , receiveBuyContractStatus ReceiveBuyContractStatus
        , receiveCloseContractStatus ReceiveCloseContractStatus
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


port receiveMouseStartButtonEvent : (MouseOver -> msg) -> Sub msg


port receiveMouseBuyButtonEvent : (MouseOver -> msg) -> Sub msg


port receiveMouseOverBuyButtonEvent : (MouseOver -> msg) -> Sub msg


port receiveMouseOutBuyButtonEvent : (MouseOver -> msg) -> Sub msg


port startContract : () -> Cmd msg


port receiveStartContractStatus : (Bool -> msg) -> Sub msg


port buyContract : () -> Cmd msg


port receiveBuyContractStatus : (Bool -> msg) -> Sub msg


port closeContract : () -> Cmd msg


port receiveCloseContractStatus : (Bool -> msg) -> Sub msg
