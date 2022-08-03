port module Main exposing
    ( BuyButtonState(..)
    , CloseButtonState(..)
    , Environment(..)
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


decodeEnvironment : String -> Maybe Environment
decodeEnvironment env =
    case env of
        "development" ->
            Just Development

        "production" ->
            Just Production

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


type Environment
    = Development
    | Production


type Msg
    = Connect SupportedWallet
    | Disconnect SupportedWallet MouseOver
    | NoOp
    | ReceiveWalletConnected (Maybe SupportedWallet)
    | ReceiveConnectionEstablished
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
    = NotConnectedNotAbleTo Environment
    | NotConnectedAbleTo Environment SupportedWallet
    | Connecting Environment
    | ConnectionEstablished Environment SupportedWallet
    | Connected Environment SupportedWallet StartButtonState BuyButtonState CloseButtonState
    | NullState Environment


init : ( String, String ) -> ( Model, Cmd Msg )
init ( supportedWallet, env ) =
    let
        wallet =
            decodeWallet supportedWallet

        environment =
            decodeEnvironment env
    in
    ( case wallet of
        Just w ->
            NotConnectedAbleTo (Maybe.withDefault Production environment) w

        Nothing ->
            NotConnectedNotAbleTo (Maybe.withDefault Production environment)
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( msg, model ) of
        ( Connect w, NotConnectedAbleTo environment wallet ) ->
            ( Connecting environment, connectWallet (encodeWallet w) )

        ( Disconnect wallet m, Connected environment b d bs cs ) ->
            ( NotConnectedAbleTo environment wallet, Cmd.none )

        ( ReceiveWalletConnected wallet, Connecting environment ) ->
            case wallet of
                Just w ->
                    update ReceiveConnectionEstablished (ConnectionEstablished environment w)

                Nothing ->
                    ( NotConnectedNotAbleTo environment, Cmd.none )

        ( ReceiveConnectionEstablished, ConnectionEstablished environment w ) ->
            ( Connected environment w NotStarted NotBought NotClosed, Cmd.none )

        ( StartContract, Connected environment b d bs cs ) ->
            ( Connected environment b Starting bs cs, startContract () )

        ( ReceiveStartContractStatus result, Connected environment b d bs cs ) ->
            let
                newModel =
                    if result == True then
                        Connected environment b Started bs cs

                    else
                        Connected environment b StartError bs cs
            in
            ( newModel
            , Cmd.none
            )

        ( BuyContract, Connected environment b d bs cs ) ->
            ( Connected environment b d Buying cs, buyContract () )

        ( ReceiveBuyContractStatus result, Connected environment b d bs cs ) ->
            let
                newModel =
                    if result == True then
                        Connected environment b d Bought cs

                    else
                        Connected environment b d BuyError cs
            in
            ( newModel
            , Cmd.none
            )

        ( CloseContract, Connected environment b d bs cs ) ->
            ( Connected environment b d bs Closing, closeContract () )

        ( ReceiveCloseContractStatus result, Connected environment b d bs cs ) ->
            let
                newModel =
                    if result == True then
                        Connected environment b d bs Closed

                    else
                        Connected environment b d bs CloseError
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

        startButton =
            case model of
                NotConnectedNotAbleTo Development ->
                    Element.Input.button
                        [ Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "No available wallet"
                        }

                NotConnectedNotAbleTo Production ->
                    Element.none

                NotConnectedAbleTo Development w ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.mouseOver
                            [ Element.Border.glow buttonHoverColor
                                10
                            ]
                        , startId
                        ]
                        { onPress =
                            Just
                                (Connect w)
                        , label =
                            Element.text
                                "Connect"
                        }

                NotConnectedAbleTo Production _ ->
                    Element.none

                ConnectionEstablished Development w ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Connection established"
                        }

                ConnectionEstablished Production _ ->
                    Element.none

                Connecting Development ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Connecting"
                        }

                Connecting Production ->
                    Element.none

                Connected Development w NotStarted _ cs ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.mouseOver
                            [ Element.Border.glow buttonHoverColor
                                10
                            ]
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                        { onPress =
                            Just
                                StartContract
                        , label =
                            Element.text
                                "Start"
                        }

                Connected Production _ _ _ _ ->
                    Element.none

                Connected Development w Starting _ cs ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Starting"
                        }

                Connected Development w Started _ cs ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Started"
                        }

                Connected Development w StartError _ cs ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , startId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Start Error"
                        }

                NullState Development ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , startId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Connect"
                        }

                NullState Production ->
                    Element.none

        buyBtton =
            case model of
                NotConnectedNotAbleTo _ ->
                    Element.Input.button
                        [ Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "No available wallet"
                        }

                NotConnectedAbleTo _ w ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.mouseOver
                            [ Element.Border.glow buttonHoverColor
                                10
                            ]
                        , buyId
                        ]
                        { onPress =
                            Just
                                (Connect w)
                        , label =
                            Element.text
                                "Connect"
                        }

                ConnectionEstablished _ w ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Connection established"
                        }

                Connecting _ ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Connecting"
                        }

                Connected _ w s NotBought cs ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.mouseOver
                            [ Element.Border.glow buttonHoverColor
                                10
                            ]
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                BuyContract
                        , label =
                            Element.text
                                "Buy"
                        }

                Connected _ w s Buying cs ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Buying"
                        }

                Connected _ w s Bought cs ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Bought"
                        }

                Connected _ w s BuyError cs ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Buy Error"
                        }

                NullState _ ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Connect"
                        }

        closeButton =
            case model of
                NotConnectedNotAbleTo Development ->
                    Element.Input.button
                        [ Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "No available wallet"
                        }

                NotConnectedNotAbleTo Production ->
                    Element.none

                NotConnectedAbleTo Development w ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.mouseOver
                            [ Element.Border.glow buttonHoverColor
                                10
                            ]
                        , buyId
                        ]
                        { onPress =
                            Just
                                (Connect w)
                        , label =
                            Element.text
                                "Connect"
                        }

                NotConnectedAbleTo Production _ ->
                    Element.none

                ConnectionEstablished Development w ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Connection established"
                        }

                ConnectionEstablished Production _ ->
                    Element.none

                Connecting Development ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Connecting"
                        }

                Connecting Production ->
                    Element.none

                Connected Production _ _ _ _ ->
                    Element.none

                Connected Development w s _ NotClosed ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.mouseOver
                            [ Element.Border.glow buttonHoverColor
                                10
                            ]
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                CloseContract
                        , label =
                            Element.text
                                "Close"
                        }

                Connected Development w s _ Closing ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Closinging"
                        }

                Connected Development w s _ Closed ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Closed"
                        }

                Connected Development w s _ CloseError ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Close Error"
                        }

                NullState Development ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , buyId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Connect"
                        }

                NullState Production ->
                    Element.none
    in
    Element.layout []
        (Element.column []
            [ startButton
            , buyBtton
            , closeButton
            ]
        )


buttonHoverColor : Element.Color
buttonHoverColor =
    Element.rgb255 3 233 244


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveWalletConnection (\s -> ReceiveWalletConnected (decodeWallet s))
        , receiveStartContractStatus ReceiveStartContractStatus
        , receiveBuyContractStatus ReceiveBuyContractStatus
        , receiveCloseContractStatus ReceiveCloseContractStatus
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


port receiveMouseOverBuyButtonEvent : (MouseOver -> msg) -> Sub msg


port receiveMouseOutBuyButtonEvent : (MouseOver -> msg) -> Sub msg


port startContract : () -> Cmd msg


port receiveStartContractStatus : (Bool -> msg) -> Sub msg


port buyContract : () -> Cmd msg


port receiveBuyContractStatus : (Bool -> msg) -> Sub msg


port closeContract : () -> Cmd msg


port receiveCloseContractStatus : (Bool -> msg) -> Sub msg
