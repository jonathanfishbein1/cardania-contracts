port module RadSaleOnChain exposing
    ( BuyButtonState(..)
    , CloseButtonState(..)
    , Environment(..)
    , Model(..)
    , Msg(..)
    , StartButtonState(..)
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
import Library


decodeEnvironment : String -> Maybe Environment
decodeEnvironment env =
    case env of
        "development" ->
            Just Development

        "production" ->
            Just Production

        _ ->
            Nothing


type Environment
    = Development
    | Production


type Msg
    = Connect Library.SupportedWallet
    | NoOp
    | ReceiveWalletConnected (Maybe Library.SupportedWallet)
    | ReceiveConnectionEstablished
    | StartContract
    | ReceiveStartContractStatus Library.TransactionSuccessStatus
    | BuyContract
    | ReceiveBuyContractStatus Library.TransactionSuccessStatus
    | CloseContract
    | ReceiveCloseContractStatus Library.TransactionSuccessStatus


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
    | NotConnectedAbleTo Environment Library.SupportedWallet
    | Connecting Environment
    | ConnectionEstablished Environment Library.SupportedWallet
    | Connected Environment Library.SupportedWallet StartButtonState BuyButtonState CloseButtonState


init : ( String, String ) -> ( Model, Cmd Msg )
init ( supportedWallet, env ) =
    let
        wallet : Maybe Library.SupportedWallet
        wallet =
            Library.decodeWallet supportedWallet

        environment : Maybe Environment
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
        ( Connect w, NotConnectedAbleTo environment _ ) ->
            ( Connecting environment, connectWallet (Library.encodeWallet w) )

        ( ReceiveWalletConnected wallet, Connecting environment ) ->
            case wallet of
                Just w ->
                    update ReceiveConnectionEstablished (ConnectionEstablished environment w)

                Nothing ->
                    ( NotConnectedNotAbleTo environment, Cmd.none )

        ( ReceiveConnectionEstablished, ConnectionEstablished environment w ) ->
            ( Connected environment w NotStarted NotBought NotClosed, Cmd.none )

        ( StartContract, Connected environment b _ bs cs ) ->
            ( Connected environment b Starting bs cs, startContract () )

        ( ReceiveStartContractStatus result, Connected environment b _ bs cs ) ->
            let
                newModel : Model
                newModel =
                    if result then
                        Connected environment b Started bs cs

                    else
                        Connected environment b StartError bs cs
            in
            ( newModel
            , Cmd.none
            )

        ( BuyContract, Connected environment b d _ cs ) ->
            ( Connected environment b d Buying cs, buyContract () )

        ( ReceiveBuyContractStatus result, Connected environment b d _ cs ) ->
            let
                newModel : Model
                newModel =
                    if result then
                        Connected environment b d Bought cs

                    else
                        Connected environment b d BuyError cs
            in
            ( newModel
            , Cmd.none
            )

        ( CloseContract, Connected environment b d bs _ ) ->
            ( Connected environment b d bs Closing, closeContract () )

        ( ReceiveCloseContractStatus result, Connected environment b d bs _ ) ->
            let
                newModel : Model
                newModel =
                    if result then
                        Connected environment b d bs Closed

                    else
                        Connected environment b d bs CloseError
            in
            ( newModel
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        startId : Element.Attribute msg
        startId =
            Element.htmlAttribute (Html.Attributes.id "startButton")

        buyId : Element.Attribute msg
        buyId =
            Element.htmlAttribute (Html.Attributes.id "buyButton")

        closeId : Element.Attribute msg
        closeId =
            Element.htmlAttribute (Html.Attributes.id "closeButton")

        startButton : Element.Element Msg
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

                ConnectionEstablished Development _ ->
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

                Connected Development _ NotStarted _ _ ->
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

                Connected Development _ Starting _ _ ->
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

                Connected Development _ Started _ _ ->
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

                Connected Development _ StartError _ _ ->
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

        buyBtton : Element.Element Msg
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

                ConnectionEstablished _ _ ->
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

                Connected _ _ _ NotBought _ ->
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

                Connected _ _ _ Buying _ ->
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

                Connected _ _ _ Bought _ ->
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

                Connected _ _ _ BuyError _ ->
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

        closeButton : Element.Element Msg
        closeButton =
            case model of
                NotConnectedNotAbleTo Development ->
                    Element.Input.button
                        [ Element.htmlAttribute (Html.Attributes.disabled True)
                        , closeId
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
                        , closeId
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

                ConnectionEstablished Development _ ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , closeId
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
                        , closeId
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

                Connected Development _ _ _ NotClosed ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.mouseOver
                            [ Element.Border.glow buttonHoverColor
                                10
                            ]
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , closeId
                        ]
                        { onPress =
                            Just
                                CloseContract
                        , label =
                            Element.text
                                "Close"
                        }

                Connected Development _ _ _ Closing ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , closeId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Closinging"
                        }

                Connected Development _ _ _ Closed ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , closeId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Closed"
                        }

                Connected Development _ _ _ CloseError ->
                    Element.Input.button
                        [ Element.Background.color buttonHoverColor
                        , Element.htmlAttribute (Html.Attributes.disabled True)
                        , closeId
                        ]
                        { onPress =
                            Just
                                NoOp
                        , label =
                            Element.text
                                "Close Error"
                        }
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
        [ receiveWalletConnection (\s -> ReceiveWalletConnected (Library.decodeWallet s))
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


port startContract : () -> Cmd msg


port receiveStartContractStatus : (Bool -> msg) -> Sub msg


port buyContract : () -> Cmd msg


port receiveBuyContractStatus : (Bool -> msg) -> Sub msg


port closeContract : () -> Cmd msg


port receiveCloseContractStatus : (Bool -> msg) -> Sub msg
