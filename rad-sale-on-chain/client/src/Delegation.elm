port module Delegation exposing
    ( Account
    , DelegationStatus(..)
    , Model(..)
    , Msg(..)
    , PoolId
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
import Library


type alias PoolId =
    String


decodeAccount : Json.Decode.Decoder Account
decodeAccount =
    Json.Decode.succeed Account
        |> Json.Decode.Pipeline.required "stake_address" Json.Decode.string
        |> Json.Decode.Pipeline.optional "pool_id" Json.Decode.string ""
        |> Json.Decode.Pipeline.required "active" Json.Decode.bool


type alias Account =
    { stake_address : String
    , pool_id : PoolId
    , active : Bool
    }


type Msg
    = Connect PoolId Library.SupportedWallet
    | NoOp
    | ReceiveWalletConnected (Maybe Library.SupportedWallet)
    | GetAccountStatus
    | ReceiveAccountStatus (Result Json.Decode.Error Account)
    | RegisterAndDelegateToSumn
    | ReceiveRegisterAndDelegateStatus Library.TransactionSuccessStatus
    | DelegateToSumn
    | ReceiveDelegateToSumnStatus Library.TransactionSuccessStatus
    | UndelegateFromSumn
    | ReceiveUndelegateStatus Library.TransactionSuccessStatus


type DelegationStatus
    = NotDelegating
    | DelegatingToSumn
    | DelegatingToOther


type Model
    = NotConnectedNotAbleTo
    | NotConnectedAbleTo PoolId Library.SupportedWallet
    | Connecting PoolId
    | GettingAcountStatus PoolId Library.SupportedWallet
    | ConnectionEstablished PoolId Library.SupportedWallet
    | Connected PoolId Library.SupportedWallet Account DelegationStatus
    | RegisteringAndDelegating PoolId Library.SupportedWallet Account
    | Delegating PoolId Library.SupportedWallet Account
    | Undelegating PoolId Library.SupportedWallet Account
    | NullState


init : ( String, PoolId ) -> ( Model, Cmd Msg )
init ( supportedWallet, sumnPoolId ) =
    let
        wallet : Maybe Library.SupportedWallet
        wallet =
            Library.decodeWallet supportedWallet
    in
    ( case wallet of
        Just w ->
            NotConnectedAbleTo sumnPoolId w

        Nothing ->
            NotConnectedNotAbleTo
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( msg, model ) of
        ( Connect sumnPoolId w, NotConnectedAbleTo _ _ ) ->
            ( Connecting sumnPoolId, connectWalletDelegation (Library.encodeWallet w) )

        ( ReceiveWalletConnected wallet, Connecting sumnPoolId ) ->
            case wallet of
                Just w ->
                    let
                        newModel : Model
                        newModel =
                            ConnectionEstablished sumnPoolId w
                    in
                    update GetAccountStatus newModel

                Nothing ->
                    ( NotConnectedNotAbleTo, Cmd.none )

        ( GetAccountStatus, ConnectionEstablished sumnPoolId w ) ->
            ( GettingAcountStatus sumnPoolId w, getAccountStatus () )

        ( ReceiveAccountStatus account, GettingAcountStatus sumnPoolId wallet ) ->
            ( case account of
                Ok acc ->
                    Connected sumnPoolId
                        wallet
                        acc
                        (if acc.active == False then
                            NotDelegating

                         else if acc.active && acc.pool_id /= sumnPoolId then
                            DelegatingToOther

                         else if acc.active && acc.pool_id == sumnPoolId then
                            DelegatingToSumn

                         else
                            NotDelegating
                        )

                Err _ ->
                    NullState
            , Cmd.none
            )

        ( RegisterAndDelegateToSumn, Connected p w account NotDelegating ) ->
            ( RegisteringAndDelegating p w account, registerAndDelegateToSumn account.stake_address )

        ( ReceiveRegisterAndDelegateStatus result, RegisteringAndDelegating p w account ) ->
            let
                newModel : Model
                newModel =
                    if result then
                        Connected p w account DelegatingToSumn

                    else
                        Connected p w account NotDelegating
            in
            ( newModel
            , Cmd.none
            )

        ( DelegateToSumn, Connected p w account DelegatingToOther ) ->
            ( Delegating p w account, delegateToSumn account.stake_address )

        ( ReceiveDelegateToSumnStatus result, Delegating p w account ) ->
            let
                newModel : Model
                newModel =
                    if result then
                        Connected p w account DelegatingToSumn

                    else
                        Connected p w account NotDelegating
            in
            ( newModel
            , Cmd.none
            )

        ( UndelegateFromSumn, Connected p w account DelegatingToSumn ) ->
            ( Undelegating p w account, undelegate account.stake_address )

        ( ReceiveUndelegateStatus result, Undelegating p w account ) ->
            let
                newModel : Model
                newModel =
                    if result then
                        Connected p w account NotDelegating

                    else
                        Connected p w account DelegatingToSumn
            in
            ( newModel
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        id : Element.Attribute msg
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

                NotConnectedAbleTo sumnPoolId w ->
                    ( Connect sumnPoolId w
                    , "Connect"
                    , [ Element.Background.color buttonHoverColor
                      , Element.mouseOver
                            [ Element.Border.glow buttonHoverColor
                                10
                            ]
                      , id
                      ]
                    )

                ConnectionEstablished _ _ ->
                    ( NoOp
                    , "Connection established"
                    , [ Element.Background.color buttonHoverColor
                      , Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                GettingAcountStatus _ _ ->
                    ( NoOp
                    , "Getting account status"
                    , [ Element.Background.color buttonHoverColor
                      , Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                Connected _ _ _ d ->
                    case d of
                        NotDelegating ->
                            ( RegisterAndDelegateToSumn
                            , "Register and Delegate"
                            , [ Element.Background.color buttonHoverColor
                              , Element.mouseOver
                                    [ Element.Border.glow buttonHoverColor
                                        10
                                    ]
                              , id
                              ]
                            )

                        DelegatingToOther ->
                            ( DelegateToSumn
                            , "Delegate"
                            , [ Element.Background.color buttonHoverColor
                              , Element.mouseOver
                                    [ Element.Border.glow buttonHoverColor
                                        10
                                    ]
                              , id
                              ]
                            )

                        DelegatingToSumn ->
                            ( UndelegateFromSumn
                            , "Undelegate"
                            , [ Element.Background.color buttonHoverColor
                              , id
                              ]
                            )

                Connecting _ ->
                    ( NoOp
                    , "Connecting"
                    , [ Element.Background.color buttonHoverColor
                      , Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                RegisteringAndDelegating _ _ _ ->
                    ( NoOp
                    , "Registering and Delegating"
                    , [ Element.Background.color buttonHoverColor
                      , Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                Delegating _ _ _ ->
                    ( NoOp
                    , "Delegating"
                    , [ Element.Background.color buttonHoverColor
                      , Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                Undelegating _ _ _ ->
                    ( NoOp
                    , "Undelegating"
                    , [ Element.Background.color buttonHoverColor
                      , Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                NullState ->
                    ( NoOp
                    , "Connect"
                    , [ Element.Background.color buttonHoverColor
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
        [ receiveWalletConnectionDelegation (\s -> ReceiveWalletConnected (Library.decodeWallet s))
        , receiveAccountStatus (\s -> ReceiveAccountStatus (Json.Decode.decodeString decodeAccount s))
        , receiveRegisterAndDelegateStatus ReceiveRegisterAndDelegateStatus
        , receiveDelegateStatus ReceiveDelegateToSumnStatus
        , receiveUndelegateStatus ReceiveUndelegateStatus
        ]


main : Program ( String, PoolId ) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port connectWalletDelegation : String -> Cmd msg


port receiveWalletConnectionDelegation : (String -> msg) -> Sub msg


port getAccountStatus : () -> Cmd msg


port receiveAccountStatus : (String -> msg) -> Sub msg


port registerAndDelegateToSumn : String -> Cmd msg


port receiveRegisterAndDelegateStatus : (Library.TransactionSuccessStatus -> msg) -> Sub msg


port delegateToSumn : String -> Cmd msg


port receiveDelegateStatus : (Library.TransactionSuccessStatus -> msg) -> Sub msg


port undelegate : String -> Cmd msg


port receiveUndelegateStatus : (Library.TransactionSuccessStatus -> msg) -> Sub msg
