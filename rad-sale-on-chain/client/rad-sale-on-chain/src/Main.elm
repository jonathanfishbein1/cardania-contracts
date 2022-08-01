port module Main exposing
    ( Account
    , DelegationStatus(..)
    , Model(..)
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
import Html.Events
import Json.Decode
import Json.Decode.Pipeline


type alias PoolId =
    String


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
    , pool_id : PoolId
    , active : Bool
    }


type SupportedWallet
    = Nami
    | Eternl
    | Flint


type Msg
    = Connect PoolId SupportedWallet
    | Disconnect PoolId SupportedWallet MouseOver
    | NoOp
    | ReceiveWalletConnected (Maybe SupportedWallet)
    | GetAccountStatus
    | ReceiveAccountStatus (Result Json.Decode.Error Account)
    | RegisterAndDelegateToSumn Account
    | ReceiveRegisterAndDelegateStatus TransactionSuccessStatus
    | DelegateToSumn
    | ReceiveDelegateToSumnStatus TransactionSuccessStatus
    | UndelegateFromSumn
    | ReceiveUndelegateStatus TransactionSuccessStatus
    | ReceiveMousedOverEvent MouseOver
    | ReceiveMouseOutEvent MouseOver


type DelegationStatus
    = NotDelegating
    | DelegatingToSumn
    | DelegatingToOther


type Model
    = NotConnectedNotAbleTo
    | NotConnectedAbleTo PoolId SupportedWallet MouseOver
    | Connecting PoolId
    | GettingAcountStatus PoolId SupportedWallet
    | ConnectionEstablished PoolId SupportedWallet
    | Connected PoolId SupportedWallet Account DelegationStatus MouseOver
    | RegisteringAndDelegating PoolId SupportedWallet Account
    | Delegating PoolId SupportedWallet Account
    | Undelegating PoolId SupportedWallet Account
    | NullState


init : ( String, PoolId ) -> ( Model, Cmd Msg )
init ( supportedWallet, sumnPoolId ) =
    let
        wallet =
            decodeWallet supportedWallet
    in
    ( case wallet of
        Just w ->
            NotConnectedAbleTo sumnPoolId w False

        Nothing ->
            NotConnectedNotAbleTo
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( msg, model ) of
        ( Connect sumnPoolId w, NotConnectedAbleTo p wallet _ ) ->
            ( Connecting sumnPoolId, connectWallet (encodeWallet w) )

        ( Disconnect sumnPoolId wallet m, _ ) ->
            ( NotConnectedAbleTo sumnPoolId wallet m, Cmd.none )

        ( ReceiveWalletConnected wallet, Connecting sumnPoolId ) ->
            case wallet of
                Just w ->
                    let
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

                         else if acc.active == True && acc.pool_id /= sumnPoolId then
                            DelegatingToOther

                         else if acc.active && acc.pool_id == sumnPoolId then
                            DelegatingToSumn

                         else
                            NotDelegating
                        )
                        False

                Err e ->
                    NullState
            , Cmd.none
            )

        ( RegisterAndDelegateToSumn a, Connected p w account NotDelegating _ ) ->
            ( RegisteringAndDelegating p w account, registerAndDelegateToSumn account.stake_address )

        ( ReceiveRegisterAndDelegateStatus result, RegisteringAndDelegating p w account ) ->
            let
                newModel =
                    if result == True then
                        Connected p w account DelegatingToSumn False

                    else
                        Connected p w account NotDelegating False
            in
            ( newModel
            , Cmd.none
            )

        ( DelegateToSumn, Connected p w account DelegatingToOther _ ) ->
            ( Delegating p w account, delegateToSumn account.stake_address )

        ( ReceiveDelegateToSumnStatus result, Delegating p w account ) ->
            let
                newModel =
                    if result == True then
                        Connected p w account DelegatingToSumn False

                    else
                        Connected p w account NotDelegating False
            in
            ( newModel
            , Cmd.none
            )

        ( UndelegateFromSumn, Connected p w account DelegatingToSumn _ ) ->
            ( Undelegating p w account, undelegate account.stake_address )

        ( ReceiveUndelegateStatus result, Undelegating p w account ) ->
            let
                newModel =
                    if result == True then
                        Connected p w account NotDelegating False

                    else
                        Connected p w account DelegatingToSumn False
            in
            ( newModel
            , Cmd.none
            )

        ( ReceiveMousedOverEvent m, NotConnectedAbleTo a b _ ) ->
            ( NotConnectedAbleTo a b m, Cmd.none )

        ( ReceiveMouseOutEvent m, NotConnectedAbleTo a b _ ) ->
            ( NotConnectedAbleTo a b m, Cmd.none )

        ( ReceiveMousedOverEvent m, Connected a b c d _ ) ->
            ( Connected a b c d m, Cmd.none )

        ( ReceiveMouseOutEvent m, Connected a b c d _ ) ->
            ( Connected a b c d m, Cmd.none )

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

                NotConnectedAbleTo sumnPoolId w m ->
                    ( Connect sumnPoolId w
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

                ConnectionEstablished sumnPoolId w ->
                    ( NoOp
                    , "Connection established"
                    , [ Element.Background.color buttonHoverColor
                      , Element.Border.glow buttonHoverColor 2
                      , Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                GettingAcountStatus _ _ ->
                    ( NoOp
                    , "Getting account status"
                    , [ Element.Background.color buttonHoverColor
                      , Element.Border.glow buttonHoverColor 2
                      , Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                Connected _ w acc d m ->
                    case d of
                        NotDelegating ->
                            ( RegisterAndDelegateToSumn acc
                            , "Register and Delegate"
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

                        DelegatingToOther ->
                            ( DelegateToSumn
                            , "Delegate"
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
                      , Element.Border.glow buttonHoverColor 2
                      , Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                RegisteringAndDelegating _ _ _ ->
                    ( NoOp
                    , "Registering and Delegating"
                    , [ Element.Background.color buttonHoverColor
                      , Element.Border.glow buttonHoverColor 2
                      , Element.htmlAttribute (Html.Attributes.disabled True)
                      , id
                      ]
                    )

                Delegating _ _ _ ->
                    ( NoOp
                    , "Delegating"
                    , [ Element.Background.color buttonHoverColor
                      , Element.Border.glow buttonHoverColor 2
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
        , receiveAccountStatus (\s -> ReceiveAccountStatus (Json.Decode.decodeString decodeAccount s))
        , receiveRegisterAndDelegateStatus ReceiveRegisterAndDelegateStatus
        , receiveDelegateStatus ReceiveDelegateToSumnStatus
        , receiveUndelegateStatus ReceiveUndelegateStatus
        , receiveMousedOverEvent ReceiveMousedOverEvent
        , receiveMouseOutEvent ReceiveMouseOutEvent
        ]


main : Program ( String, PoolId ) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port connectWallet : String -> Cmd msg


port receiveWalletConnection : (String -> msg) -> Sub msg


port getAccountStatus : () -> Cmd msg


port receiveAccountStatus : (String -> msg) -> Sub msg


port registerAndDelegateToSumn : String -> Cmd msg


port receiveRegisterAndDelegateStatus : (TransactionSuccessStatus -> msg) -> Sub msg


port delegateToSumn : String -> Cmd msg


port receiveDelegateStatus : (TransactionSuccessStatus -> msg) -> Sub msg


port undelegate : String -> Cmd msg


port receiveUndelegateStatus : (TransactionSuccessStatus -> msg) -> Sub msg


port receiveMousedOverEvent : (MouseOver -> msg) -> Sub msg


port receiveMouseOutEvent : (MouseOver -> msg) -> Sub msg
