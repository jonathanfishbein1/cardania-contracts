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
    )

import Browser
import Element
import Element.Input
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
    | ReceiveWalletConnected (Maybe SupportedWallet)
    | GetAccountStatus
    | ReceiveAccountStatus (Result Json.Decode.Error Account)
    | RegisterAndDelegateToSumn Account
    | ReceiveRegisterAndDelegateStatus Bool
    | DelegateToSumn
    | UndelegateFromSumn
    | ReceiveUndelegateStatus Bool


type DelegationStatus
    = NotDelegating
    | DelegatingToSumn
    | DelegatingToOther


type Model
    = NotConnectedNotAbleTo
    | NotConnectedAbleTo String SupportedWallet
    | Connecting String
    | GettingAcountStatus String SupportedWallet
    | ConnectionEstablished String SupportedWallet
    | Connected String SupportedWallet Account DelegationStatus
    | RegisteringAndDelegating String SupportedWallet Account
    | Delegating String SupportedWallet Account
    | Undelegating String SupportedWallet Account DelegationStatus
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


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( msg, model ) of
        ( Connect sumnPoolId w, NotConnectedAbleTo p wallet ) ->
            ( Connecting sumnPoolId, connectWallet (encodeWallet w) )

        ( Disconnect sumnPoolId wallet, _ ) ->
            ( NotConnectedAbleTo sumnPoolId wallet, Cmd.none )

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

                Err e ->
                    NullState
            , Cmd.none
            )

        ( RegisterAndDelegateToSumn a, Connected p w account NotDelegating ) ->
            ( RegisteringAndDelegating p w account, registerAndDelegateToSumn account.stake_address )

        ( ReceiveRegisterAndDelegateStatus result, RegisteringAndDelegating p w account ) ->
            let
                newModel =
                    if result == True then
                        Connected p w account DelegatingToSumn

                    else
                        Connected p w account NotDelegating
            in
            ( newModel
            , Cmd.none
            )

        ( DelegateToSumn, Connected p w account DelegatingToOther ) ->
            ( model, Cmd.none )

        ( UndelegateFromSumn, Connected p w account DelegatingToSumn ) ->
            ( Undelegating p w account DelegatingToSumn, undelegate account.stake_address )

        ( ReceiveUndelegateStatus result, Undelegating p w account _ ) ->
            let
                newModel =
                    if result == True then
                        Connected p w account NotDelegating

                    else
                        Connected p w account DelegatingToSumn
            in
            ( newModel
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        ( buttonOnPress, buttonText ) =
            case model of
                NotConnectedNotAbleTo ->
                    ( NoOp, "No available wallet" )

                NotConnectedAbleTo sumnPoolId w ->
                    ( Connect sumnPoolId w, "Connect" )

                ConnectionEstablished sumnPoolId w ->
                    ( NoOp, "Connection established" )

                GettingAcountStatus _ _ ->
                    ( NoOp, "Getting account status" )

                Connected _ w acc d ->
                    case d of
                        NotDelegating ->
                            ( RegisterAndDelegateToSumn acc, "Delegate" )

                        DelegatingToOther ->
                            ( DelegateToSumn, "Delegate" )

                        DelegatingToSumn ->
                            ( UndelegateFromSumn, "Undelegate" )

                Connecting _ ->
                    ( NoOp, "Connecting" )

                RegisteringAndDelegating _ _ _ ->
                    ( NoOp, "Registering and Delegating" )

                Delegating _ _ _ ->
                    ( NoOp
                    , "Delegating"
                    )

                Undelegating _ _ _ _ ->
                    ( NoOp, "Undelegating" )

                NullState ->
                    ( NoOp, "Connect" )
    in
    Element.layout []
        (Element.Input.button
            []
            { onPress =
                Just
                    buttonOnPress
            , label =
                Element.text
                    buttonText
            }
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveWalletConnection (\s -> ReceiveWalletConnected (decodeWallet s))
        , receiveAccountStatus (\s -> ReceiveAccountStatus (Json.Decode.decodeString decodeAccount s))
        , receiveRegisterAndDelegateStatus ReceiveRegisterAndDelegateStatus
        , receiveUndelegateStatus ReceiveUndelegateStatus
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


port getAccountStatus : () -> Cmd msg


port receiveAccountStatus : (String -> msg) -> Sub msg


port registerAndDelegateToSumn : String -> Cmd msg


port receiveRegisterAndDelegateStatus : (Bool -> msg) -> Sub msg


port undelegate : String -> Cmd msg


port receiveUndelegateStatus : (Bool -> msg) -> Sub msg
