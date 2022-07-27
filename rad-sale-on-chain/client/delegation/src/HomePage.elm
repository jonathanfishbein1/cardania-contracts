module HomePage exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events


type Msg
    = Connect
    | Disconnect


type State
    = NotConnected
      -- | Connecting
    | Connected


init : { state : State }
init =
    { state = NotConnected }


update : Msg -> { state : State } -> { state : State }
update msg model =
    case msg of
        Connect ->
            { state = Connected }

        Disconnect ->
            { state = NotConnected }


view : { state : State } -> Html.Html Msg
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
        [ Html.text (Debug.toString model.state) ]


main =
    Browser.sandbox { init = init, update = update, view = view }
