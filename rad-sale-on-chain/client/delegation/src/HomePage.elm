module HomePage exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events


type Msg
    = Connect


update : Msg -> number -> number
update msg model =
    case msg of
        Connect ->
            model + 1


view : Int -> Html.Html Msg
view model =
    Html.button [ Html.Events.onClick Connect ]
        [ Html.text (String.fromInt model) ]


main : Program () Int Msg
main =
    Browser.sandbox { init = 0, update = update, view = view }
