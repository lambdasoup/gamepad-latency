module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Dict exposing (Dict)
import Gamepad
import Html exposing (Html, div)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Posix)


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \() -> init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { pressed : Bool
    , phase : Phase
    , time : Posix
    , pushes : List Posix
    , targets : List Posix
    , result : List Duration
    , gamepads : Dict Int Gamepad.Gamepad
    }


init : ( Model, Cmd Msg )
init =
    ( { pressed = False
      , phase = 0.0
      , time = Time.millisToPosix 0
      , targets = []
      , pushes = []
      , gamepads = Dict.empty
      , result = []
      }
    , Cmd.none
    )


type Msg
    = Frame Posix
    | OnInput Gamepad.Input
    | OnConnectionChanged Gamepad.ConnectionChanged


type ButtonAction
    = None
    | Up
    | Down


type alias Phase =
    Float


type Duration
    = Duration Int


phaseMs : Int
phaseMs =
    1000


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame now ->
            let
                millis =
                    Time.posixToMillis now

                mod =
                    modBy phaseMs millis

                phase =
                    toFloat mod / toFloat phaseMs

                target =
                    millis // phaseMs |> (*) 1000 |> Time.millisToPosix

                allTargets =
                    case List.reverse model.targets of
                        last :: _ ->
                            if last /= target then
                                List.append model.targets [ target ]

                            else
                                model.targets

                        [] ->
                            [ target ]

                targets =
                    List.filter (\t -> within10secsBefore now t) allTargets

                pushes =
                    List.filter (\t -> within10secsBefore now t) model.pushes

                pushResult =
                    result pushes
            in
            ( { model
                | phase = phase
                , targets = targets
                , time = now
                , pushes = pushes
                , result = pushResult
              }
            , Cmd.none
            )

        OnInput input ->
            let
                pushes =
                    case buttonAction model input of
                        Down ->
                            List.append model.pushes [ input.time ]

                        _ ->
                            model.pushes
            in
            ( { model
                | pressed = input.pressed
                , pushes = pushes
              }
            , Cmd.none
            )

        OnConnectionChanged connectionChanged ->
            case connectionChanged of
                Gamepad.Connected gamepad ->
                    ( { model | gamepads = Dict.insert gamepad.index gamepad model.gamepads }
                    , Cmd.none
                    )

                Gamepad.Disconnected gamepad ->
                    ( { model | gamepads = Dict.remove gamepad.index model.gamepads }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Gamepad.onInput OnInput
        , Gamepad.onConnectionChanged OnConnectionChanged
        , onAnimationFrame Frame
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ id "info" ]
            [ Html.text ((model.pushes |> List.length |> String.fromInt) ++ " pushes")
            ]
        , div [ id "result" ]
            [ model.result |> mean |> viewDuration ]
        , viewGamepads model.gamepads
        , viewCircle model.pressed model.phase
        , viewGraph model.result
        ]


viewDuration : Duration -> Html Msg
viewDuration (Duration x) =
    String.fromInt x ++ " ms" |> Html.text


viewGamepads : Dict Int Gamepad.Gamepad -> Html Msg
viewGamepads gamepads =
    div [ id "gamepads" ]
        [ if Dict.isEmpty gamepads then
            text "No gamepads found - Connect and press a gamepad button to activate."

          else
            Dict.values gamepads
                |> List.map (\gamepad -> Html.text gamepad.id)
                |> div [ class "gamepad" ]
        ]


viewGraph : List Duration -> Html Msg
viewGraph ds =
    svg
        [ width "800"
        , height "400"
        , viewBox "0 0 1000 500"
        , Svg.Attributes.id "action"
        , Svg.Attributes.style "margin: auto"
        ]
        (Svg.line
            [ x1 "500"
            , y1 "0"
            , x2 "500"
            , y2 "500"
            , stroke "#bbb"
            , strokeWidth "1"
            ]
            []
            :: List.map
                (\(Duration t) ->
                    Svg.rect
                        [ x (String.fromInt (t + 500))
                        , y "250"
                        , width "10"
                        , height "10"
                        , fill "#f00"
                        ]
                        []
                )
                ds
        )


viewCircle : Bool -> Phase -> Html Msg
viewCircle pushed phase =
    svg
        [ width "400"
        , height "400"
        , viewBox "0 0 100 150"
        , Svg.Attributes.id "graph"
        ]
        [ g []
            [ g
                []
                [ line
                    [ x1 "50"
                    , y1 "25"
                    , x2 "50"
                    , y2 "50"
                    , stroke "#000"
                    ]
                    []
                ]
            , g
                [ transform ("translate(0, 50) rotate(" ++ String.fromFloat (angle phase) ++ ", 50, 50)")
                ]
                [ circle
                    [ cx "50"
                    , cy "50"
                    , r "50"
                    , fill
                        (if pushed then
                            "#0f0"

                         else
                            "#0f0"
                        )
                    ]
                    []
                , line
                    [ x1 "50"
                    , y1 "50"
                    , x2 "50"
                    , y2 "0"
                    , stroke "#000"
                    ]
                    []
                ]
            ]
        ]


result : List Posix -> List Duration
result =
    List.map
        (\t ->
            let
                millis =
                    Time.posixToMillis t

                mod =
                    modBy 1000 millis

                dist =
                    if mod < 500 then
                        mod

                    else
                        mod - 1000
            in
            Duration dist
        )


mean : List Duration -> Duration
mean ds =
    List.map (\(Duration xs) -> abs xs) ds
        |> List.sum
        |> (\sum -> sum // List.length ds)
        |> Duration


within10secsBefore : Posix -> Posix -> Bool
within10secsBefore t1 t2 =
    10000 > (Time.posixToMillis t1 - Time.posixToMillis t2)


buttonAction : Model -> Gamepad.Input -> ButtonAction
buttonAction model input =
    if model.pressed == input.pressed then
        None

    else if model.pressed && not input.pressed then
        Up

    else
        Down


angle : Phase -> Float
angle p =
    360.0 * p
