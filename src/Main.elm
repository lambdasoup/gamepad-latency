module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Dict exposing (Dict)
import Gamepad
import Html exposing (Html, div)
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
    { input : ButtonAction
    , phase : Phase
    , time : Posix
    , gamepads : Dict Int Gamepad.Gamepad
    , step : Step
    }


init : ( Model, Cmd Msg )
init =
    ( { input = None
      , phase = 0.0
      , time = Time.millisToPosix 0
      , gamepads = Dict.empty
      , step = Start
      }
    , Cmd.none
    )


type Msg
    = Frame Posix
    | OnInput ButtonAction
    | OnConnectionChanged Gamepad.ConnectionChanged


type Sample
    = Sample Posix Posix


type alias Samples =
    List Sample


type ButtonAction
    = None
    | Up Posix Posix
    | Down Posix


type Step
    = Start
    | Measure Samples
    | End Samples


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
            case model.step of
                Start ->
                    ( model, Cmd.none )

                End _ ->
                    ( model, Cmd.none )

                Measure samples ->
                    if List.length samples > 12 then
                        ( { model
                            | step = End samples
                          }
                        , Cmd.none
                        )

                    else
                        let
                            millis =
                                Time.posixToMillis now

                            mod =
                                modBy phaseMs millis

                            phase =
                                toFloat mod / toFloat phaseMs
                        in
                        ( { model
                            | phase = phase
                            , time = now
                          }
                        , Cmd.none
                        )

        OnInput input ->
            let
                ( newModel, cmd ) =
                    case model.step of
                        Start ->
                            ( { model | step = Measure [] }, Cmd.none )

                        Measure samples ->
                            case input of
                                Up since until ->
                                    let
                                        sample =
                                            Sample since until
                                    in
                                    ( { model
                                        | step = sample :: samples |> Measure
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )
            in
            ( { newModel | input = input }, cmd )

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
subscriptions model =
    Sub.batch
        [ buttonAction model >> OnInput |> Gamepad.onInput
        , Gamepad.onConnectionChanged OnConnectionChanged
        , onAnimationFrame Frame
        ]


view : Model -> Html Msg
view model =
    div []
        (case model.step of
            Start ->
                [ viewGamepads model.gamepads
                ]

            Measure _ ->
                [ viewCircle model.phase
                ]

            End samples ->
                [ div [ id "info" ]
                    [ Html.text ((samples |> List.length |> String.fromInt) ++ " pushes")
                    ]
                , div [ id "result" ]
                    [ samples |> durations |> mean |> viewDuration ]
                , viewGraph samples
                ]
        )


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


viewGraph : Samples -> Html Msg
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
                (durations ds)
        )


viewCircle : Phase -> Html Msg
viewCircle phase =
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
                    , fill "#0f0"
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


durations : List Sample -> List Duration
durations =
    List.map
        (\(Sample t _) ->
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


buttonAction : Model -> Gamepad.Input -> ButtonAction
buttonAction model input =
    case model.input of
        Down since ->
            if input.pressed then
                model.input

            else
                Up since input.time

        _ ->
            if input.pressed then
                Down input.time

            else
                None


angle : Phase -> Float
angle p =
    360.0 * p
