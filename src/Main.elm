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
    , gamepads : Dict Int Gamepad.Gamepad
    , step : Step
    }


init : ( Model, Cmd Msg )
init =
    ( { input = None
      , gamepads = Dict.empty
      , step = Start Idle
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
    = Start Launcher
    | Measure
        { samples : Samples
        , phase : Phase
        }
    | End Samples


type alias Phase =
    Float


type Launcher
    = Idle
    | Launching
        { timestamp : Posix
        , progress : Float
        }


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
                Start launcher ->
                    case launcher of
                        Idle ->
                            case model.input of
                                Down _ ->
                                    ( { model
                                        | step =
                                            Launching
                                                { timestamp = now
                                                , progress = 0.0
                                                }
                                                |> Start
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        Launching { timestamp, progress } ->
                            let
                                op =
                                    case model.input of
                                        Down _ ->
                                            (+)

                                        _ ->
                                            (-)

                                newProgress =
                                    op progress (0.001 * toFloat (Time.posixToMillis now - Time.posixToMillis timestamp))

                                step =
                                    if newProgress < 0.0 then
                                        Start Idle

                                    else if newProgress > 1.0 then
                                        Measure
                                            { samples = [], phase = 0.0 }

                                    else
                                        Launching { timestamp = now, progress = newProgress } |> Start
                            in
                            ( { model | step = step }, Cmd.none )

                End _ ->
                    ( model, Cmd.none )

                Measure measure ->
                    if List.length measure.samples >= 12 then
                        ( { model
                            | step = End measure.samples
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
                            | step = Measure { measure | phase = phase }
                          }
                        , Cmd.none
                        )

        OnInput input ->
            let
                ( newModel, cmd ) =
                    case model.step of
                        Start _ ->
                            ( model, Cmd.none )

                        Measure measure ->
                            case input of
                                Up since until ->
                                    let
                                        newSamples =
                                            Sample since until :: measure.samples
                                    in
                                    ( { model
                                        | step = Measure { measure | samples = newSamples }
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
    div [ id "elmapp" ]
        (case model.step of
            Start launcher ->
                [ viewGamepads model.gamepads
                , viewLaunch launcher
                ]

            Measure measure ->
                [ viewMeasure measure ]

            End samples ->
                [ div [ id "info" ]
                    [ Html.text ((samples |> List.length |> String.fromInt) ++ " pushes")
                    ]
                , div [ id "result" ]
                    [ samples |> durations |> mean |> viewDuration ]
                , viewGraph samples
                ]
        )


viewLaunch : Launcher -> Html Msg
viewLaunch launcher =
    div [ id "launcher" ]
        [ case launcher of
            Launching launching ->
                svg
                    [ width "400"
                    , height "400"
                    , viewBox "0 0 100 150"
                    ]
                    [ Svg.rect
                        [ x "0"
                        , y "0"
                        , width (String.fromInt (round (100.0 * launching.progress)))
                        , height "100"
                        , fill "#f00"
                        ]
                        []
                    ]

            _ ->
                Html.text "Hold button to start!"
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


viewGraph : Samples -> Html Msg
viewGraph ds =
    svg
        [ width "800"
        , height "400"
        , viewBox "0 0 1000 500"
        , Svg.Attributes.id "action"
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
            :: List.indexedMap
                (\i ( Duration t1, Duration t2 ) ->
                    let
                        y =
                            500 // List.length ds * i
                    in
                    Svg.rect
                        [ x (String.fromInt (t1 + 500))
                        , Svg.Attributes.y (String.fromInt y)
                        , height "10"
                        , width (String.fromInt (t2 - t1))
                        , fill "#f00"
                        ]
                        []
                )
                (durations ds)
        )


viewMeasure : { phase : Phase, samples : Samples } -> Html Msg
viewMeasure measure =
    svg
        [ width "800"
        , height "600"
        , viewBox "0 0 800 600"
        , Svg.Attributes.id "measure"
        ]
        [ g []
            [ g
                []
                [ circle
                    [ cx "400"
                    , cy "300"
                    , r "200"
                    , stroke "#86c232"
                    , fillOpacity "0"
                    , strokeWidth "2px"
                    ]
                    []
                ]
            , g
                [ transform ("translate(" ++ String.fromFloat (measure.phase * 800.0) ++ ", 0)")
                ]
                [ circle
                    [ cx "400"
                    , cy "300"
                    , r "200"
                    , stroke "#86c232"
                    , fillOpacity "0"
                    , strokeWidth "2px"
                    ]
                    []
                ]
            , g
                [ transform ("translate(" ++ String.fromFloat ((measure.phase - 1.0) * 800.0) ++ ", 0)")
                ]
                [ circle
                    [ cx "400"
                    , cy "300"
                    , r "200"
                    , stroke "#86c232"
                    , fillOpacity "0"
                    , strokeWidth "2px"
                    ]
                    []
                ]
            ]
        ]


durations : List Sample -> List ( Duration, Duration )
durations =
    let
        duration : Posix -> Duration
        duration =
            \t ->
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
    in
    List.map (\(Sample t1 t2) -> ( duration t1, duration t2 ))


mean : List ( Duration, Duration ) -> Duration
mean ds =
    List.map (\( Duration t1, Duration _ ) -> abs t1) ds
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
