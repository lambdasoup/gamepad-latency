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
    { input : ButtonAction
    , gamepads : Dict Int Gamepad.Gamepad
    , step : Step
    }


init : ( Model, Cmd Msg )
init =
    ( { input = None
      , gamepads = Dict.empty
      , step = Idle
      }
    , Cmd.none
    )


type Msg
    = Frame Posix
    | OnInput ButtonAction
    | OnConnectionChanged Gamepad.ConnectionChanged


type alias Hit =
    { down : Posix, up : Posix }


type ButtonAction
    = None
    | Up Posix Posix
    | Down Posix


type Step
    = Idle
    | Launching
        { timestamp : Posix
        , progress : Float
        }
    | FromLaunching Transition
    | ToMeasure Transition
    | Measure Measurement
    | Result (List Target)


type alias Measurement =
    { targets : List Target
    , time : Posix
    }


type alias Target =
    { time : Posix
    , hits : List Hit
    }


type Transition
    = InTransition { start : Posix, length : Duration, progress : Float }
    | EndTransition


startTransition : Posix -> Transition
startTransition start =
    InTransition
        { start = start
        , length = Duration 200
        , progress = 0.0
        }


advanceTransition : Transition -> Posix -> Transition
advanceTransition tr now =
    case tr of
        EndTransition ->
            EndTransition

        InTransition it ->
            let
                start =
                    Time.posixToMillis it.start

                nowMs =
                    Time.posixToMillis now

                progress =
                    toFloat (nowMs - start) / toFloat (durationToMillis it.length)
            in
            if progress >= 1.0 then
                EndTransition

            else
                InTransition { it | progress = progress }


type Duration
    = Duration Int


durationToMillis : Duration -> Int
durationToMillis (Duration ms) =
    ms


run : Posix -> Step
run now =
    startTransition now |> ToMeasure


makeTargets : Posix -> Int -> List Target -> List Target
makeTargets time n targets =
    case n of
        0 ->
            targets

        _ ->
            makeTargets
                (addDuration time (Duration 2000))
                (n - 1)
                ({ time = time, hits = [] } :: targets)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame now ->
            case model.step of
                Idle ->
                    case model.input of
                        Down _ ->
                            ( { model
                                | step =
                                    Launching
                                        { timestamp = now
                                        , progress = 0.0
                                        }
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
                                Idle

                            else if newProgress > 1.0 then
                                startTransition now |> FromLaunching

                            else
                                Launching { timestamp = now, progress = newProgress }
                    in
                    ( { model | step = step }, Cmd.none )

                FromLaunching transition ->
                    let
                        step =
                            case advanceTransition transition now of
                                EndTransition ->
                                    run now

                                InTransition it ->
                                    InTransition it |> FromLaunching
                    in
                    ( { model | step = step }, Cmd.none )

                Result _ ->
                    ( model, Cmd.none )

                ToMeasure transition ->
                    let
                        step =
                            case advanceTransition transition now of
                                EndTransition ->
                                    { targets =
                                        makeTargets
                                            (addDuration now (Duration 3000))
                                            12
                                            []
                                    , time = now
                                    }
                                        |> Measure

                                InTransition it ->
                                    InTransition it |> ToMeasure
                    in
                    ( { model | step = step }, Cmd.none )

                Measure measure ->
                    if targetsDone measure.targets measure.time then
                        ( { model | step = Result measure.targets }, Cmd.none )

                    else
                        ( { model
                            | step =
                                Measure { measure | time = now }
                          }
                        , Cmd.none
                        )

        OnInput input ->
            let
                ( newModel, cmd ) =
                    case model.step of
                        ToMeasure _ ->
                            ( model, Cmd.none )

                        Measure measure ->
                            case input of
                                Up since until ->
                                    ( { model
                                        | step =
                                            Measure
                                                { measure
                                                    | targets =
                                                        applyHit
                                                            measure.targets
                                                            (Hit since until)
                                                }
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


addDuration : Posix -> Duration -> Posix
addDuration p (Duration ms) =
    Time.posixToMillis p + ms |> Time.millisToPosix


targetsDone : List Target -> Posix -> Bool
targetsDone targets time =
    let
        last =
            List.foldr
                (\target max ->
                    if before max target.time then
                        target.time

                    else
                        max
                )
                (Time.millisToPosix 0)
                targets
    in
    before (addDuration last (Duration 3000)) time


before : Posix -> Posix -> Bool
before t1 t2 =
    Time.posixToMillis t1 < Time.posixToMillis t2


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
            Idle ->
                [ viewGamepads model.gamepads
                , viewLaunch 1.0 0.0
                ]

            Launching launching ->
                [ viewGamepads model.gamepads
                , viewLaunch 1.0 launching.progress
                ]

            FromLaunching transition ->
                let
                    opacity =
                        case transition of
                            InTransition it ->
                                1.0 - it.progress

                            EndTransition ->
                                0.0
                in
                [ viewLaunch opacity 0.0 ]

            ToMeasure transition ->
                [ let
                    opacity =
                        case transition of
                            InTransition it ->
                                it.progress

                            EndTransition ->
                                1.0
                  in
                  viewMeasurement opacity Nothing
                ]

            Measure measurement ->
                [ viewMeasurement 1.0 (Just measurement) ]

            Result targets ->
                [ div [ id "info" ]
                    [ Html.text ((targets |> List.length |> String.fromInt) ++ " pushes")
                    ]
                , div [ id "result" ]
                    [ targets
                        |> analyze
                        |> (\result ->
                                case result of
                                    Ok ds ->
                                        mean ds |> viewDuration

                                    Err msg ->
                                        Html.text msg
                           )
                    ]
                , viewGraph targets
                ]
        )


analyze : List Target -> Result String (List ( Duration, Duration ))
analyze =
    List.foldr
        (\target acc ->
            case target.hits of
                [ hit ] ->
                    case acc of
                        Ok ds ->
                            Ok (durations hit :: ds)

                        _ ->
                            acc

                _ ->
                    Err "Error: Make sure to hit each target exactly once"
        )
        (Ok [])


viewLaunch : Float -> Float -> Html Msg
viewLaunch opacity progress =
    div [ id "launcher", Html.Attributes.style "opacity" (String.fromFloat opacity) ]
        [ svg
            [ width "600"
            , height "200"
            , viewBox "0 0 600 200"
            , Svg.Attributes.style "font-size: 48px; border: solid 1px #6b6e70;"
            ]
            [ Svg.rect
                [ x "0"
                , y "0"
                , width (String.fromInt (round (600.0 * progress)))
                , height "200"
                , fill "#61892f"
                ]
                []
            , Svg.text_
                [ fill "white"
                , x "300"
                , y "100"
                , Svg.Attributes.textAnchor "middle"
                , Svg.Attributes.dominantBaseline "middle"
                ]
                [ Svg.text "Hold button to start!" ]
            ]
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


viewGraph : List Target -> Html Msg
viewGraph targets =
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
            :: List.concat
                (List.indexedMap
                    (\i target ->
                        let
                            y =
                                500 // List.length targets * i
                        in
                        List.map
                            (\hit ->
                                let
                                    ( Duration t1, Duration t2 ) =
                                        durations hit
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
                            target.hits
                    )
                    targets
                )
        )


viewMeasurement : Float -> Maybe Measurement -> Html Msg
viewMeasurement opacity maybe =
    div [ id "measure", Html.Attributes.style "opacity" (String.fromFloat opacity) ]
        [ svg
            [ width "800"
            , height "600"
            , viewBox "0 0 800 600"
            ]
            [ g []
                (g
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
                    :: (case maybe of
                            Nothing ->
                                []

                            Just measurement ->
                                List.map
                                    (\target ->
                                        let
                                            phase =
                                                toFloat (Time.posixToMillis measurement.time - Time.posixToMillis target.time) / 2000.0
                                        in
                                        g
                                            [ transform ("translate(" ++ String.fromFloat (phase * 800.0) ++ ", 0)")
                                            ]
                                            [ circle
                                                [ cx "400"
                                                , cy "300"
                                                , r "100"
                                                , stroke "#86c232"
                                                , fillOpacity "0"
                                                , strokeWidth "2px"
                                                ]
                                                []
                                            ]
                                    )
                                    measurement.targets
                       )
                )
            ]
        ]


applyHit : List Target -> Hit -> List Target
applyHit targets sample =
    List.map
        (\target ->
            let
                diff =
                    Time.posixToMillis target.time - Time.posixToMillis sample.down
            in
            if abs diff < 500 then
                { target | hits = sample :: target.hits }

            else
                target
        )
        targets


durations : Hit -> ( Duration, Duration )
durations hit =
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
    ( duration hit.down, duration hit.up )


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
