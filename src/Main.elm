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
      , step = Start Idle
      }
    , Cmd.none
    )


type Msg
    = Frame Posix
    | OnInput ButtonAction
    | OnConnectionChanged Gamepad.ConnectionChanged


type alias Sample =
    { down : Posix, up : Posix }


type ButtonAction
    = None
    | Up Posix Posix
    | Down Posix


type Step
    = Start Launcher
    | Run Sampling
    | End Result


type alias Result =
    List Target


type Sampling
    = Measure
        { targets : List Target
        , time : Posix
        }
    | AnimateIn Transition


type alias Target =
    { time : Posix
    , hits : List Sample
    }


type Launcher
    = Idle
    | Launching
        { timestamp : Posix
        , progress : Float
        }
    | AnimateOut Transition


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
    startTransition now |> AnimateIn |> Run


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
                                        startTransition now |> AnimateOut |> Start

                                    else
                                        Launching { timestamp = now, progress = newProgress } |> Start
                            in
                            ( { model | step = step }, Cmd.none )

                        AnimateOut transition ->
                            let
                                step =
                                    case advanceTransition transition now of
                                        EndTransition ->
                                            run now

                                        InTransition it ->
                                            InTransition it |> AnimateOut |> Start
                            in
                            ( { model | step = step }, Cmd.none )

                End _ ->
                    ( model, Cmd.none )

                Run sampling ->
                    case sampling of
                        AnimateIn transition ->
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
                                                |> Run

                                        InTransition it ->
                                            InTransition it |> AnimateIn |> Run
                            in
                            ( { model | step = step }, Cmd.none )

                        Measure measure ->
                            if targetsDone measure.targets measure.time then
                                ( { model | step = End measure.targets }, Cmd.none )

                            else
                                ( { model
                                    | step =
                                        Measure { measure | time = now }
                                            |> Run
                                  }
                                , Cmd.none
                                )

        OnInput input ->
            let
                ( newModel, cmd ) =
                    case model.step of
                        Run sampling ->
                            case sampling of
                                AnimateIn _ ->
                                    ( model, Cmd.none )

                                Measure measure ->
                                    case input of
                                        Up since until ->
                                            ( { model
                                                | step =
                                                    Measure { measure | targets = applyHit measure.targets (Sample since until) }
                                                        |> Run
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
            Start launcher ->
                [ viewGamepads model.gamepads
                , viewLaunch launcher
                ]

            Run sampling ->
                [ viewSampling sampling ]

            End result ->
                [ div [ id "info" ]
                    [ Html.text ((result |> List.length |> String.fromInt) ++ " pushes")
                    ]
                , div [ id "result" ]
                    [ result |> durations |> mean |> viewDuration ]
                , viewGraph result
                ]
        )


viewLaunch : Launcher -> Html Msg
viewLaunch launcher =
    let
        opacity =
            case launcher of
                AnimateOut tr ->
                    case tr of
                        InTransition it ->
                            1.0 - it.progress

                        EndTransition ->
                            0.0

                _ ->
                    1.0

        draw =
            \progress ->
                svg
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
    in
    div [ id "launcher", Html.Attributes.style "opacity" (String.fromFloat opacity) ]
        [ case launcher of
            Launching launching ->
                draw launching.progress

            AnimateOut _ ->
                draw 1.0

            _ ->
                draw 0.0
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


viewSampling : Sampling -> Html Msg
viewSampling sampling =
    let
        opacity =
            case sampling of
                AnimateIn tr ->
                    case tr of
                        InTransition it ->
                            it.progress

                        EndTransition ->
                            1.0

                _ ->
                    1.0
    in
    case sampling of
        Measure measure ->
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
                            :: List.map
                                (\target ->
                                    let
                                        phase =
                                            toFloat (Time.posixToMillis measure.time - Time.posixToMillis target.time) / 2000.0
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
                                measure.targets
                        )
                    ]
                ]

        _ ->
            div [] []


applyHit : List Target -> Sample -> List Target
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


durations : List Target -> List ( Duration, Duration )
durations targets =
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
    List.concat
        (List.map
            (\target ->
                List.map (\hit -> ( duration hit.down, duration hit.up )) target.hits
            )
            targets
        )


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
