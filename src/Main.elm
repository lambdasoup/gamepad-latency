module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
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
    { pad : Bool
    , phase : Phase
    , time : Posix
    , pushes : List Posix
    , targets : List Posix
    , result : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { pad = False
      , phase = 0.0
      , time = Time.millisToPosix 0
      , targets = []
      , pushes = []
      , result = 0
      }
    , Cmd.none
    )


type Msg
    = Frame Posix
    | OnInput Gamepad.Input


type ButtonAction
    = None
    | Up
    | Down


type alias Phase =
    Float


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
                            List.append model.pushes [ input.t ]

                        _ ->
                            model.pushes
            in
            ( { model
                | pad = input.p
                , pushes = pushes
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Gamepad.onInput OnInput
        , onAnimationFrame Frame
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ id "info" ]
            [ Html.text ((model.pushes |> List.length |> String.fromInt) ++ " pushes")
            ]
        , div [ id "result" ]
            [ Html.text ((model.result |> String.fromInt) ++ " ms")
            ]
        , viewCircle model.pad model.phase
        , viewGraph model.targets model.pushes model.time
        ]


viewGraph : List Posix -> List Posix -> Posix -> Html Msg
viewGraph targets allPushes time =
    let
        pushes =
            allPushes
    in
    svg
        [ width "1000"
        , height "200"
        , viewBox ("0 0 " ++ String.fromInt (phaseMs * 10) ++ " 2000")
        , Svg.Attributes.id "action"
        ]
        (List.map
            (\tx ->
                let
                    x =
                        String.fromInt (Time.posixToMillis time - Time.posixToMillis tx)
                in
                line
                    [ x1 x
                    , y1 "0"
                    , x2 x
                    , y2 "2000"
                    , stroke "#000"
                    , strokeWidth "20"
                    ]
                    []
            )
            targets
            ++ List.map
                (\tx ->
                    let
                        x =
                            String.fromInt (Time.posixToMillis time - Time.posixToMillis tx)
                    in
                    line
                        [ x1 x
                        , y1 "950"
                        , x2 x
                        , y2 "1050"
                        , stroke "#f00"
                        , strokeWidth "100"
                        ]
                        []
                )
                pushes
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


result : List Posix -> Int
result ts =
    List.map
        (\t ->
            let
                millis =
                    Time.posixToMillis t

                mod =
                    modBy 1000 millis

                nearest =
                    if mod < 500 then
                        (millis // 1000) * 1000

                    else
                        (millis // 1000 + 1) * 1000
            in
            abs (millis - nearest)
        )
        ts
        |> List.sum
        |> (\sum -> sum // List.length ts)


within10secsBefore : Posix -> Posix -> Bool
within10secsBefore t1 t2 =
    10000 > (Time.posixToMillis t1 - Time.posixToMillis t2)


buttonAction : Model -> Gamepad.Input -> ButtonAction
buttonAction model input =
    if model.pad == input.p then
        None

    else if model.pad && not input.p then
        Up

    else
        Down


angle : Phase -> Float
angle p =
    360.0 * p
