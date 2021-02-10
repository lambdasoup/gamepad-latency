port module Gamepad exposing (Input, onInput)

import Json.Decode
import Time exposing (Posix)


port onSample : (Json.Decode.Value -> msg) -> Sub msg


type alias Pad =
    Bool


type alias Input =
    { t : Posix
    , p : Pad
    }


decodeInput : Json.Decode.Value -> Result Json.Decode.Error Input
decodeInput =
    Json.Decode.decodeValue
        (Json.Decode.map2 Input
            (Json.Decode.field "timestamp" posixDecoder)
            (Json.Decode.field "pressed" Json.Decode.bool)
        )


posixDecoder : Json.Decode.Decoder Posix
posixDecoder =
    Json.Decode.map Time.millisToPosix Json.Decode.int


onInput : (Input -> msg) -> Sub msg
onInput msg =
    onSample (decodeInput >> unresult >> msg)


unresult : Result Json.Decode.Error Input -> Input
unresult result =
    case result of
        Ok sample ->
            sample

        Err _ ->
            Input (Time.millisToPosix 0) False
