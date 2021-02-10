port module Gamepad exposing
    ( ConnectionChanged(..)
    , Gamepad
    , Input
    , onConnectionChanged
    , onInput
    )

import Time exposing (Posix)


port onInputInternal : (InputInternal -> msg) -> Sub msg


port onGamepadConnected : (Gamepad -> msg) -> Sub msg


port onGamepadDisconnected : (Gamepad -> msg) -> Sub msg


type alias Pad =
    Bool


type alias InputInternal =
    { time : Int
    , pressed : Pad
    }


type alias Input =
    { time : Posix
    , pressed : Pad
    }


type alias Gamepad =
    { index : Int
    , id : String
    }


type ConnectionChanged
    = Connected Gamepad
    | Disconnected Gamepad


decodeInput : InputInternal -> Input
decodeInput ii =
    { time = Time.millisToPosix ii.time
    , pressed = ii.pressed
    }


onConnectionChanged : (ConnectionChanged -> msg) -> Sub msg
onConnectionChanged msg =
    Sub.batch
        [ onGamepadConnected (Connected >> msg)
        , onGamepadDisconnected (Disconnected >> msg)
        ]


onInput : (Input -> msg) -> Sub msg
onInput msg =
    onInputInternal (decodeInput >> msg)
