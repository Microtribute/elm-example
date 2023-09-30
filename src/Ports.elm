port module Ports exposing (..)

{-| Manage LocalStorage
-}


{-| Incoming value from JavaScript
-}
type alias LocalStorageKey =
    String


type alias LocalStorageValue =
    Maybe String


type alias LocalStorageKeyValue =
    ( LocalStorageKey, LocalStorageValue )


{-| Get rid of whitespace on both sides of a string.

    localStorage.setValue(key, value)

-}
port setLocalStorage : LocalStorageKeyValue -> Cmd msg


{-| Read incoming values from JavaScript

    localStorage.getItem(key)

-}
port acceptLocalStorage : (LocalStorageKeyValue -> msg) -> Sub msg


port acceptAllLocalStorage : (List LocalStorageKeyValue -> msg) -> Sub msg


port getLocalStorage : String -> Cmd msg


port getAllLocalStorage : () -> Cmd msg
