module String.Ext exposing (..)


toChar : String -> Maybe Char
toChar =
    String.uncons >> Maybe.map Tuple.first


toChar2 : String -> Maybe Char
toChar2 =
    String.toList >> List.head
