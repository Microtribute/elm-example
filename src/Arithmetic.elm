module Arithmetic exposing (..)

add : String -> String -> String
add str1 str2 =
    let
        addDigits : Char -> Char -> Int -> (Char, Int)
        addDigits d1 d2 c =
            let
                n1 = Char.toCode d1 - 48
                n2 = Char.toCode d2 - 48
                s = n1 + n2 + c
            in
            (Char.fromCode (remainderBy 10 s + 48), s // 10)

        addHelper : String -> String -> Int -> String -> String
        addHelper s1 s2 c acc =
            case (String.uncons s1, String.uncons s2) of
                (Nothing, Nothing) ->
                    if c == 0 then
                        acc
                    else
                        String.cons (Char.fromCode (c + 48)) acc

                (Just (d1, r1), Nothing) ->
                    let
                        (s, cc) = addDigits d1 '0' c
                    in
                    addHelper r1 "" cc (String.cons s acc)

                (Nothing, Just (d2, r2)) ->
                    let
                        (s, cc) = addDigits '0' d2 c
                    in
                    addHelper "" r2 cc (String.cons s acc)

                (Just (d1, r1), Just (d2, r2)) ->
                    let
                        (s, cc) = addDigits d1 d2 c
                    in
                    addHelper r1 r2 cc (String.cons s acc)
    in
    addHelper str1 str2 0 ""
