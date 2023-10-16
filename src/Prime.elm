module Prime exposing (..)

isPrime : Int -> Bool
isPrime n =
    if n < 2 then
        False
    else if n == 2 then
        True
    else if modBy 2 n == 0 then
        False
    else
        let
            boundary : Int
            boundary = (floor << sqrt << toFloat) n

            checkPrime : Int -> Bool
            checkPrime i =
                if i > boundary then
                    True
                else if modBy i n == 0 then
                    False
                else
                    checkPrime (i + 2)
        in
        checkPrime 3
