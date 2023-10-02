module MedalStanding exposing (MedalStanding, rank)


type alias MedalStanding =
    { country : String
    , gold : Int
    , silver : Int
    , bronze : Int
    }


sorter : MedalStanding -> String
sorter { gold, silver, bronze } =
    [ gold, silver, bronze ]
        |> List.map (String.fromInt >> String.padLeft 16 '0')
        |> String.join ""


comparator : MedalStanding -> MedalStanding -> Order
comparator s1 s2 =
    case
        ( compare s1.gold s2.gold
        , compare s1.silver s2.silver
        , compare s1.bronze s2.bronze
        )
    of
        ( EQ, EQ, EQ ) ->
            EQ

        ( GT, _, _ ) ->
            GT

        ( EQ, GT, _ ) ->
            GT

        ( EQ, EQ, GT ) ->
            GT

        _ ->
            LT


diff : Order -> Order -> Int
diff o1 o2 =
    let
        weight o =
            case o of
                EQ ->
                    0

                LT ->
                    -1

                GT ->
                    1
    in
    weight o1 - weight o2


comparator2 : MedalStanding -> MedalStanding -> Order
comparator2 s1 s2 =
    compare ( s1.gold, s1.silver, s1.bronze ) ( s2.gold, s2.silver, s2.bronze )


rank : List MedalStanding -> List MedalStanding
rank =
    List.sortWith comparator2 >> List.reverse
