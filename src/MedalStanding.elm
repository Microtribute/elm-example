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


rank_ : List MedalStanding -> List MedalStanding
rank_ =
    List.sortBy sorter >> List.reverse


rank : List MedalStanding -> List MedalStanding
rank =
    List.sortWith comparator >> List.reverse
