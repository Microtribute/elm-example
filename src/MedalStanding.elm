module MedalStanding exposing
    ( MedalStanding
    , populateMedalStandings
    , populateMedalStandings2
    , populateMedalStandings3
    , populateMedalStandings4
    , populateMedalStandings5
    , populateMedalStandings6
    )

import Random


type alias MedalStanding =
    { country : String
    , gold : Int
    , silver : Int
    , bronze : Int
    }


attendingCountries : List String
attendingCountries =
    [ "China"
    , "Taiwan"
    , "Hong Kong"
    , "Iran"
    , "Kazakhstan"
    , "Cambodia"
    , "Nepal"
    , "Vietnam"
    , "Tajikistan"
    , "Japan"
    , "India"
    , "Jordan"
    , "Malaysia"
    , "Bangladesh"
    , "Armenia"
    , "Sri Lanka"
    , "Saudi Arabia"
    , "Kuwait"
    , "Mongolia"
    , "Afghanistan"
    , "Armenia"
    , "Bahrain"
    , "Bhutan"
    , "Philippines"
    , "Indonesia"
    , "Australia"
    ]


{-| Not used

@deprecated

-}
toComparable : MedalStanding -> String
toComparable { gold, silver, bronze } =
    [ gold, silver, bronze ]
        |> List.map (String.fromInt >> String.padLeft 16 '0')
        |> String.join ""


keyComparator : MedalStanding -> MedalStanding -> Order
keyComparator s1 s2 =
    compare (toComparable s1) (toComparable s2)


traditionalComparator : MedalStanding -> MedalStanding -> Order
traditionalComparator s1 s2 =
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


ifComparator : MedalStanding -> MedalStanding -> Order
ifComparator s1 s2 =
    if s1.gold == s2.gold && s1.silver == s2.silver && s1.bronze == s2.bronze then
        EQ

    else if s1.gold > s2.gold || (s1.gold == s2.gold && s1.silver > s2.silver) || (s1.gold == s2.gold && s1.silver == s2.silver && s1.bronze > s2.bronze) then
        GT

    else
        LT


defaultComparator : MedalStanding -> MedalStanding -> Order
defaultComparator s1 s2 =
    compare ( s1.gold, s1.silver, s1.bronze ) ( s2.gold, s2.silver, s2.bronze )


listComparator : MedalStanding -> MedalStanding -> Order
listComparator s1 s2 =
    compare [ s1.gold, s1.silver, s1.bronze ] [ s2.gold, s2.silver, s2.bronze ]


distributeMedalsToCountries : List Int -> List MedalStanding
distributeMedalsToCountries medals =
    let
        walk : List Int -> List String -> List MedalStanding -> List MedalStanding
        walk medals_ countries_ result =
            case countries_ of
                [] ->
                    result

                country :: otherCountries ->
                    case medals_ of
                        x :: y :: z :: otherMedals ->
                            walk otherMedals otherCountries (MedalStanding country x y z :: result)

                        [ x, y ] ->
                            walk [] otherCountries (MedalStanding country x y 0 :: result)

                        [ x ] ->
                            walk [] otherCountries (MedalStanding country x 0 0 :: result)

                        _ ->
                            walk [] otherCountries (MedalStanding country 0 0 0 :: result)
    in
    walk medals attendingCountries []


defaultGenerator : Random.Generator Int
defaultGenerator =
    Random.int 0 100


seedRandomNumbers : Int -> Int -> List Int
seedRandomNumbers seed size =
    let
        helper : Random.Seed -> Int -> List Int -> List Int
        helper currentSeed currentSize generated =
            if currentSize < size then
                let
                    ( value, newSeed ) =
                        Random.step defaultGenerator currentSeed
                in
                helper newSeed (currentSize + 1) (value :: generated)

            else
                generated
    in
    helper (Random.initialSeed seed) 0 []


seedRandomNumbers2 : Int -> Int -> List Int
seedRandomNumbers2 seed size =
    Random.step
        (Random.list size defaultGenerator)
        (Random.initialSeed seed)
        |> Tuple.first


totalMedalSlots : Int
totalMedalSlots =
    -- 3 here because there are 3 classes of medals:
    -- gold, silver, and bronze
    3 * List.length attendingCountries


generateAndApplySorter : (MedalStanding -> MedalStanding -> Order) -> Int -> List MedalStanding
generateAndApplySorter sorter n =
    totalMedalSlots
        |> seedRandomNumbers n
        |> distributeMedalsToCountries
        |> List.sortWith sorter
        |> List.reverse


generateAndApplySorter2 : (MedalStanding -> MedalStanding -> Order) -> Int -> List MedalStanding
generateAndApplySorter2 sorter n =
    totalMedalSlots
        |> seedRandomNumbers2 n
        |> distributeMedalsToCountries
        |> List.sortWith sorter
        |> List.reverse


populateMedalStandings : Int -> List MedalStanding
populateMedalStandings =
    generateAndApplySorter defaultComparator


populateMedalStandings2 : Int -> List MedalStanding
populateMedalStandings2 =
    generateAndApplySorter traditionalComparator


populateMedalStandings3 : Int -> List MedalStanding
populateMedalStandings3 =
    generateAndApplySorter keyComparator


populateMedalStandings4 : Int -> List MedalStanding
populateMedalStandings4 =
    generateAndApplySorter listComparator


populateMedalStandings5 : Int -> List MedalStanding
populateMedalStandings5 =
    generateAndApplySorter ifComparator


populateMedalStandings6 : Int -> List MedalStanding
populateMedalStandings6 =
    generateAndApplySorter2 ifComparator



-- newMedalStandings : (List MedalStanding -> msg) -> Cmd msg
-- newMedalStandings receiver =
--     Random.int 0 100
--         |> Random.list (3 * List.length attendingCountries)
--         |> Random.generate (distributeMedalsToCountries >> List.sortWith defaultComparator >> List.reverse >> receiver)
-- newMedalStandings2 : (List MedalStanding -> msg) -> Cmd msg
-- newMedalStandings2 receiver =
--     Random.int 0 100
--         |> Random.list (3 * List.length attendingCountries)
--         |> Random.generate (distributeMedalsToCountries >> List.sortWith traditionalComparator >> List.reverse >> receiver)
