module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onMouseDown, onMouseMove, onMouseUp)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, readonly, style, type_, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode
import List
import MedalStanding exposing (MedalStanding, rank)
import Nanoid
import Ports exposing (..)
import Process
import Random
import Set
import String.Ext
import Task
import UUID exposing (UUID)


type Msg
    = GenerateRandomNumber
    | GotRandomNumber Int
    | PressedGenerateButton
    | DelayRandomNumberGeneration Int
    | UpdateLower String
    | UpdateUpper String
    | CharacterKey Char
    | ControlKey String
    | GenerateNanoid
    | GenerateUuid
    | GotNanoid Nanoid.Nanoid
    | GotUuid UUID
    | MouseMove Coord2D
    | MouseDown Coord2D
    | MouseUp Coord2D
    | GotValueFromLocalStorage LocalStorageKeyValue
    | RequestFullLocalStorage
    | GotFullLocalStorage (List LocalStorageKeyValue)
    | ToggleMouseMoveSetting
    | RandomizeMedalStandings
    | GotMedals (List Int)
    | NoOp


type alias RangeUpdate =
    ( Int, Int )


type alias Coord2D =
    ( Int, Int )


attendingCountries : List String
attendingCountries =
    [ "China"
    , "Taiwan"
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


type alias Model =
    { upper : Int
    , lower : Int
    , integer : Maybe Int

    -- Nanoid
    , nanoid : Maybe Nanoid.Nanoid

    -- UUID
    , uuid : Maybe UUID

    -- Cause of generation
    , generatedBy : Maybe String

    -- Mouse click position
    , mouseClickedAt : Maybe Coord2D

    -- Realtime position of mouse
    , mousePosition : Coord2D

    -- Enable capturing the mouse movement
    , mouseMoveEnabled : Bool

    -- Medal Standings
    , medalStandings : List MedalStanding
    , generationDelay : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel
    , initialize
    )


randomizeStandingsMedals : Cmd Msg
randomizeStandingsMedals =
    Random.int 0 100
        |> Random.list (3 * List.length attendingCountries)
        |> Random.generate GotMedals


initialize : Cmd Msg
initialize =
    Cmd.batch
        [ getLocalStorage "nanoid"
        , getLocalStorage "number"
        , getLocalStorage "uuid"
        , command RandomizeMedalStandings
        ]


initialModel : Model
initialModel =
    { upper = 1000
    , lower = 0
    , integer = Nothing
    , generatedBy = Nothing
    , nanoid = Nothing
    , uuid = Nothing
    , mouseClickedAt = Nothing
    , mousePosition = ( 0, 0 )
    , mouseMoveEnabled = False
    , medalStandings = []
    , generationDelay = Nothing
    }


destructMaybe : b -> (a -> b) -> Maybe a -> b
destructMaybe d f m =
    case m of
        Nothing ->
            d

        Just v ->
            f v


showUUID : Maybe UUID -> String
showUUID =
    destructMaybe "" UUID.toString


showNanoid : Maybe Nanoid.Nanoid -> String
showNanoid =
    destructMaybe "" Nanoid.toString


showNumber : Maybe Int -> String
showNumber =
    destructMaybe "" String.fromInt


isGenerating : Maybe Int -> Bool
isGenerating =
    (<) 0 << Maybe.withDefault 0


view : Model -> Html Msg
view model =
    main_ []
        [ section []
            [ h1 [] [ text "Medal Standings" ]
            , renderMedalStandings model.medalStandings
            , button [ onClick RandomizeMedalStandings ] [ text "Randomize" ]
            ]
        , section []
            [ h1 [] [ text "Random Number Generation" ]
            , renderKeystrokeTester
            , renderRangeInput "Lower value: " model.lower UpdateLower
            , renderRangeInput "Upper value: " model.upper UpdateUpper
            , button [ disabled <| isGenerating model.generationDelay, onClick PressedGenerateButton ] [ text "Generate" ]
            , renderGenerateWithDelayButton 10 model.generationDelay
            , renderOutput model
            ]
        , section []
            [ h1 [] [ text "Nanoid Generation" ]
            , div [] [ button [ onClick GenerateNanoid ] [ text "Generate" ] ]
            , div []
                [ code [] [ text (showNanoid model.nanoid) ] ]
            ]
        , section []
            [ h1 [] [ text "UUID Generation" ]
            , div [] [ button [ onClick GenerateUuid ] [ text "Generate" ] ]
            , div []
                [ code [] [ text (showUUID model.uuid) ] ]
            ]
        , section []
            [ h1 [] [ text "MouseMove" ]
            , renderMouseMoveToggle model.mouseMoveEnabled
            , renderCoord model.mousePosition
            ]
        , section []
            [ h1 [] [ text "Event" ]
            , div []
                [ span [] [ renderMouseClickPosition model.mouseClickedAt ]
                , strong [] [ text "Moved Enough: " ]
                , code []
                    [ text <|
                        if isMovedEnough model then
                            "True"

                        else
                            "False"
                    ]
                ]
            ]
        , section []
            [ h1 [] [ text "What's in localStorage?" ]
            , renderLocalStorage model
            ]
        ]


renderMouseClickPosition : Maybe Coord2D -> Html Msg
renderMouseClickPosition pos =
    case pos of
        Nothing ->
            text ""

        Just p ->
            renderCoord p


renderLocalStorageKey : LocalStorageKey -> Html msg
renderLocalStorageKey key =
    code [ class "key" ] [ text key ]


renderLocalStorageValue : LocalStorageValue -> Html msg
renderLocalStorageValue value =
    code [ class "value" ] [ text (Maybe.withDefault "" value) ]


renderSeparator : Html msg
renderSeparator =
    code [ class "separator" ] [ text ": " ]


renderLocalStorage : Model -> Html Msg
renderLocalStorage { uuid, nanoid, integer } =
    [ ( "uuid", showUUID uuid )
    , ( "nanoid", showNanoid nanoid )
    , ( "number", showNumber integer )
    ]
        |> List.map ((\( key, value ) -> [ renderLocalStorageKey key, renderSeparator, renderLocalStorageValue (Just value) ]) >> li [])
        >> ul []


renderMouseMoveToggle : Bool -> Html Msg
renderMouseMoveToggle enabled =
    let
        verb =
            if enabled then
                "Disable"

            else
                "Enable"
    in
    div []
        [ button [ onClick ToggleMouseMoveSetting ] [ text (verb ++ " Mouse Move") ] ]


wrapString : ( String, String ) -> String -> String
wrapString ( head, tail ) str =
    head ++ str ++ tail


renderCoord : Coord2D -> Html Msg
renderCoord ( left, right ) =
    code []
        [ [ left, right ]
            |> List.map String.fromInt
            |> String.join ", "
            |> wrapString ( "(", ")" )
            |> text
        ]


renderMedalStanding : MedalStanding -> Html Msg
renderMedalStanding { country, gold, silver, bronze } =
    tr []
        [ td [] [ text (String.toUpper country) ]
        , td [] [ text (String.fromInt gold) ]
        , td [] [ text (String.fromInt silver) ]
        , td [] [ text (String.fromInt bronze) ]
        ]


viewKeyedMedalStanding : MedalStanding -> ( String, Html Msg )
viewKeyedMedalStanding medalStanding =
    ( medalStanding.country, lazy renderMedalStanding medalStanding )


renderMedalStandings : List MedalStanding -> Html Msg
renderMedalStandings medalStandings =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Country" ]
                , th [] [ text "🥇" ]
                , th [] [ text "🥈" ]
                , th [] [ text "🥉" ]
                ]
            ]
        , Keyed.node "tbody" [] <| List.map viewKeyedMedalStanding medalStandings
        ]


renderGenerateWithDelayButton : Int -> Maybe Int -> Html Msg
renderGenerateWithDelayButton delay maybe =
    let
        countdown =
            Maybe.withDefault 0 maybe

        ( attrs, caption ) =
            if countdown < 1 then
                ( [ onClick <| DelayRandomNumberGeneration delay ], "Generate with a delay of " ++ String.fromInt delay ++ " seconds" )

            else
                ( [ disabled True ], "Generating in " ++ String.fromInt countdown ++ " seconds" )
    in
    button attrs [ text caption ]



-- List.map renderMedalStanding >> ol []


renderRangeInput : String -> Int -> (String -> Msg) -> Html Msg
renderRangeInput label val inputHandler =
    div []
        [ text label
        , input
            [ type_ "number"
            , value (String.fromInt val)
            , onInput inputHandler
            , stopPropagationOn "keydown" alwaysStopBubbling
            , stopPropagationOn "keyup" alwaysStopBubbling
            , stopPropagationOn "keypress" alwaysStopBubbling
            ]
            []
        ]


{-| On this input box, pressing 'r' or 't' keys would not trigger number generation
-}
renderKeystrokeTester : Html Msg
renderKeystrokeTester =
    div []
        [ text "Keystroke Tester: "
        , input
            [ type_ "text"
            , readonly True
            , stopPropagationOn "keydown" (stopBubblingOnKeystrokes [ 'r', 't' ])
            ]
            []
        ]


unique : List a -> List a
unique l =
    let
        incUnique : a -> List a -> List a
        incUnique elem lst =
            if List.member elem lst then
                lst

            else
                elem :: lst
    in
    List.foldr incUnique [] l


unique2 : List comparable -> List comparable
unique2 =
    Set.fromList >> Set.toList


getPreventingKeys : String -> List Char
getPreventingKeys str =
    String.split "|" str
        |> List.map String.trim
        |> List.map String.Ext.toChar
        |> List.map (Maybe.withDefault ' ')
        |> unique2


stopBubblingOnKeystrokes : List Char -> Json.Decode.Decoder ( Msg, Bool )
stopBubblingOnKeystrokes chars =
    let
        keyNames =
            List.map (String.fromChar << Char.toLower) chars
    in
    Json.Decode.map
        (\key -> ( NoOp, List.member key keyNames ))
        (Json.Decode.field "key" Json.Decode.string)


alwaysStopBubbling : Json.Decode.Decoder ( Msg, Bool )
alwaysStopBubbling =
    Json.Decode.succeed ( NoOp, True )


renderOutput : Model -> Html Msg
renderOutput model =
    case model.generatedBy of
        Nothing ->
            div [ style "color" "gray", style "font-style" "italic" ]
                [ text "Generate a new random number. In order to get a new number you can:"
                , ul []
                    [ li [] [ text "Click the \"Generate\" button." ]
                    , li [] [ text "Press a key." ]
                    , li [] [ text "Change the lower value." ]
                    , li [] [ text "Change the upper value." ]
                    , li [] [ text "Move your mouse." ]
                    ]
                ]

        Just cause ->
            div []
                [ strong [] [ text (String.fromInt <| Maybe.withDefault 0 model.integer) ]
                , span
                    [ style "color" "red"
                    , style "font-weight" "bold"
                    ]
                    [ text " popped out by "
                    , text cause
                    , text "."
                    ]
                ]


makePositive : String -> Int
makePositive input =
    let
        intVal =
            Maybe.withDefault 0 <| String.toInt input
    in
    if intVal >= 0 then
        intVal

    else
        0


generateNewRandomNumber : Int -> Int -> Cmd Msg
generateNewRandomNumber lower upper =
    Random.generate GotRandomNumber (Random.int lower upper)


generateNumber : Model -> RangeUpdate -> ( Model, Cmd Msg )
generateNumber currentModel ( newLower, newUpper ) =
    let
        model =
            { currentModel
                | upper = newUpper
                , lower = newLower
            }
    in
    ( model
    , if isGenerating currentModel.generationDelay then
        Cmd.none

      else
        generateNewRandomNumber model.lower model.upper
    )


uppercase : Char -> String
uppercase c =
    String.fromChar <|
        if Char.isAlpha c then
            Char.toUpper c

        else
            c


parseLocalStorageValue : (String -> Maybe a) -> LocalStorageValue -> Maybe a
parseLocalStorageValue func val =
    destructMaybe Nothing func val


parseAsUUID : LocalStorageValue -> Maybe UUID
parseAsUUID =
    parseLocalStorageValue (Result.toMaybe << UUID.fromString)


parseAsNanoid : LocalStorageValue -> Maybe Nanoid.Nanoid
parseAsNanoid =
    parseLocalStorageValue (Just << Nanoid.fromString)


parseAsNumber : LocalStorageValue -> Maybe Int
parseAsNumber =
    parseLocalStorageValue String.toInt


command : Msg -> Cmd Msg
command =
    Task.perform identity << Task.succeed


defer : Int -> Msg -> Cmd Msg
defer delay msg =
    delay |> toFloat |> Process.sleep |> Task.attempt (\_ -> msg)


batch : List Msg -> Cmd Msg
batch =
    Cmd.batch << List.map command


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressedGenerateButton ->
            ( { model | generatedBy = Just "clicking the \"Generate\" button" }, command GenerateRandomNumber )

        GenerateRandomNumber ->
            generateNumber model ( model.lower, model.upper )

        DelayRandomNumberGeneration delay ->
            let
                ( newModel, nextMsg ) =
                    if delay > 0 then
                        ( { model
                            | generationDelay = Just delay
                            , generatedBy = Just "waiting"
                          }
                        , defer 1000 <| DelayRandomNumberGeneration (delay - 1)
                        )

                    else
                        ( { model
                            | generationDelay = Nothing
                            , generatedBy = Just "delayed generation"
                          }
                        , command GenerateRandomNumber
                        )
            in
            ( newModel, nextMsg )

        GotRandomNumber number ->
            ( { model | integer = Just number }, Ports.setLocalStorage ( "number", Just (String.fromInt number) ) )

        UpdateUpper upper ->
            generateNumber { model | generatedBy = Just "updating upper value" } ( model.lower, makePositive upper )

        UpdateLower lower ->
            generateNumber { model | generatedBy = Just "updating lower value" } ( makePositive lower, model.upper )

        ControlKey controlKey ->
            generateNumber { model | generatedBy = Just ("pressing the " ++ controlKey ++ " key") } ( model.lower, model.upper )

        CharacterKey key ->
            generateNumber { model | generatedBy = Just ("pressing the " ++ uppercase key ++ " key") } ( model.lower, model.upper )

        GenerateNanoid ->
            ( model, Random.generate GotNanoid Nanoid.generator )

        GenerateUuid ->
            ( model, Random.generate GotUuid UUID.generator )

        GotNanoid nanoid ->
            ( { model | nanoid = Just nanoid }, Ports.setLocalStorage ( "nanoid", Just (Nanoid.toString nanoid) ) )

        GotUuid uuid ->
            ( { model | uuid = Just uuid }, Ports.setLocalStorage ( "uuid", Just (UUID.toString uuid) ) )

        GotValueFromLocalStorage ( "nanoid", value ) ->
            ( { model | nanoid = parseAsNanoid value }, Cmd.none )

        GotValueFromLocalStorage ( "number", value ) ->
            let
                n =
                    parseAsNumber value

                cause =
                    destructMaybe model.generatedBy (\_ -> Just " initiating from local storage") n
            in
            ( { model | integer = n, generatedBy = cause }, Cmd.none )

        GotValueFromLocalStorage ( "uuid", value ) ->
            ( { model | uuid = parseAsUUID value }, Cmd.none )

        GotValueFromLocalStorage _ ->
            ( model, Cmd.none )

        MouseDown pos ->
            ( { model | mouseClickedAt = Just pos }, Cmd.none )

        MouseMove pos ->
            ( { model | mousePosition = pos }, Cmd.none )

        MouseUp _ ->
            let
                moved =
                    model.mouseMoveEnabled && isMovedEnough model
            in
            ( { model
                | mouseClickedAt = Nothing
                , generatedBy =
                    if moved then
                        Just "mouse move"

                    else
                        model.generatedBy
              }
            , if moved then
                batch [ GenerateNanoid, GenerateRandomNumber, GenerateUuid ]

              else
                Cmd.none
            )

        ToggleMouseMoveSetting ->
            ( { model | mouseMoveEnabled = not model.mouseMoveEnabled }, Cmd.none )

        RequestFullLocalStorage ->
            ( model, Ports.getAllLocalStorage () )

        GotFullLocalStorage _ ->
            ( model, Cmd.none )

        RandomizeMedalStandings ->
            ( model, randomizeStandingsMedals )

        GotMedals medals ->
            ( { model | medalStandings = distributeMedalsToCountries medals |> rank }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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


isMovedEnough : Model -> Bool
isMovedEnough { mouseClickedAt, mousePosition } =
    let
        ( x1, y1 ) =
            Maybe.withDefault mousePosition mouseClickedAt

        ( x2, y2 ) =
            mousePosition

        dx =
            abs (x1 - x2)

        dy =
            abs (y1 - y2)
    in
    dx > 100 && dy > 100


mapSub : model -> List (model -> Sub msg) -> List (Sub msg)
mapSub model =
    List.map (\subscriber -> subscriber model)


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map toKey (Json.Decode.field "key" Json.Decode.string)


mouseEventDecoder : Json.Decode.Decoder Coord2D
mouseEventDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "layerX" Json.Decode.int)
        (Json.Decode.field "layerY" Json.Decode.int)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        _ ->
            ControlKey keyValue


readKeyPress : Model -> Sub Msg
readKeyPress _ =
    onKeyDown keyDecoder


watchMouseMove : Model -> Sub Msg
watchMouseMove _ =
    onMouseMove (Json.Decode.map MouseMove mouseEventDecoder)


watchMouseDown : Model -> Sub Msg
watchMouseDown _ =
    onMouseDown (Json.Decode.map MouseDown mouseEventDecoder)


watchMouseUp : Model -> Sub Msg
watchMouseUp _ =
    onMouseUp (Json.Decode.map MouseUp mouseEventDecoder)


{-| Accepts individual localStorage item
-}
captureLocalStorageItem : Model -> Sub Msg
captureLocalStorageItem _ =
    acceptLocalStorage GotValueFromLocalStorage


{-| Accepts full localStorage items |
-}
captureFullLocalStorage : Model -> Sub Msg
captureFullLocalStorage _ =
    acceptAllLocalStorage GotFullLocalStorage


subscriptions : Model -> Sub Msg
subscriptions model =
    [ captureLocalStorageItem
    , captureFullLocalStorage
    , readKeyPress
    , watchMouseDown
    , watchMouseMove
    , watchMouseUp
    ]
        |> mapSub model
        |> Sub.batch


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
