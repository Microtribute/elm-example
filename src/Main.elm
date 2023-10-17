module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onMouseDown, onMouseMove, onMouseUp)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, readonly, style, type_, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode
import List
import MedalStanding exposing (MedalStanding, populateMedalStandings)
import Nanoid
import Ports exposing (..)
import Process
import Random
import Task
import UUID exposing (UUID)
import Prime exposing (isPrime)
import Random exposing (maxInt)


type Msg
    = NoOp
    | GenerateRandomNumber String
    | GotRandomNumber Int
    | PressedGenerateButton
    | DelayAction String Int Msg
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
    | GotMedalStandings (List MedalStanding)


type alias RangeUpdate =
    ( Int, Int )


type alias Coord2D =
    ( Int, Int )


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

    -- keep generation delay
    , generationDelay : Maybe Int
    
    -- Delays for delayed actions
    , delays : Dict String Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel
    , initialize
    )


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
    , delays = Dict.empty
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


showSeconds : Int -> String
showSeconds n =
    let
        noun =
            if n > 1 then
                "seconds"

            else
                "second"
    in
    String.fromInt n ++ " " ++ noun


integerKey : String
integerKey =
    "integer"


nanoidKey : String
nanoidKey =
    "nanoid"


uuidKey : String
uuidKey =
    "uuid"

renderPrimeNumbers : Html Msg
renderPrimeNumbers =
    List.range 0 50000
        |> List.filter isPrime
        |> List.map (String.fromInt >> String.padLeft 5 (Char.fromCode 160) >> text >> List.singleton >> code [ style "background-color" "yellow" ])
        |> div [ style "display" "flex", style "flex-wrap" "wrap", style "gap" "5px", style "justify-content" "flex-start" ]


view : Model -> Html Msg
view model =
    let
        getCountdown : String -> Maybe Int
        getCountdown key =
            Dict.get key model.delays
    in
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
            , button [ disabled <| isGenerating (Dict.get "integer" model.delays), onClick PressedGenerateButton ] [ text "Generate" ]
            , delayedButton integerKey 5 (GenerateRandomNumber "executing \"integer\"") <| getCountdown integerKey
            , renderOutput model
            ]
        , section []
            [ h1 [] [ text "Nanoid Generation" ]
            , div []
                [ button [ onClick GenerateNanoid ] [ text "Generate" ]
                , delayedButton nanoidKey 8 GenerateNanoid <| getCountdown nanoidKey
                ]
            , div []
                [ code [] [ text (showNanoid model.nanoid) ] ]
            ]
        , section []
            [ h1 [] [ text "UUID Generation" ]
            , div []
                [ button [ onClick GenerateUuid ] [ text "Generate" ]
                , delayedButton uuidKey 10 GenerateUuid <| getCountdown uuidKey
                ]
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
        , section []
            [ h1 [] [ text "Prime Numbers" ]
            , renderPrimeNumbers
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


delayedButton : String -> Int -> Msg -> Maybe Int -> Html Msg
delayedButton key delay msg countdown =
    let
        _ =
            Debug.log "rendering button" <|
                key
                    ++ ", "
                    ++ String.fromInt delay
                    ++ ", "
                    ++ String.fromInt (Maybe.withDefault 0 countdown)

        ( attrs, caption ) =
            case countdown of
                Nothing ->
                    ( [ onClick <| DelayAction key delay msg ]
                    , "Execute \"" ++ key ++ "\" with a delay of " ++ showSeconds delay
                    )

                Just c ->
                    ( [ disabled True ]
                    , "Executing \"" ++ key ++ "\" in " ++ showSeconds c
                    )
    in
    button attrs [ text caption ]


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
            let integer = Maybe.withDefault 0 model.integer in
            div []
                [ text <| "A " ++ ( if isPrime integer then "prime" else "composite" ) ++ " number "
                , strong [] [ text (String.fromInt <| Maybe.withDefault 0 model.integer) ]
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


{-| Same as the `command` function but in a different approach
-}
command1 : Msg -> Cmd Msg
command1 msg =
    Task.attempt (\_ -> msg) <| Task.succeed ()


batch : List Msg -> Cmd Msg
batch =
    Cmd.batch << List.map command


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressedGenerateButton ->
            ( model, command (GenerateRandomNumber "clicking the \"Generate\" button") )

        GenerateRandomNumber cause ->
            generateNumber { model | generatedBy = Just cause } ( model.lower, model.upper )

        DelayAction key delay action ->
            if delay > 1 then
                ( { model | delays = Dict.insert key (delay - 1) model.delays }
                , defer 1000 <| DelayAction key (delay - 1) action
                )

            else
                ( { model | delays = Dict.remove key model.delays }, command action )

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
                let
                    cause =
                        "mouse move"
                in
                batch [ GenerateNanoid, GenerateRandomNumber cause, GenerateUuid ]

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
            ( model, Random.generate (populateMedalStandings >> GotMedalStandings) (Random.int Random.minInt Random.maxInt) )

        GotMedalStandings medalStandings ->
            ( { model | medalStandings = medalStandings }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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
