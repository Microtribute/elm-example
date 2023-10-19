module FileName exposing
    ( fromString
    , setBaseName
    , setExtension
    , toString
    )


type FileName
    = FileName String (Maybe String)


fromString : String -> FileName
fromString rawFileName =
    let
        length =
            String.length rawFileName

        dotPos =
            String.indices "." rawFileName
                |> List.reverse
                |> List.head
                |> Maybe.withDefault length

        baseName =
            String.left dotPos rawFileName

        extension =
            if dotPos == length then
                Nothing

            else
                String.right (length - dotPos - 1) rawFileName
                    |> Just
    in
    FileName baseName extension


toString : FileName -> String
toString (FileName baseName extension) =
    extension
        |> Maybe.map ((++) ".")
        |> Maybe.withDefault ""
        |> (++) baseName


setExtension : FileName -> Maybe String -> FileName
setExtension (FileName baseName _) extension =
    extension
        |> Maybe.map (String.replace "." "")
        |> FileName baseName


setBaseName : FileName -> String -> FileName
setBaseName (FileName _ extension) baseName =
    FileName baseName extension
