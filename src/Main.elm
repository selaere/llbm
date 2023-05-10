module Main exposing (main)

import Browser
import Html exposing (Html, text, pre)
import Http
import Dict
import Dict exposing (Dict)

main : Program () Model Msg
main = Browser.element {
    init = init,
    update = update,
    subscriptions = \_->Sub.none,
    view = view }

type Model
    = Failure
    | Loading
    | Success String


init : () -> (Model, Cmd Msg)
init _ = ( Loading, Http.get { url = "../output.txt", expect = Http.expectString GotText } )

modes : List Char
modes = String.toList "bwuipscmhaedtnvklg"

type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg _ = case msg of
    GotText result -> case result of
        Ok fullText -> (Success fullText, Cmd.none)
        Err _ -> (Failure, Cmd.none)

type alias Score = {
    score: Int,
    mode: String,
    date: String,
    owner: String }

parse_score : String -> Maybe Score
parse_score ln = case String.split "," ln of
    (score :: mode :: date :: name) -> String.toInt score
        |> Maybe.map (\scorenum -> Score scorenum mode date (String.join "," name))
    _ -> Nothing

parse : String -> Dict String Score
parse lns = String.lines lns 
    |> List.filterMap (parse_score >> (Maybe.map (\x-> (x.mode, x))))
    |> Dict.fromList

view : Model -> Html Msg
view model = case model of
    Failure -> text "loading failed epically"
    Loading -> text "loading..."
    Success fullText -> let scores = parse fullText in Html.div [] [
        make_table scores,
        pre [] [ scores
            |> Dict.values
            |> List.map (\{score, mode, date, owner} -> 
                owner ++ " " ++ (String.fromInt score) ++ " in " ++ mode ++ " at " ++ date)
            |> String.join "\n"
            |> text ]]

make_cell : Dict String Score -> String -> Html msg
make_cell scores mode = case Dict.get mode scores of
    Just score -> Html.td [] [
        text (String.fromInt score.score),
        Html.small [] [" " ++ score.mode |> text],
        Html.br [] [],
        Html.small [] [score.owner |> text] ]
    Nothing -> Html.td [] [text "so sad"]

make_table : Dict String Score -> Html msg
make_table scores =
    let modesstr = modes |> List.map String.fromChar in
    (modesstr)
    |> List.indexedMap Tuple.pair
    |> List.map (\(x, m1) ->
        modesstr 
        |> List.take (x+1)
        |> List.map (\m2 -> make_cell scores (if m1 /= m2 then m2 ++ m1 else m1))
        |> Html.tr [])
    |> Html.table []
