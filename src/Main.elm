module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Http
import Dict
import Dict exposing (Dict)
import Html exposing (Attribute)
import Html.Attributes as Attrs exposing (type_)
import Html.Events as Events
import Murmur3 exposing (hashString)
import Mode exposing (Mode)
import Json.Decode
import Time

main : Program () Model Msg
main = Browser.element {
    init = init,
    update = update,
    subscriptions = \_->Sub.none,
    view = view }

type alias State = {
    scores: Dict Mode Score,
    scol: Bool,
    modes: List Mode }

type Model
    = Failure
    | Loading
    | Success State

init : () -> (Model, Cmd Msg)
init _ = ( Loading, Http.get { url = "../output.txt", expect = Http.expectString GotText } )

type Msg
    = GotText (Result Http.Error String)
    | Do (State -> State)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    GotText result -> case result of
        Ok fullText -> (Success { scores=parse fullText, scol=False, modes=Mode.modes}, Cmd.none)
        Err _ -> (Failure, Cmd.none)
    Do thing -> case model of
        Success state -> (Success (thing state), Cmd.none)
        otherwise -> (otherwise, Cmd.none)

type alias Score = {
    score: Int,
    mode: Mode,
    date: Time.Posix,
    owner: String }

parse_score : String -> Maybe Score
parse_score ln = case String.split " " ln of
    (scorestr :: mode :: datestr :: name) -> 
        String.toInt scorestr |> Maybe.map (\score ->
        String.toInt datestr  |> Maybe.map (\date ->
            Score score (Mode.fromString mode) (Time.millisToPosix (date * 1000)) (String.join "," name)
        )) |> Maybe.andThen identity
    _ -> Nothing

parse : String -> Dict Mode Score
parse lns = String.lines lns 
    |> List.filterMap (parse_score >> (Maybe.map (\x-> (x.mode, x))))
    |> Dict.fromList

on_change : (String -> msg) -> Attribute msg
on_change tagger = Json.Decode.map tagger Events.targetValue
    |> Json.Decode.map (\x->(x, True))
    |> Events.stopPropagationOn "change"

view : Model -> Html Msg
view model = case model of
    Failure -> text "loading failed epically"
    Loading -> text "loading..."
    Success state -> Html.div [] [
        make_table state,
        Html.main_ [] [
            Html.label [] [
                Html.input [
                    type_ "checkbox",
                    Events.onCheck ( \bool -> Do (\s-> {s | scol=bool}))] [],
                text "one-letter modes on single column"],
            Html.br [] [],
            Html.label [] [
                text "modes: ",
                Html.input [
                    type_ "text",
                    Attrs.size 40,
                    Attrs.value (state.modes |> List.map Mode.toString |> String.join " "),
                    on_change ( \m -> Do (\s-> {s | modes=
                        String.words m |> List.map Mode.fromString}))] []],
            Html.button [Events.onClick (Do (\s-> {s|modes=Mode.modes}))] [text "reset"],
            Html.label [] [
                text " add to all: ",
                Html.input [
                    type_ "text",
                    Attrs.value "",
                    on_change( \ms -> Do (\s -> {s | modes=
                        let m1 = Mode.fromString ms in
                        s.modes |> List.filterMap (\m2->
                            if Mode.intersect m1 m2 /= 0 then Nothing else
                                Just (Mode.merge m1 m2))}))
                ] []]
        ]]

player_color : String -> Attribute msg
player_color name =
    let hash = hashString 3054 name in
    let hue = hash |> modBy 360 |> String.fromInt in
    let lgt = ((hash // 360) |> modBy 45) + 40 |> String.fromInt in
    Attrs.style "background-color" ("hsl("++hue++",60%,"++lgt++"%")

fmt_date : Time.Posix -> String
fmt_date date =
    let 
        fromMonth month = case month of
            Time.Jan -> 1  -- WHAT the FUCK elm/time
            Time.Feb -> 2  -- what on EARTH were you thinking
            Time.Mar -> 3  -- did you really expect noöne to need this
            Time.Apr -> 4  -- it's fucking defined there it's just private
            Time.May -> 5  -- like ?????
            Time.Jun -> 6  -- > Represents a Month so that you can convert it to a String or Int however you please.
            Time.Jul -> 7  -- oh so how do you convert it into an Int?
            Time.Aug -> 8  -- NOPE
            Time.Sep -> 9  -- STUPID QUESTION
            Time.Oct -> 10 -- hate this stupid ass language
            Time.Nov -> 11 -- and there's no way to compress this down
            Time.Dec -> 12 -- if i were using elm/format this would take 3 times as many lines
        padzero digits = String.fromInt >> String.padLeft digits '0'
        t thing = (thing Time.utc date)
        ts digits = t >> padzero digits
    in
        (ts 4 Time.toYear) ++ "-" ++
        (t Time.toMonth |> fromMonth |> padzero 2) ++ "-" ++
        (ts 2 Time.toDay) ++ " " ++
        (ts 2 Time.toHour) ++ ":" ++
        (ts 2 Time.toMinute) ++ ":" ++
        (ts 2 Time.toSecond) ++ " (UTC+00:00)"

make_cell : Dict Mode Score -> Mode -> Html msg
make_cell scores mode = 
    let cell attrs = Html.a [Attrs.href ("https://ubq323.website/ffbm#" ++ Mode.toString mode)] >> List.singleton >> Html.td attrs in
    case Dict.get mode scores of
        Just {score, owner, date} -> [
            text (String.fromInt score),
            Html.small [] [text (" " ++ Mode.toString mode)],
            Html.br [] [],
            Html.small [] [text owner] ]
            |> cell [player_color owner, Attrs.title (
                owner ++ " " ++ (String.fromInt score) ++ " in " ++ (Mode.toString mode) ++ " at "
                ++ (fmt_date date))]
        Nothing -> [text (Mode.toString mode)] |> cell []
    

doif : Bool -> (a -> a) -> (a -> a)
doif bl fun = if bl then fun else identity

make_table : State -> Html msg
make_table state =
    state.modes
    |> List.indexedMap Tuple.pair
    |> List.map (\(x, m1) ->
        state.modes |> doif state.scol (\i->0::i)
        |> List.take (x + 1)
        |> List.map (\m2 -> make_cell state.scores (Mode.merge m1 m2))
        |> doif state.scol     (\i->i++[ Html.td [Attrs.class "diag"] [text (Mode.toString m1)] ])
        |> doif (not state.scol) ((::) ( Html.td [Attrs.class "left"] [text (Mode.toString m1)] ))
        |> Html.tr [])
    |> Html.table []
