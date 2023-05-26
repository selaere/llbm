module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Http
import Dict
import Dict exposing (Dict)
import Html exposing (Attribute)
import Html.Attributes as Attrs exposing (type_)
import Html.Events as Events
import List exposing (singleton)
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
    context: Mode,
    coloring: Coloring,
    modes: List Mode }

type Coloring
    = ByName Int
    | ByDate
    | ByScore

type Model
    = Failure
    | Loading
    | Success State

init : () -> (Model, Cmd Msg)
init _ = ( Loading, Http.get { url = "output.txt", expect = Http.expectString GotText } )

type Msg
    = GotText (Result Http.Error String)
    | Do (State -> State)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    GotText result -> case result of
        Ok fullText -> (Success {
            scores = parse fullText,
            scol = False,
            context = 0,
            coloring = ByName 3054,
            modes = Mode.modes }, Cmd.none)
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
        Html.h1 [] [text ",leader lead board man? (llbm)"],
        if state.context == 0 then text "" else 
            Html.p [] [
                Html.text "using modes ",
                Html.b [] [text (Mode.toString state.context)],
                Html.text ". ",
                Html.button [Events.onClick (Do (\s-> {s|context=0}))] [text "reset"]
            ],
        make_table state,
        menu state
        ]

on_radio_button : Coloring -> Attribute Msg
on_radio_button coloring = Events.onCheck (\bool -> Do (if bool then \m->{m|coloring = coloring} else identity))

menu : State -> Html Msg
menu state = Html.main_ [] [
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
    Html.br [] [],
    text "color by:",
    Html.br [] [],
    Html.label [] [
        Html.input [ type_ "radio", Attrs.name "color",
            Attrs.checked (case state.coloring of
                ByName _ -> True
                _ -> False),
            on_radio_button (ByName 3054)] [],
        text " name"
    ],
    (case state.coloring of
        ByName num -> Html.label [] [
            text " seed: ",
            Html.input [
                type_ "number",
                Attrs.value (String.fromInt num),
                Events.onInput ( \m -> Do (\s-> {s | coloring=String.toInt m |> Maybe.withDefault 3054 |> ByName}))] []]
        _ -> text ""),
    Html.br [] [],
    Html.label [] [
        Html.input [ type_ "radio", Attrs.name "color",
            Attrs.checked (state.coloring == ByDate),
            on_radio_button ByDate] [],
        text " date"
    ],
    (if state.coloring /= ByDate then text "" else
        [("prehistory", 0), ("2020", 1577836800), ("2021", 1609459200), ("2022", 1640995200), ("2023", 1672531200)]
        |> List.map (\(year, time)->
            text year |> singleton |> Html.td [
                Attrs.style "background-color" (
                    player_color ByDate {score=0,mode=0,owner="",date=Time.millisToPosix (1000 * time)})
            ]
        )
        |> Html.tr [] |> singleton |> Html.table [Attrs.style "display" "inline"]
    ),
    Html.br [] [],
    Html.label [] [
        Html.input [ type_ "radio", Attrs.name "color",
            Attrs.checked (state.coloring == ByScore),
            on_radio_button ByScore] [],
        text " score" ]]

fromMonth : Time.Month -> Int
fromMonth month = case month of
    Time.Jan -> 1  -- WHAT the FUCK elm/time
    Time.Feb -> 2  -- what on EARTH were you thinking
    Time.Mar -> 3  -- did you really expect no one to need this
    Time.Apr -> 4  -- it's fucking defined there it's just private
    Time.May -> 5  -- like ?????
    Time.Jun -> 6  -- > Represents a Month so that you can convert it to a String or Int however you please.
    Time.Jul -> 7  -- oh so how do you convert it into an Int?
    Time.Aug -> 8  -- NOPE
    Time.Sep -> 9  -- STUPID QUESTION
    Time.Oct -> 10 -- hate this stupid ass language
    Time.Nov -> 11 -- and there's no way to compress this down
    Time.Dec -> 12 -- if i were using elm/format this would take 3 times as many lines

fmt_date : Time.Posix -> String
fmt_date date =
    let 
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

player_color : Coloring -> Score -> String
player_color coloring score =
    let 
        {hue, lgt} = case coloring of
            ByName seed ->
                let hash = hashString seed score.owner in
                { hue = hash |> modBy 360
                , lgt = ((hash // 360) |> modBy 45) + 40}
            ByDate -> 
                { hue = (Time.posixToMillis score.date // 1000 - 1577836800) // 500000 |> max -20
                , lgt = 60 }
            ByScore ->
                { hue = 40
                , lgt = score.score + 3 |> toFloat
                    |> logBase (1002 ^ (1/45)) |> (+) 40 |> floor }
    in "hsl("++(String.fromInt hue)++",60%,"++(String.fromInt lgt)++"%"

make_cell : State -> Mode -> Html msg
make_cell state mode = 
    let cell attrs = Html.a [Attrs.href ("https://ubq323.website/ffbm#" ++ Mode.toString mode)] >> singleton >> Html.td attrs in
    case Dict.get mode state.scores of
        Just ({score, owner, date} as sco) -> 
            cell [
                Attrs.style "background-color" (player_color state.coloring sco),
                Attrs.title ( 
                    owner ++ " " ++ (String.fromInt score) ++ " in " ++ (Mode.toString mode) ++ " at " ++ (fmt_date date))
            ] [
                text (String.fromInt score),
                Html.small [] [text (" " ++ Mode.toString mode)],
                Html.br [] [],
                Html.small [] [text owner]
            ]
        Nothing -> [text (Mode.toString mode)] |> cell []
    

doif : Bool -> (a -> a) -> (a -> a)
doif bl fun = if bl then fun else identity

make_table : State -> Html Msg
make_table state =
    let 
        modes = state.modes |> List.filterMap (\m->
            if Mode.intersect state.context m /= 0 then Nothing else
                Just (Mode.merge state.context m))
        header class m = Html.th
            [Attrs.class class, Events.onClick(Do (\s -> {s|context=m}))]
            [text (Mode.toString m)]
    in modes
    |> List.indexedMap Tuple.pair
    |> List.map (\(x, m1) ->
        modes |> doif state.scol (\i->0::i)
        |> List.take (x + 1)
        |> List.map (\m2 -> make_cell state (Mode.merge m1 m2))
        |> doif state.scol     (\i->i++[ header "diag" m1])
        |> doif (not state.scol) ((::) ( header "left" m1 ))
        |> Html.tr [])
    |> Html.table []
