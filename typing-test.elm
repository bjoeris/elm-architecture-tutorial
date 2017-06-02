-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/text_fields.html

import Html exposing (Html, Attribute, program, text, div, textarea)
import Platform.Cmd as Cmd
import Platform.Cmd exposing (Cmd)
import Html.Attributes exposing (attribute, style)
import Html.Events exposing (onInput)
import Array exposing (Array)
import Maybe exposing (withDefault)
import Http
import Regex exposing (regex)
import Time exposing (Time, second, minute)
import Task

challenge_url = "input.txt"
-- time_limit = 5 * minute
time_limit = 5 * second

main : Program Never Model Msg
main = program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }


-- MODEL

type State
  = Init
  | Running Time -- Running, with the time began
  | Finished

tickState : State -> Time -> State
tickState state t =
  case state of
    Running began ->
      if t - began >= time_limit
      then Finished
      else state
    _ -> state

type alias Model =
  { input : String
  , challenges : Array (Array String)
  , now : Time
  , state : State
  }


-- UPDATE

type Msg
  = Challenges(Result Http.Error String)
  | Input String
  | StartTime Time
  | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Challenges (Ok challenges) ->
      ( { model | challenges = parseParagraphs challenges }
      , Cmd.none )
    Challenges (Err _) ->
      (model, Cmd.none)
    Input input ->
      ( { model | input = input }
      , case model.state of
          Init -> Time.now |> Task.perform StartTime
          _ -> Cmd.none
      )
    StartTime t ->
      ( { model | now = t
                , state = Running t }
      , Cmd.none )
    Tick t ->
      ( { model | now = t
                , state = tickState model.state t }
      , Cmd.none )

parseParagraph : String -> Array String
parseParagraph string =
  let
    -- token is either:
    -- * one or more letters/numbers (e.g. "ab123c"),
    -- * one or more whitespace characters
    -- * exactly one other character (presumably punctuation)
    raw_tokens =
        string
          |> Regex.find Regex.All (regex "([\\w]+)|([\\s]+)|([^\\w\\s])")
          |> List.map (\match -> match.match)
    correct_whitespace prev_token token =
      if Regex.contains (regex "[\\s]") token
      then
          if prev_token == "."
          then "  "
          else " "
      else
          token
  in
    List.map2 correct_whitespace ("" :: raw_tokens) raw_tokens
      |> Array.fromList

mergeLines : List String -> List String
mergeLines lines =
    List.foldl (\line res ->
                  if String.isEmpty (String.trim line)
                  then "" :: res
                  else
                    case res of
                      r0 :: resTail ->
                        (r0 ++ " " ++ line) :: resTail
                      _ -> [line]
                ) [] lines
      |> List.map String.trim
      |> List.filter (String.isEmpty >> not)
      |> List.reverse

parseParagraphs : String -> Array (Array String)
parseParagraphs raw =
    raw |> String.lines
        |> mergeLines
        -- |> Regex.split Regex.All (regex "[\\r\\n][\\r\\n]+")
        |> List.map parseParagraph
        |> Array.fromList

score : Array String -> Array String -> (Int, (Array Bool, Array Bool))
score x y =
  let nx = Array.length x
      ny = Array.length y
      -- maxMaybe : Maybe Int -> Maybe Int -> Maybe Int
      maxMaybe a b =
        case (a,b) of
          (a, Nothing) -> a
          (Nothing, b) -> b
          (Just av, Just bv) -> max av bv
      -- grow : Int -> Int -> Array Int -> List Int -> Int
      grow i j rows rowCurr =
        case (Array.get i x, Array.get j y) of
          (Nothing, _) ->
            Array.fromList (List.reverse rows)
          (_, Nothing) ->
            grow (i+1) 0 (rowCurr |> List.reverse |> Array.fromList) []
          (Just xi, Just yj) ->
            let stepi = List.head rows
                      |> Maybe.andThen (Array.get i)
                      |> Maybe.map (\(k,_,_) -> (k+1, 1, 0))
                stepj = List.head rowCurr
                      |> Maybe.map (\(k,_,_) -> (k+1, 0, 1))
                mismatch = if xi == yj then 0 else 1
                stepij = List.head rows
                       |> Maybe.andThen (Array.get (i-1) rowPrev)
                       |> Maybe.map (\(k,_,_) -> (k+mismatch, 1, 1))
                next = stepi |> maxMaybe stepj |> maxMaybe stepij |> withDefault 0
            in grow i (j+1) rowPrev (next :: rowCurr)
      table = grow 0 0 [] []
      getErrors i j xErrors yErrors =
        table |> Array.get i
              |> Maybe.andThen (Array.get j)
              |> Maybe.map (\(_,di,dj) ->
                  let isMatch = Maybe.map2 (\xi yj -> xi == yj)
                                 (Array.get i xi) (Array.get j xj)
                                 |> withDefault False
                                 |> and (di == 1)
                                 |> and (dj == 1)
                      xErrors_ = if di == 1
                                 then isMatch :: xErrors
                                 else xErrors
                      yErrors_ = if dj == 1
                                 then isMatch :: yErrors
                                 else yErrors
                  in getErrors (i-di) (j-dj) xErrors_ yErrors_)
              |> withDefault (xErrors, yErrors)
      score = table
            |> Array.get (nx-1)
            |> Maybe.andThen (Array.get (ny-1))
            |> Maybe.map (\(k,_,_) -> k)
      errors = getErrors (nx-1) (ny-1) [] []
               |>
    in (score, Array.fromList xErrors, Array.fromList yErrors)


-- VIEW

view : Model -> Html Msg
view model =
  let
    finished =
      case model.state of
        Finished -> False
        _ -> True
    timeRemaining =
      case model.state of
        Init -> time_limit
        Running began -> began + time_limit - model.now
        Finished -> 0
    minutesRemaining =
      timeRemaining |> Time.inMinutes |> truncate
    secondsRemaining =
      (timeRemaining - ((toFloat minutesRemaining) * minute)) |> Time.inSeconds |> truncate
    secondsString =
      [secondsRemaining // 10, secondsRemaining % 10] |> List.map toString |> String.join ""
    timeString =
      [toString minutesRemaining, secondsString] |> String.join ":"
    clock =
      div [ clockStyle ] [ text timeString ]
    input =
      textarea
      [ placeholder "Type here"
      , onInput Input
      , textStyle
      , attribute "spellcheck" "false"
      , disabled (not finished)
      , rows 20
      , cols 83] []
    paragraph =
      textarea
      [ textStyle
      , attribute "spellcheck" "false"
      , disabled True
      , rows 20
      , cols 83] [ text (challenge model)]
    -- paragraph =
    --   div [ textStyle, style [ ("height", "20em")] ] [ text (challenge model) ]
  in
    div []
      [ clock
      , input
      , paragraph
      ]

challenge : Model -> String
challenge model =
     Array.get 1 model.challenges  |> withDefault Array.empty |> Array.toList |> String.join ""


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick
  -- Sub.none


-- INIT

init : (Model, Cmd Msg)
init =
  let
    model = { challenges = Array.empty, input = "", state = Init, now = 0 }
    cmd = Http.send Challenges (Http.getString challenge_url)
  in
    (model, cmd)

monoFont = "Consolas,Monaco,Lucida Console,Liberation Mono,DejaVu Sans Mono,Bitstream Vera Sans Mono,Courier New,monospace"

textStyle =
  style
    [ ("padding", "10px 10px")
    , ("font-family", monoFont)
    , ("white-space", "pre-wrap")
    , ("width", "83ch")
    , ("margin-left", "50%")
    , ("transform", "translate(-50%, 0)")
    , ("overflow", "hidden")
    , ("background-color", "#eff0f1")
    ]

clockStyle = 
  style
    [ ("padding", "10px 0px")
    , ("font-family", monoFont)
    , ("margin-left", "50%")
    , ("transform", "translate(-50%, 0)")
    , ("text-align", "right")
    ]
