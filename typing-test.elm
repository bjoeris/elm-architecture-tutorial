-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/text_fields.html

import Html exposing (Html, Attribute, program, text, div, textarea, span)
import Platform.Cmd as Cmd
import Platform.Cmd exposing (Cmd)
import Html.Attributes exposing (attribute, style, placeholder, disabled, rows, cols)
import Html.Events exposing (onInput)
import Array exposing (Array)
import Maybe exposing (withDefault)
import Http
import Regex exposing (regex)
import Time exposing (Time, second, minute)
import Task

import EditDistance

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
  { input : Array String
  , challenges : Array (Array String)
  , challengeId : Int
  , now : Time
  , score : Maybe Int
  , inputErrors : Array Bool
  , challengeErrors : Array Bool
  , errorMatrix : Array (Array (Int, EditDistance.ErrorOp))
  , state : State
  }

challenge : Model -> Array String
challenge model =
  Array.get model.challengeId model.challenges |> withDefault Array.empty


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
      let input_ = parseParagraph input
          errors = challenge model
                 |> EditDistance.score input_
      in ( { model | input = parseParagraph input
                   , score = Just errors.score
                   , inputErrors = errors.leftErrors
                   , challengeErrors = errors.rightErrors
                   , errorMatrix = errors.matrix
           }
         , case model.state of
               -- Init -> Time.now |> Task.perform StartTime
               _    -> Cmd.none )
    StartTime t ->
      ( { model | now = t
                , state = Running t }
      , Cmd.none )
    Tick t ->
      ( case model.state of
          Running began ->
            let model_ = { model | now = t }
            in if t - began >= time_limit
               then let errors = EditDistance.score model.input (challenge model)
                    in { model_ | state = Finished
                                , score = Just errors.score
                                , inputErrors = errors.leftErrors
                                , challengeErrors = errors.rightErrors
                                , errorMatrix = errors.matrix
                                }
               else model_
          _ -> model
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


-- VIEW

view : Model -> Html Msg
view model =
  let
    finished =
      case model.state of
        Finished -> True
        _ -> False
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
      , style [ ("display", if finished then "none" else "inline-block")]
      , attribute "spellcheck" "false"
      , disabled finished
      , rows 20
      , cols 83] []
    input_score =
      div
      [ textStyle
      , style
        [ ("height", "24em")
        , ("display", if finished then "inline-block" else "none")] ]
      [ format_tokens model.input model.inputErrors ]
    paragraph =
      div
      [ textStyle
      , style [("height", "24em")] ]
      [ Array.get model.challengeId model.challenges
        |> withDefault Array.empty
      |> (\tokens -> format_tokens tokens model.challengeErrors) ]
    -- paragraph =
    --   textarea
    --   [ textStyle
    --   , attribute "spellcheck" "false"
    --   , disabled True
    --   , rows 20
    --   , cols 83] [ text (challenge model)]
    -- paragraph =
    --   div [ textStyle, style [ ("height", "20em")] ] [ text (challenge model) ]
  in
    div []
      [ clock
      , input
      , input_score
      , paragraph
      ]

format_token : String -> Bool -> Html Msg
format_token s err = span [style [("background", if err then "red" else "none")]] [text s]

format_tokens : Array String -> Array Bool -> Html Msg
format_tokens tokens errors =
  let getFormattedToken i s = Array.get i errors
                            |> withDefault True
                            |> format_token s
  in div [] (Array.indexedMap getFormattedToken tokens |> Array.toList)

-- challenge : Model -> String
-- challenge model =
--      Array.get model.challengeId model.challenges  |> withDefault Array.empty |> Array.toList |> String.join ""


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Running _ -> Time.every second Tick
    _ -> Sub.none
  -- Sub.none


-- INIT

init : (Model, Cmd Msg)
init =
  let
    model = { challenges = Array.empty
            , challengeId = 1
            , input = Array.empty
            , state = Init
            , now = 0
            , score = Nothing
            , inputErrors = Array.empty
            , challengeErrors = Array.empty
            , errorMatrix = Array.empty
            }
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
