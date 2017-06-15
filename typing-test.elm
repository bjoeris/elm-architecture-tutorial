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
import Random

import Debug

import EditDistance exposing (ErrorOp, TokenErr(TokenIncorrect, TokenMissing))


{-| The path which challenge paragraphs are loaded from. -}
challengeUrl : String
challengeUrl = "input.txt"


timeLimit : Time
timeLimit = 5 * minute


{-| Entry point -}
main : Program Never Model Msg
main = program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }


-- MODEL

{-| Possible states of the application -}
type State
  = Init  -- ^ initial state, before typing begins
  | Running  -- ^ currently typing, time is running
  | Finished  -- ^ time has run out, or the user indicated they were finished

{-| All data of the running application -}
type alias Model =
  { input : Array String  -- ^ the typed input, parsed into tokens
  , challenges : Array (Array String)  -- ^ the challenge paragraphs, each parsed into tokens
  , challengeId : Int  -- ^ which of the challenge paragraphs to display
  , startTime : Time  -- ^ time which typing began
  , now : Time  -- ^ current time
  , numErrors : Int  -- ^ number of typing errors
  , numWords : Int  -- ^ 'word count' of correctly counted words (see EditDistance.countCorrectWords)
  , inputErrors : Array TokenErr  -- ^ the error statuses (correct, incorrect, or missing) of the typed input tokens
  , challengeErrors : Array TokenErr  -- ^ the error statuses of the challenge paragraph tokens
  , scoreMatrix : Array (Array (Int, EditDistance.ErrorOp))  -- ^ the score matrix (see EditDistance.scoreMatrix)
  , state : State  -- ^ state of the application (see State)
  }


{-| Get the current challenge paragraph from the model -}
challenge : Model -> Array String
challenge model =
  Array.get model.challengeId model.challenges |> withDefault Array.empty


-- UPDATE

{-| Possible messages that can be received by the application -}
type Msg
  = Challenges (Result Http.Error String)
  | ChallengeId Int
  | Input String
  | StartTime Time
  | Tick Time
  | Discard Int  -- ^ Hack. there is a bug with the first number from the random number generator


{-| Move the model to the Finished state -}
finish : Model -> Model
finish model =
  let errors = EditDistance.score model.input (challenge model)
  in { model | state = Finished
             , numWords = errors.numWords
             , numErrors = errors.numErrors
             , inputErrors = errors.leftErrors
             , challengeErrors = errors.rightErrors
             , scoreMatrix = errors.matrix }


{-| Update the model, given an input message. -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Challenges (Ok challenges) ->
      let parsedChallenges = parseParagraphs challenges
      in ( { model | challenges = parsedChallenges }
         , Random.generate ChallengeId (Random.int 0 (Debug.log "length" (Array.length parsedChallenges - 1))) )
    Challenges (Err _) ->
      (model, Cmd.none)
    ChallengeId challengeId ->
      ( { model | challengeId = Debug.log "challengeId" challengeId }, Cmd.none )
    Input input ->
      let input_ = parseParagraph input
          model_ = { model | input = input_ }
          finishIndicated = Array.get (Array.length input_ - 1) input_
                            |> withDefault ""
                            |> String.endsWith "\n\n"
          newModel = if finishIndicated
                     then finish model_
                     else model_
      in ( newModel
         , case newModel.state of
               Init -> Time.now |> Task.perform StartTime
               _    -> Cmd.none )
    StartTime t ->
      ( { model | now = t
                , startTime = t
                , state = Running }
      , Cmd.none )
    Tick t ->
      ( case model.state of
          Running ->
            let model_ = { model | now = t }
            in if t - model.startTime >= timeLimit
               then let errors = EditDistance.score model.input (challenge model)
                    in { model_ | state = Finished
                                , numWords = errors.numWords
                                , numErrors = errors.numErrors
                                , inputErrors = errors.leftErrors
                                , challengeErrors = errors.rightErrors
                                , scoreMatrix = errors.matrix
                                }
               else model_
          _ -> model
      , Cmd.none )
    Discard _ -> (model, Cmd.none)


{-| Parse a paragraph into an array of tokens.

A token is a maximal substring that is either

* one or more whitespace characters
* one or more alpha-numeric characters
* exactly one non-whitespace, non-alpha-numeric character
-}
parseParagraph : String -> Array String
parseParagraph string =
  string |> Regex.find Regex.All (regex "([\\w]+)|([\\s]+)|([^\\w\\s])")
         |> List.map (\match -> match.match)
         |> Array.fromList


{-| Convert all whitespace tokens into a single whitespace character, except
whitespace tokens following a full-stop which are converted to two whitespace
characters.
-}
normalizeSpaces : Array String -> Array String
normalizeSpaces paragraph =
  let fixSpace i token =
        let prevToken = paragraph |> Array.get i |> withDefault ""
        in if Regex.contains (regex "[\\s]") token
            then
                if prevToken == "."
                then "  "
                else " "
            else
                token
  in Array.indexedMap fixSpace paragraph


{-| Merge lines into paragraphs. At least one empty line marks the break between paragraphs. -}
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


{-| parse the paragraphs from the input file -}
parseParagraphs : String -> Array (Array String)
parseParagraphs raw =
    raw |> String.lines
        |> mergeLines
        |> List.map (parseParagraph >> normalizeSpaces)
        |> Array.fromList


-- VIEW

{-| Create the visual representation of the application -}
view : Model -> Html Msg
view model =
  let
    finished =
      case model.state of
        Finished -> True
        _ -> False
    timeRemaining =
      case model.state of
        Init -> timeLimit
        Running -> model.startTime + timeLimit - model.now
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
    numErrors k = span [style [("padding-right", "20pt")]]
                  [ text "Errors: "
                  , k |> toString |> text ]
    wpm model = span []
            [ text "WPM: "
            , round (toFloat model.numWords / (model.startTime - model.now |> Time.inMinutes)) |> toString |> text ]
    header =
      case model.state of
        Finished -> div [scoreStyle]
                    [ span [style [("padding-right", "20pt")]]
                          [ text "Errors: "
                          , model.numErrors |> toString |> text ]
                    , span []
                        [ text "WPM: "
                        , round (toFloat model.numWords /
                                     (model.now - model.startTime |>
                                          Time.inMinutes)) |>
                            toString |> text ] ]
        _ -> clock
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
    inputScore =
      div
      [ textStyle
      , style
        [ ("height", "24em")
        , ("display", if finished then "inline-block" else "none")] ]
      [ formatTokens model.input model.inputErrors ]
    paragraph =
      div
      [ textStyle
      , style [("height", "24em")] ]
      [ Array.get model.challengeId model.challenges
        |> withDefault Array.empty
      |> (\tokens -> formatTokens tokens model.challengeErrors) ]
  in
    div []
      [ header
      , input
      , inputScore
      , paragraph
      ]


{-| Set the background color of a token, based on its error status
* Correct: no background
* Incorrect: red background
* Missing: yellow background
-}
formatToken : String -> Maybe TokenErr -> Html Msg
formatToken s err =
  let background =
        case err of
          Just TokenIncorrect -> "red"
          Just TokenMissing -> "yellow"
          _ -> "none"
  in span [style [("background", background)]] [text s]


{-| Set the background color of a sequence of tokens -}
formatTokens : Array String -> Array TokenErr -> Html Msg
formatTokens tokens errors =
  let getFormattedToken i s = Array.get i errors
                            |> formatToken s
  in div [] (Array.indexedMap getFormattedToken tokens |> Array.toList)


-- SUBSCRIPTIONS


{-| The regular updates the application listens to -}
subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Running -> Time.every second Tick
    _ -> Sub.none


-- INIT

{-| Initial application data -}
init : (Model, Cmd Msg)
init =
  let
    model = { challenges = Array.empty
            , challengeId = 0
            , input = Array.empty
            , state = Init
            , now = 0
            , startTime = 0
            , numErrors = 0
            , numWords = 0
            , inputErrors = Array.empty
            , challengeErrors = Array.empty
            , scoreMatrix = Array.empty
            }
    cmd = Cmd.batch
          [ Random.generate Discard (Random.int Random.minInt Random.maxInt)
          , Http.send Challenges (Http.getString challengeUrl) ]
  in
    (model, cmd)


-- STYLES

{-| Mono-space fonts -}
monoFont : String
monoFont = "Consolas,Monaco,Lucida Console,Liberation Mono,DejaVu Sans Mono,Bitstream Vera Sans Mono,Courier New,monospace"


{-| Style for the input and challenge text -}
textStyle : Attribute msg
textStyle =
  style
    [ ("padding", "10px 10px")
    , ("font-family", monoFont)
    , ("font-size", "100%")
    , ("white-space", "pre-wrap")
    , ("width", "84ch")
    , ("margin-left", "50%")
    , ("transform", "translate(-50%, 0)")
    , ("overflow", "hidden")
    , ("background-color", "#eff0f1")
    ]


{-| Style for the countdown clock -}
clockStyle : Attribute msg
clockStyle =
  style
    [ ("padding", "10px 0px")
    , ("font-family", monoFont)
    , ("margin-left", "50%")
    , ("transform", "translate(-50%, 0)")
    , ("text-align", "right")
    ]

{-| Style for the Error and WPM display -}
scoreStyle : Attribute msg
scoreStyle =
  style
    [ ("padding", "10px 0px")
    , ("font-family", monoFont)
    , ("text-align", "center")
    ]
