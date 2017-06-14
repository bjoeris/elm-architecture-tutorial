module EditDistance exposing (score, ErrorOp)

import Array exposing (Array)
import Maybe exposing (withDefault)

type ErrorOp
  = Match
  | Substitution
  | Left
  | Right

score_matrix : Array String -> Array String -> Array (Array (Int, ErrorOp))
score_matrix x y =
  let nx = Array.length x
      ny = Array.length y
      minMaybe a b =
        case (a,b) of
          (Nothing, _) -> b
          (_, Nothing) -> a
          (Just (a0,a1), Just (b0,b1)) ->
            if a0 < b0
            then Just (a0,a1)
            else Just (b0,b1)
      grow i j rows rowCurr =
        case (Array.get i x, Array.get j y) of
          (Nothing, _) ->
            Array.fromList (List.reverse rows)
          (_, Nothing) ->
            grow (i+1) 0 ((rowCurr |> List.reverse |> Array.fromList) :: rows) []
          (Just xi, Just yj) ->
            let stepi = List.head rows
                      |> Maybe.andThen (Array.get i)
                      |> Maybe.map (\(k,_) -> (k+1, Left))
                stepj = List.head rowCurr
                      |> Maybe.map (\(k,_) -> (k+1, Right))
                mismatch_cost = if xi == yj then 0 else 1
                mismatch_op = if xi == yj then Match else Substitution
                stepij = List.head rows
                       |> Maybe.andThen (Array.get (i-1))
                       |> Maybe.map (\(k,_) -> k)
                       |> withDefault 0
                       |> (\k -> (k+mismatch_cost, mismatch_op))
                next = stepi |> minMaybe stepj |> minMaybe (Just stepij) |> withDefault stepij
            in grow i (j+1) rows (next :: rowCurr)
  in grow 0 0 [] []

score_sequence : Array (Array (Int, ErrorOp)) -> (Array Bool, Array Bool)
score_sequence matrix =
  let nx = Array.length matrix
      ny = matrix |> Array.get 0 |> Maybe.map Array.length |> withDefault 0
      getErrors i j xErrors yErrors =
        let getNextError op =
              case op of
                Match ->
                  getErrors (i-1) (j-1) (False :: xErrors) (False :: yErrors)
                Substitution ->
                  getErrors (i-1) (j-1) (True :: xErrors) (True :: yErrors)
                Left ->
                  getErrors (i-1) j (True :: xErrors) yErrors
                Right ->
                  getErrors i (j-1) xErrors (True :: yErrors)
            finalizeErrors n errorList =
              let errorArray = Array.fromList errorList
              in Array.append
                  (Array.repeat (n - Array.length errorArray) True)
                  errorArray
        in matrix |> Array.get i
                  |> Maybe.andThen (Array.get j)
                  |> Maybe.map (\(_, op) -> getNextError op)
                  |> withDefault (finalizeErrors nx xErrors,
                                  finalizeErrors ny yErrors)
  in getErrors (nx-1) (ny-1) [] []


reverse : Array a -> Array a
reverse arr = arr |> Array.toList |> List.reverse |> Array.fromList

score : Array String -> Array String -> { score : Int
                                        , leftErrors : Array Bool
                                        , rightErrors : Array Bool
                                        , matrix : Array (Array (Int, ErrorOp))}
score x y =
  let nx = Array.length x
      ny = Array.length y
      matrix = score_matrix (reverse x) (reverse y)
      score = matrix
            |> Array.get (nx-1)
            |> Maybe.andThen (Array.get (ny-1))
            |> Maybe.map (\(k,_) -> k)
            |> withDefault 0
      errors = score_sequence matrix
               |> \(xErrors, yErrors) ->
                   { score=score
                   , leftErrors=(reverse xErrors)
                   , rightErrors=(reverse yErrors)
                   , matrix=matrix }
  in errors
