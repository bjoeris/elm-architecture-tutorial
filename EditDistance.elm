module EditDistance exposing (score, ErrorOp, TokenErr(..))

import Array exposing (Array)
import Maybe exposing (withDefault)


{-| A type representing the relationship between two tokens, used in the edit distance matrix.
-}
type ErrorOp
  = Match  -- ^ the two tokens are equal
  | Substitution  -- ^ one token is a typo of the other
  | LeftMissing  -- ^ the left token is missing from the right sequence
  | RightMissing  -- ^ the right token is missing from the left sequence


{-| The status of a single token in one of two tokens being compared
-}
type TokenErr
  = TokenCorrect  -- ^ the token is matched correctly with a token in the other sequence
  | TokenIncorrect  -- ^ the token is a typo of a token in the other sequence
  | TokenMissing  -- ^ the token is missing from the other sequence


isTokenCorrect : TokenErr -> Bool
isTokenCorrect err =
  case err of
    TokenCorrect -> True
    _ -> False


isTokenIncorrect : TokenErr -> Bool
isTokenIncorrect err =
  case err of
    TokenIncorrect -> True
    _ -> False


isTokenMissing : TokenErr -> Bool
isTokenMissing err =
  case err of
    TokenMissing -> True
    _ -> False


{-| Create the score matrix.

Rows correspond to tokens of the left sequence, columns correspond to tokens of the right string.

Entry (i,j) is a pair (dist, op), where dist is the edit distance between the suffixes of the
left and right strings beginning at index i and j, respectively, and op indicates the relationship
between the initial tokens in these two suffixes which minimize the edit distance.
-}
scoreMatrix : Array String -> Array String -> Array (Array (Int, ErrorOp))
scoreMatrix x y =
  let nx = Array.length x
      ny = Array.length y
      minFirst a b =
        case (a,b) of
          ((aScore,aOp), (bScore,bOp)) ->
              if bScore < aScore
              then b
              else a
      grow i j rows rowCurr =
        case (Array.get i x, Array.get j y) of
          (Nothing, _) ->
            Array.fromList rows
          (_, Nothing) ->
            grow (i-1) (ny-1) ((rowCurr |> Array.fromList) :: rows) []
          (Just xi, Just yj) ->
            let getPrevRow j_ = List.head rows
                    |> Maybe.andThen (Array.get j_)
                    |> Maybe.map (\(k,_) -> k)
                    |> withDefault (nx - i - 1 + ny - j_)
                stepi = (1 + getPrevRow j, LeftMissing)
                stepj = List.head rowCurr
                      |> Maybe.map (\(k,_) -> k)
                      |> withDefault (nx - i)
                      |> \k -> (k+1, RightMissing)
                mismatchCost = if xi == yj then 0 else 1
                mismatchOp = if xi == yj then Match else Substitution
                stepij = (mismatchCost + getPrevRow (j+1), mismatchOp)
                next = stepi |> minFirst stepj |> minFirst stepij
            in grow i (j-1) rows (next :: rowCurr)
  in grow (nx-1) (ny-1) [] []


{-| Convert an edit distance matrix into a pair of sequences, consisting of the statuses of
the tokens in the left and right sequences.
-}
scoreSequence : Array (Array (Int, ErrorOp)) -> (Array TokenErr, Array TokenErr)
scoreSequence matrix =
  let nx = Array.length matrix
      ny = matrix |> Array.get 0 |> Maybe.map Array.length |> withDefault 0
      getErrors i j xErrors yErrors =
        let getNextError op =
              case op of
                Match ->
                  getErrors (i+1) (j+1) (TokenCorrect :: xErrors) (TokenCorrect :: yErrors)
                Substitution ->
                  getErrors (i+1) (j+1) (TokenIncorrect :: xErrors) (TokenIncorrect :: yErrors)
                LeftMissing ->
                  getErrors (i+1) j (TokenMissing :: xErrors) yErrors
                RightMissing ->
                  getErrors i (j+1) xErrors (TokenMissing :: yErrors)
            finalizeErrors n errorList =
              let errorArray = Array.fromList (List.reverse errorList)
              in Array.append
                  errorArray
                  (Array.repeat (n - Array.length errorArray) TokenMissing)
        in matrix |> Array.get i
                  |> Maybe.andThen (Array.get j)
                  |> Maybe.map (\(_, op) -> getNextError op)
                  |> withDefault (finalizeErrors nx xErrors,
                                  finalizeErrors ny yErrors)
  in getErrors 0 0 [] []


{-| Count the number of entries in an array satisfying a predicate.
-}
count : (a -> Bool) -> List a -> Int
count p = List.foldl (\x n -> if p x then (n+1) else n) 0

{-| Compute the 'word count', defined as the number of non-whitespace characters
in correct words, divided by 5.
-}
countCorrectWords : Array String -> Array TokenErr -> Int
countCorrectWords tokens errors =
  let trimmedToken i = tokens |> Array.get i |> withDefault "" |> String.trim
  in errors |> Array.indexedMap (\i err -> if isTokenCorrect err
                                            then String.length (trimmedToken i)
                                            else 0)
            |> Array.foldl (+) 0
            |> (\numChars -> numChars // 5)


dropWhile : (a -> Bool) -> List a -> List a
dropWhile pred =
  let loop list =
        case list of
          [] -> []
          x0 :: xs -> if pred x0
                      then loop xs
                      else list
  in loop

countErrors : Array TokenErr -> Int
countErrors errors =
  errors |> Array.toList
         |> List.reverse
         |> dropWhile isTokenMissing
         |> count (\t -> not (isTokenCorrect t))


{-| Compute the scoring information. -}
score : Array String -> Array String -> { numErrors : Int
                                        , numWords : Int
                                        , leftErrors : Array TokenErr
                                        , rightErrors : Array TokenErr
                                        , matrix : Array (Array (Int, ErrorOp))}
score x y =
  let nx = Array.length x
      ny = Array.length y
      matrix = scoreMatrix x y
      errors = scoreSequence matrix
               |> \(xErrors, yErrors) ->
                   { numErrors=countErrors yErrors
                   , numWords=countCorrectWords y yErrors
                   , leftErrors=xErrors
                   , rightErrors=yErrors
                   , matrix=matrix }
  in errors
