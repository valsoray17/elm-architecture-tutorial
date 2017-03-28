import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import List exposing (..)
import String exposing (..)
import Char exposing (..)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


-- MODEL

type alias Model =
  { name : String
  , age : String
  , password : String
  , passwordAgain : String
  , validationErrors : List String
  }


model : Model
model =
  Model "" "" "" "" []


-- UPDATE

type Msg
    = Name String
    | Age String
    | Password String
    | PasswordAgain String
    | Validate


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Age age ->
      { model | age = age }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Validate ->
      { model | validationErrors = validateModel model}


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" Name
    , viewInput "text" "Age" Age
    , viewInput "password" "Password" Password
    , viewInput "password" "Re-enter Password" PasswordAgain
    , button [ onClick Validate ] [ text "Validate" ]
    , viewValidation model.validationErrors
    ]


viewInput : String -> String -> (String -> Msg) -> Html Msg
viewInput withType withPlaceholder withOnInput =
  div []
    [ span []
      [ input [ type_ withType
              , placeholder withPlaceholder
              , onInput withOnInput
              ][]
      ]
    ]


viewValidation : (List String) -> Html msg
viewValidation validationErrors =
  let
    ( color, errorLis ) =
      if List.isEmpty validationErrors then
        ( "green", [] )
      else
        ( "red", List.map (\e -> li [] [ text e ]) validationErrors )
  in
    div [ style [("color", color)] ] [ ul [] errorLis ]


-- Helpers
type alias Validator =
    { f : Model -> Bool, error : String }


validateModel : Model -> List String
validateModel model =
  let
        nameIsNotEmpty =
            Validator (\m -> not <| String.isEmpty m.name)
              "Name should not be empty"

        isPositiveInt =
          (\s -> (Result.withDefault 0 <| String.toInt s) > 0)

        ageIsInt =
            Validator (\m -> isPositiveInt m.age)
              "Age must be a positive integer!"

        passwordsMatch =
            Validator (\m -> m.password == m.passwordAgain)
              "Passwords do not match!"

        passwordLength =
            Validator (\m -> String.length m.password >= 8)
              "Password must be at least 8 characters!"

        passwordHasCapital =
            Validator (\m -> String.any isUpper m.password)
              "Password must have a capital letter."

        passwordHasLowercase =
            Validator (\m -> String.any isLower m.password)
              "Password must have a lowercase letter."

        passwordHasNumber =
            Validator (\m -> String.any isDigit m.password)
              "Password must have a number."

        validators =
            [ nameIsNotEmpty, ageIsInt, passwordsMatch, passwordLength
            , passwordHasCapital, passwordHasLowercase, passwordHasNumber ]
    in
      validators
        |> List.filterMap (checkValidator model)


checkValidator : Model -> Validator -> Maybe String
checkValidator model validator =
  if validator.f model then
    Nothing
  else
    Just validator.error
