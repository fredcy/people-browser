module Main (..) where

import Html


-- import Html.Attributes

import Html.Events
import Http.Extra
import Json.Decode exposing ((:=))
import Task
import Effects
import StartApp


type alias Model =
  { query : String
  , matches : List Person
  }


type alias Person =
  { firstName : String
  , lastName : String
  , uid : List String
  }


init : ( Model, Effects.Effects Action )
init =
  ( Model "" [], Effects.none )


type Action
  = NoOp
  | UpdateQuery String
  | UpdatePeople (List Person)
  | HttpError (Http.Extra.Error String)


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action |> Debug.log "action" of
    NoOp ->
      ( model, Effects.none )

    UpdateQuery query ->
      ( { model | query = query }, Effects.task <| getPeople query )

    UpdatePeople persons ->
      ( { model | matches = persons }, Effects.none )

    HttpError error ->
      ( model, Effects.none )


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div
    []
    [ Html.input
        [ Html.Events.on "input" Html.Events.targetValue (Signal.message address << UpdateQuery) ]
        []
    , Html.div
        []
        [ Html.h2 [] [ Html.text "Query" ]
        , Html.text model.query
        ]
    , Html.ol
        []
        (List.map viewPerson model.matches)
    ]


viewPerson : Person -> Html.Html
viewPerson person =
  Html.li
    []
    [ Html.text <| person.lastName ++ ", " ++ person.firstName ++ " (" ++ toString person.uid ++ ")" ]


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


main : Signal Html.Html
main =
  app.html


getPeople : String -> Task.Task Effects.Never Action
getPeople query =
  let
    url =
      "http://app.devnet.imsa.edu:8082/search/" ++ query

    getTask =
      Http.Extra.get url
        |> Http.Extra.send decodePeople decodeError
  in
    Task.onError (Task.map (UpdatePeople << .data) getTask) handleError


handleError : Http.Extra.Error String -> Task.Task Effects.Never Action
handleError error =
  Task.succeed <| HttpError error


decodeError : Json.Decode.Decoder String
decodeError =
  Json.Decode.string

decodePeople : Json.Decode.Decoder (List Person)
decodePeople =
  Json.Decode.list decodePerson


decodePerson : Json.Decode.Decoder Person
decodePerson =
  Json.Decode.object3
    (\f l u -> Person f l u)
    ("FirstName" := Json.Decode.string)
    ("LastName" := Json.Decode.string)
    ("Uid" := (Json.Decode.list Json.Decode.string))
