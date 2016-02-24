module Main (..) where

import Html


-- import Html.Attributes

import Html.Events
import Http
import Http.Extra
import Json.Decode as Json exposing ((:=))
import Task
import Effects
import StartApp
import Time exposing (Time, second)


type alias Model =
  { query : String
  , matches : List Person
  , debounce : Maybe DebounceState
  }


type alias Person =
  { firstName : String
  , lastName : String
  , uid : List String
  }


init : ( Model, Effects.Effects Action )
init =
  ( Model "" [] Nothing, Effects.none )


type alias DebounceState =
  { prevClockTime : Time, elapsedTime : Time }


type Action
  = NoOp
  | UpdateQuery String
  | UpdatePeople (List Person)
  | HttpError (Http.Extra.Error Json.Value)
  | HttpError' Http.Error
  | Tick String Time


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action |> Debug.log "action" of
    NoOp ->
      ( model, Effects.none )

    UpdateQuery query ->
      case model.debounce of
        Nothing ->
          ( { model | query = query }, Effects.tick <| Tick query )
        Just { elapsedTime, prevClockTime } ->
          let
            debounce = Just { elapsedTime = 0, prevClockTime = prevClockTime }
          in
            ( { model | query = query, debounce = debounce }, Effects.none )

    UpdatePeople persons ->
      ( { model | matches = persons }, Effects.none )

    HttpError error ->
      ( model, Effects.none )

    HttpError' error ->
      ( model, Effects.none )

    Tick tag clockTime ->
      let
        newElapsedTime =
          case model.debounce of
            Nothing ->
              0

            Just { elapsedTime, prevClockTime } ->
              elapsedTime + (clockTime - prevClockTime)
      in
        if newElapsedTime > 500 then
          ( { model | debounce = Nothing }
          , getPeople' model.query |> Effects.task |> Debug.log "get people"
          )
        else
          ( { model | debounce = Just { elapsedTime = newElapsedTime, prevClockTime = clockTime } }
          , Effects.tick <| Tick tag
          )


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div
    []
    [ Html.input
        [ Html.Events.on "input" Html.Events.targetValue (Signal.message address << UpdateQuery) ]
        []
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
        |> Http.Extra.send decodePeople Json.value
  in
    Task.onError (Task.map (UpdatePeople << .data) getTask) handleError


handleError : Http.Extra.Error Json.Value -> Task.Task Effects.Never Action
handleError error =
  Task.succeed <| HttpError error


getPeople' : String -> Task.Task Effects.Never Action
getPeople' query =
  let
    url =
      "http://app.devnet.imsa.edu:8082/search/" ++ query

    getTask =
      Http.get decodePeople url
        |> Task.map UpdatePeople

    errorHandler error =
      Task.succeed <| HttpError' error
  in
    Task.onError getTask errorHandler


decodePeople : Json.Decoder (List Person)
decodePeople =
  Json.list decodePerson


decodePerson : Json.Decoder Person
decodePerson =
  Json.object3
    (\f l u -> Person f l u)
    ("FirstName" := Json.string)
    ("LastName" := Json.string)
    ("Uid" := (Json.list Json.string))
