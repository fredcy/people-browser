module Main (..) where

import Html
import Html.Events
import Http
import Http.Extra
import Json.Decode as Json exposing ((:=))
import Task
import Effects
import StartApp
import Timer


type alias Model =
  { query : String
  , matches : List Person
  , timer : Timer.Model
  }


type alias Person =
  { firstName : String
  , lastName : String
  , uid : List String
  }


init : ( Model, Effects.Effects Action )
init =
  ( Model "" [] Timer.init
  , Effects.none
  )


type Action
  = UpdateQuery String
  | UpdatePeople (List Person)
  | HttpError (Http.Extra.Error Json.Value)
  | HttpError' Http.Error
  | TimerAction Timer.Action


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    UpdateQuery query ->
      ( { model | query = query }
      , Timer.start 250 |> Effects.map TimerAction
      )

    UpdatePeople persons ->
      ( { model | matches = persons }, Effects.none )

    HttpError error ->
      ( model, Effects.none )

    HttpError' error ->
      ( model, Effects.none )

    TimerAction taction ->
      let
        ( newTimer, timerEffect ) =
          Timer.update taction model.timer

        effect =
          if taction == Timer.Expire then
            getPeople' model.query |> Effects.task
          else
            timerEffect |> Effects.map TimerAction
      in
        ( { model | timer = newTimer }, effect )


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div
    []
    [ Html.input
        [ Html.Events.on "input" Html.Events.targetValue (Signal.message address << UpdateQuery) ]
        []
      --    , Html.text <| toString model.timer
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

    handleError error =
      Task.succeed <| HttpError error
  in
    Task.onError (Task.map (UpdatePeople << .data) getTask) handleError


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
