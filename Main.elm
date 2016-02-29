module Main (..) where

import Html
import Html.Attributes
import Html.Events
import Http.Extra
import Json.Decode as Json exposing ((:=))
import Task
import Effects
import StartApp


type alias Model =
  { query : String
  , matches : List Person
  , sleepCount : Int
  , httpError : Maybe (Http.Extra.Error String)
  }


type alias Person =
  { firstName : String
  , lastName : String
  , uid : List String
  }


init : ( Model, Effects.Effects Action )
init =
  ( Model "" [] 0 Nothing
  , Effects.none
  )


type Action
  = UpdateQuery String
  | UpdatePeople (List Person)
  | HttpError (Http.Extra.Error String)
  | Timeout Int


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    UpdateQuery query ->
      let
        newCount =
          model.sleepCount + 1
      in
        ( { model | query = query, sleepCount = newCount }
        , Task.sleep 250 |> Effects.task |> Effects.map (always (Timeout newCount))
        )

    UpdatePeople persons ->
      ( { model | matches = persons, httpError = Nothing }, Effects.none )

    HttpError error ->
      ( { model | httpError = Just error }, Effects.none )

    Timeout count ->
      if count == model.sleepCount then
        ( { model | sleepCount = 0 }
        , getPeople model.query |> Effects.task
        )
      else
        ( model, Effects.none )


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div
    []
    [ Html.input
        [ Html.Events.on "input" Html.Events.targetValue (Signal.message address << UpdateQuery) ]
        []
      --    , Html.text <| toString model.timer
    , viewError model
    , Html.ol
        []
        (List.map viewPerson model.matches)
    ]


viewError : Model -> Html.Html
viewError model =
  case model.httpError of
    Just error ->
      Html.div [ Html.Attributes.class "error-message" ] [ Html.text <| toString error ]

    Nothing ->
      Html.span [] []


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
    , inputs = [ actionMailbox.signal ]
    }


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


main : Signal Html.Html
main =
  app.html


actionMailbox : Signal.Mailbox Action
actionMailbox =
  Signal.mailbox (Timeout 0)


getPeople : String -> Task.Task Effects.Never Action
getPeople query =
  let
    url =
      "http://app.devnet.imsa.edu:8082/search/" ++ query

    getTask =
      Http.Extra.get url
        |> Http.Extra.send (Http.Extra.jsonReader decodePeople) Http.Extra.stringReader

    handleError error =
      Task.succeed <| HttpError error
  in
    if query == "" then
      Task.succeed (UpdatePeople [])
    else
      Task.onError (Task.map (.data >> UpdatePeople) getTask) handleError


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
