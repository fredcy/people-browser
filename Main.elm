module Main (..) where

import Html
import Html.Attributes exposing (class)
import Html.Events
import Http.Extra
import Json.Decode as Json exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Task
import Effects
import Regex
import StartApp
import String


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
  , orgStatus : String
  , dn : String
  , ous : List String
  }


init : ( Model, Effects.Effects Action )
init =
  ( Model "" [] 0 Nothing
  , Effects.task (Task.succeed (UpdateQuery initQuery))
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
        [ Html.Events.on "input" Html.Events.targetValue (Signal.message address << UpdateQuery)
        , Html.Attributes.value model.query
        ]
        []
      --    , Html.text <| toString model.timer
    , viewError model
    , viewPeople model.matches
    ]


viewError : Model -> Html.Html
viewError model =
  case model.httpError of
    Just error ->
      Html.div [ Html.Attributes.class "error-message" ] [ Html.text <| toString error ]

    Nothing ->
      Html.span [] []


viewPeople : List Person -> Html.Html
viewPeople persons =
  Html.table
    [ class "persons" ]
    (personHeaderRow
      :: (List.map viewPersonRow (List.sortBy .lastName persons))
    )


personHeaderRow : Html.Html
personHeaderRow =
  let
    th s =
      Html.th [] [ Html.text s ]
  in
    Html.tr
      []
      [ th "first"
      , th "last"
      , th "uids"
      , th "orgStatus"
      , th "ou"
      ]


viewPersonRow : Person -> Html.Html
viewPersonRow person =
  Html.tr
    []
    [ Html.td [ class "firstname" ] [ Html.text person.firstName ]
    , Html.td [ class "lastname" ] [ Html.text person.lastName ]
    , Html.td [] (viewUids person.uid)
    , Html.td [] [ Html.text person.orgStatus ]
    , Html.td [] [ Html.text <| String.join ", " (List.sort person.ous) ]
    ]


viewUids : List String -> List Html.Html
viewUids uidList =
  let
    separator =
      Html.span [ Html.Attributes.property "innerHTML" (Json.Encode.string "&nbsp;&nbsp;") ] []

    uidSpan uid =
      Html.span
        [ Html.Attributes.classList
            [ ( "uid", True )
            , ( "alum", isAlumUid uid )
            ]
        ]
        [ Html.text uid ]

    uidSpans =
      List.map uidSpan (List.sortWith compareUid uidList)
  in
    List.intersperse separator uidSpans


{-| Compare such that the alumni Uid values sort after other Uid values.
-}
compareUid : String -> String -> Order
compareUid a b =
  case ( isAlumUid a, isAlumUid b ) of
    ( False, False ) ->
      compare a b

    ( True, True ) ->
      compare a b

    ( False, True ) ->
      LT

    ( True, False ) ->
      GT


isAlumUid : String -> Bool
isAlumUid string =
  Regex.contains (Regex.regex "[A-Z]") string


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


port initQuery : String
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
  Json.succeed Person
    |: ("FirstName" := Json.string)
    |: ("LastName" := Json.string)
    |: ("Uid" := (Json.list Json.string))
    |: ("OrgStatus" := Json.string)
    |: ("Dn" := Json.string)
    |: ("Ous" := (Json.list Json.string))
